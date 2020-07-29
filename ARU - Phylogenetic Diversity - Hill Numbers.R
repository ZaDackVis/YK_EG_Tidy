################################################################
########PHYLOGENETIC AND FUNCTIONAL DIV USING HILL NUMBERS######
################################################################

library(hillR)
library(tidyverse)
library(ape)
library(phytools)
library(ggplot2)
library(picante)
library(mnormt)
library(FD)

setwd("C:/Users/DackZ/Documents/R/Yukon_Birds/0_Data")

#Read in the WF Data and make WF by Zone and by Site_Zone

WF_ARU_PC <- read.csv("WF_ARU_PC.csv")


WF_All_Zones <- WF_ARU_PC %>%
  group_by(WF_ARU_PC$zone) %>%
  summarise_at(c(2:57), sum, na.rm = FALSE)



#The AOU Taxonomy and the Hackett backbone are very different. 
#Correct for species scientific names... 
#replace Alpha codes with Hackett Sci names

Taxa <- read.csv("Species_Taxonomy_Reference.csv")

WF_All_Zones <- column_to_rownames(WF_All_Zones, "WF_ARU_PC$zone")

WF_All_Zones <- t(WF_All_Zones)
WF_All_Zones <- as.data.frame(WF_All_Zones)
WF_All_Zones <- rownames_to_column(WF_All_Zones,"alpha_codes")

WF_All_Zones <- left_join(WF_All_Zones, Taxa, "alpha_codes")

WF_All_Zones <- column_to_rownames(WF_All_Zones, var = "Hackett_Taxa")

WF_All_Zones$alpha_codes <- NULL
WF_All_Zones$species_AOU_name <- NULL

WF_All_Zones <- t(WF_All_Zones)
WF_All_Zones <- as.data.frame(WF_All_Zones)

#read in your tree 


treefile <- read.nexus("Phylo_tree/output.nex")


#Create a consensus tree. 
#Note: p=0.5 specifies that the tree must be "majority rules consensus (MRC)".


consensus <- consensus.edges(treefile, consensus.tree=consensus(treefile,p=0.5))

plotTree(consensus, fsize=0.6)



#######HILL NUMBERS AND PHYLOGENETIC ANALYSIS############


#phylogenetic distance of all site_zones using picinate

Faith_Index_All_Zones <- pd(WF_All_Zones, consensus, include.root	= TRUE)

Faith_Index_All_Zones


#HIll numbers using hillR for all zones

Zonal_Phylo_Div_Faith <- hill_phylo(WF_All_Zones, consensus, q = 0)
Zonal_Phylo_Div_Entropy <- hill_phylo(WF_All_Zones, consensus, q = 1)
Zonal_Phylo_Div_Rao <- hill_phylo(WF_All_Zones, consensus, q = 2)

Zonal_Phylo_Div_Faith           
Zonal_Phylo_Div_Entropy
Zonal_Phylo_Div_Rao


#############3#tree with elevation for continuous data...

library(ggtree)


YK_ARU_PC <- read.csv("YK_ARU_PC_Tidy.csv")

YK_Env <- read.csv("C:/Users/DackZ/Documents/R/Yukon_Birds/0_data/YK_Env.csv")
YK_Env$location <- toupper(YK_Env$location)

elev <- YK_Env

elev <- elev[c("location","elev")]

YK_ARU_PC <- left_join(YK_ARU_PC, elev, "location")

Mean_Species_Elev <- YK_ARU_PC 
Mean_Species_Elev <- Mean_Species_Elev[c("scientific_name","elev")]


Mean_Species_Elev <- Mean_Species_Elev %>%
  group_by(scientific_name) %>%
  summarize_at(c(1), mean)

Mean_Species_Elev <- column_to_rownames(Mean_Species_Elev, var = "scientific_name")


dotTree(x = Mean_Species_Elev, 
        tree = consensus,
        legend = TRUE,
        standardize = FALSE)
    
plotTree.barplot(consensus,Mean_Species_Elev)

ggtree(consensus, aes(color=tree$elev)) +
  geom_rootedge(rootedge = 1) +
  geom_tiplab(consensus$tip.label) + 
  scale_color_continuous(low='darkgreen', high='red') +
  theme(legend.position="right")


