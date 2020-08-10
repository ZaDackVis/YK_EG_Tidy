################################################################
########PHYLOGENETIC AND FUNCTIONAL DIV USING HILL NUMBERS######
################################################################

library(tidyverse)
library(ape)
library(phytools)
library(picante)
library(mnormt)
library(rlist)


setwd("C:/Users/DackZ/Documents/R/Yukon_Birds")

#Read in the WF Data and make WF by Zone and by Site_Zone

WF_HO_PC <- read.csv("0_Data/WF_HO_PC.csv")


WF_All_Zones <- WF_HO_PC %>%
  group_by(WF_HO_PC$zone) %>%
  summarise_at(c(2:72), sum, na.rm = FALSE)


Species <- names(WF_All_Zones[2:72])

Species <- as.data.frame(Species)
names(Species)[names(Species) == "Species"] <- "alpha_codes"

write.csv(Species, "1_Pipe/All_HO_Species.csv")
rm(Species)


#The AOU Taxonomy and the Hackett backbone are very different. 
#Replaced the AUO Taxonomy with it 

Hackett_Taxa <- read.csv("YK_All_Species_Taxonomy.csv")


WF_All_Zones <- column_to_rownames(WF_All_Zones,"WF_HO_PC$zone")


#Pacrific Wren and Winter Wren are lumped in this Taxonomy.

Wrens <- WF_All_Zones[,c("WIWR","PAWR")]

WF_All_Zones$WIWR <- rowSums(Wrens)
WF_All_Zones$PAWR <- NULL

WF_All_Zones <- t(WF_All_Zones)
WF_All_Zones <- as.data.frame(WF_All_Zones)
WF_All_Zones <- rownames_to_column(WF_All_Zones,"alpha_codes")

WF_All_Zones <- left_join(WF_All_Zones, Hackett_Taxa, "alpha_codes")


WF_All_Zones <- column_to_rownames(WF_All_Zones, var = "Hackett_Taxa")

WF_All_Zones$alpha_codes <- NULL
WF_All_Zones$species_AOU_name <- NULL
WF_All_Zones$X.1 <- NULL
WF_All_Zones$X <- NULL


WF_All_Zones <- t(WF_All_Zones)
WF_All_Zones <- as.data.frame(WF_All_Zones)


#read in your tree 


treefile <- read.nexus("0_Data/HO_Phylo_Tree/output.nex")


#Create a consensus tree. 
#Note: p=0.5 specifies that the tree must be "majority rules consensus (MRC)".


#consensus <- consensus.edges(treefile, consensus.tree=consensus(treefile,p=0.5))

#HO_Consensus <- list.save(consensus,'HO_Phylo_Consensus.RData')

HO_Phylo_Consensus <- readRDS("~/R/Yukon_Birds/1_Pipe/HO_Phylo_Consensus.rds")

plotTree(consensus, fsize=0.6)



#######HILL NUMBERS AND PHYLOGENETIC ANALYSIS############


#HIll numbers using hillR for all zones
library(hillR)

Zonal_Phylo_Div_Faith <- hill_phylo(WF_All_Zones, HO_Phylo_Consensus, q = 0)
Zonal_Phylo_Div_Entropy <- hill_phylo(WF_All_Zones, HO_Phylo_Consensus, q = 1)
Zonal_Phylo_Div_Rao <- hill_phylo(WF_All_Zones, HO_Phylo_Consensus, q = 2)

Zonal_Phylo_Div_Faith           
Zonal_Phylo_Div_Entropy
Zonal_Phylo_Div_Rao


#Community distances

Boreal <-comdist(WF_All_Zones, cophenetic(HO_Phylo_Consensus), abundance.weighted=TRUE)
Boreal

#############3#tree with elevation for continuous data...

library(ggtree)


YK_HO_PC <- read.csv("0_Data/YK_HO_PC_Tidy.csv")

sub <-gsub("PAWR","WIWR",YK_HO_PC$species_code)
YK_HO_PC$species_code <- sub


YK_Env <- read.csv("C:/Users/DackZ/Documents/R/Yukon_Birds/0_data/YK_Env.csv")
YK_Env$location <- toupper(YK_Env$location)

elev <- YK_Env

elev <- elev[c("location","elev")]

YK_HO_PC <- left_join(YK_HO_PC, elev, "location")

Mean_Species_Elev <- YK_HO_PC 
Mean_Species_Elev <- Mean_Species_Elev[c("species_code","elev")]
names(Mean_Species_Elev)[names(Mean_Species_Elev)=="species_code"] <- "alpha_codes"


Mean_Species_Elev <- Mean_Species_Elev %>%
  group_by(alpha_codes) %>%
  summarize_at(c(1), mean)

Mean_Species_Elev <- left_join(Mean_Species_Elev,Hackett_Taxa,by = "alpha_codes")


Mean_Species_Elev <- column_to_rownames(Mean_Species_Elev, var = "Hackett_Taxa")


Mean_Species_Elev <- Mean_Species_Elev[,c("elev")]

dotTree(x = Mean_Species_Elev, 
        tree = consensus,
        legend = TRUE,
        standardize = FALSE)


ggtree(consensus)+
  geom_rootedge(rootedge = 1)+
  geom_tiplab(consensus$tip.label) 
#+ 
  scale_color_continuous(low='darkgreen', high='red') +
  theme(legend.position="right")

