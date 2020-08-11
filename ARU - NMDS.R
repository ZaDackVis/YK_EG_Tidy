
##GOAL - conduct a nmds for each zone. 
##Is there a difference in community strcture between zones
#if all mountains are aggregated?
library(vegan)
library(ggplot2)
library(tidyverse)
library(ggordiplots)

setwd("~/R/Yukon_Birds")

#####Prelim work#####

WF_ARU_PC <- read.csv("0_Data/WF_ARU_PC.csv")
WF_ARU_PC <- column_to_rownames(WF_ARU_PC, "X")


#read in the environmental data in for later...
YK_Env <- read.csv("0_Data/YK_Env.csv")


#make a list of species to refer to later.

Species <- WF_ARU_PC[!names(WF_ARU_PC) %in% c("location","site","zone","stat_num","Site_Zone")]

Species$GCRF <- NULL #a single obs of GCRF may be removed if I want to...

#####Getting matrix in order######
#I need to aggregate all point-count locations by zone, and sum raw abundance...

ALL_Sites_by_Zone <- WF_ARU_PC %>% 
  group_by(WF_ARU_PC$Site_Zone) %>% 
  summarise_at(names(Species), sum, na.rm = FALSE)


names(ALL_Sites_by_Zone)[names(ALL_Sites_by_Zone)=="WF_ARU_PC$Site_Zone"] <- "Site_Zone"


#Split the column for later
ALL_Sites_by_Zone <- separate(ALL_Sites_by_Zone, Site_Zone, 
                              c("site", "zone"), 
                             sep = "_", FALSE, FALSE, "warn")

######Running the  the NMDS for ELEVATION#####




ARU_MDS <- metaMDS(ALL_Sites_by_Zone[names(Species)], "bray",k = 2, autotransform = FALSE)
stressplot(ARU_MDS)

#adding env data to the matrix.
ARU_elev <- YK_Env[c("Site_Zone","elev", "Tmax_sm")] #create a vector of site elevations.


ARU_elev <- ARU_elev %>% group_by(ARU_elev$Site_Zone) %>% 
             summarise_at(c('elev','Tmax_sm'), mean, na.rm = FALSE)   #take the mean.

names(ARU_elev)[names(ARU_elev)=="ARU_elev$Site_Zone"] <- "Site_Zone" 

ARU_MDS_scores <- as.data.frame(scores(ARU_MDS))  #make a dataframe of the NMDS coordinates


ALL_Sites_by_Zone <- column_to_rownames(ALL_Sites_by_Zone,"Site_Zone")  #populate the new dataframe with descriptive varriables.

ARU_MDS_scores$Site_Zone <- rownames(ALL_Sites_by_Zone)  

ARU_MDS_scores <- separate(ARU_MDS_scores, Site_Zone, 
                          c("Site", "Zone"), 
                          sep = "_", FALSE, FALSE, "warn")


ARU_MDS_scores <- left_join(ARU_MDS_scores, ARU_elev, by = "Site_Zone")



#############Base graphics###########
plot(ARU_MDS,type="n")
ordipointlabel(ARU_MDS,
               display="sites",
               cex=.5)

ordi.ellipse <- ordiellipse(ARU_MDS,
                            groups = ALL_Sites_by_Zone$zone, 
                            col=1:3,
                            lwd=3)


#Put the spiders on it!
ordispider(ARU_MDS,
           groups = ALL_Sites_by_Zone$zone, 
           col=1:3,
           lwd=3)
ordisurf(ARU_MDS_scores[c("NMDS1","NMDS2")],ARU_MDS_scores$elev, add = TRUE)  #project a surface to the plot based on mean elevation. 

Elev_Fit <- envfit(ARU_MDS_scores[c("NMDS1","NMDS2")], ARU_MDS_scores$Tmax_sm, permutations = 999, strata=NULL, choices=c(1,2),display = "species")

plot(Elev_Fit, add = TRUE)

####GGordination with ggordiplot###

#plot points with elev vector
GGElev_Fit <- gg_envfit(ord = ARU_MDS_scores[c("NMDS1","NMDS2")],env = ARU_MDS_scores$elev,perm = 1000,groups = ARU_MDS_scores$Zone)
 
#plot ppints with max summer temp as vector.
gg_envfit(ord = ARU_MDS_scores[c("NMDS1","NMDS2")], env = ARU_MDS_scores$Tmax_sm,perm = 1000,groups = ARU_MDS_scores$Zone)


#Ordiplot of NMDS with ellipses and spiders.
gg_ordiplot(ord = ARU_MDS_scores[c("NMDS1","NMDS2")],groups = ARU_MDS_scores$Zone, ellipse = TRUE,spiders = TRUE)



#gg ordisurf (cannot manipulate colors.)

gg_ordisurf(ord = ARU_MDS_scores[c("NMDS1","NMDS2")],env = ARU_MDS_scores$elev)


#########Hacked Ordi_surf using GGplot for color changing. #####
#I need a geom for the topo map.

ordi <- ordisurf(ARU_MDS_scores[c("NMDS1","NMDS2")],ARU_MDS_scores$elev, add = TRUE) #butcher the function so I can see all the parts of it...

ordi.grid <- ordi$grid    #currently the ordination isn't in a form I can plot it as.

ordi.xyz <- expand.grid(x = ordi.grid$x, y = ordi.grid$y)  #Creates a matrix of XY coordinates that I can plot

ordi.xyz$z <- as.vector(ordi.grid$z) #extracts Z coordinates...

ordi.xyz <- data.frame(na.omit(ordi.xyz)) #creates a dataframe with no NAs.

rm(ordi.grid)
rm(ordi)

GGElev_Surf <- ggplot()+geom_point(ARU_MDS_scores, mapping = aes(x=NMDS1, y=NMDS2))+
  stat_contour(data = ordi.xyz, aes(x = x, y = y, z = z, color =(..level..)), binwidth = 25)+ #adds the contour lines.
  scale_colour_gradient(high = "darkorange", low = "darkgreen")


GGElev_Surf
