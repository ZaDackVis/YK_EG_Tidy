###############
####ARU NMDS-ings###
###############

##GOAL - conduct a nmds for each zone. 
##Is there a difference in community strcture between zones
#if all mountains are aggregated?
library(vegan)
library(ggplot2)
library(tidyverse)

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
ggord.PCA
#I need to aggregate all point-count locations by zone, and sum raw abundance...

ALL_Sites_by_Zone <- WF_ARU_PC %>% 
  group_by(WF_ARU_PC$Site_Zone) %>% 
  summarise_at(names(Species), sum, na.rm = FALSE)


names(ALL_Sites_by_Zone)[names(ALL_Sites_by_Zone)=="WF_ARU_PC$Site_Zone"] <- "Site_Zone"


#Split the column for later
ALL_Sites_by_Zone <- separate(ALL_Sites_by_Zone, Site_Zone, 
                              c("site", "zone"), 
                             sep = "_", FALSE, FALSE, "warn")

######Running the  the NMDS#####
ALL_Zones_ARU_MDS <- metaMDS(ALL_Sites_by_Zone[names(Species)], "bray",k = 2, autotransform = FALSE)


#Plot it basic
plot(ALL_Zones_ARU_MDS,type="n")
ordipointlabel(ALL_Zones_ARU_MDS,
               display="site",
               cex=.5)


#encircle the surface at the 95% confidence ellipses  
ordi.ellipse <- ordiellipse(ALL_Zones_ARU_MDS,
            groups = ALL_Sites_by_Zone$zone, 
            col=1:3,
            lwd=3)


#Put the spiders on it!
ordispider(ALL_Zones_ARU_MDS,
           groups = ALL_Sites_by_Zone$zone, 
           col=1:3,
           lwd=3)

#What about a sweet topographical map!?


ARU_elev <- YK_Env[c("Site_Zone","elev")] #create a vector of site elevations.


ARU_elev <- ARU_elev %>% group_by(ARU_elev$Site_Zone) %>% 
             summarise_at(c('elev'), mean, na.rm = FALSE)   #take the mean.

names(ARU_elev)[names(ARU_elev)=="ARU_elev$Site_Zone"] <- "Site_Zone" 

ARU_MDS_scores <- as.data.frame(scores(ALL_Zones_ARU_MDS))  #make a dataframe of the NMDS coordinates


ALL_Sites_by_Zone <- column_to_rownames(ALL_Sites_by_Zone,"Site_Zone")  #populate the new dataframe with descriptive varriables.

ARU_MDS_scores$Site_Zone <- rownames(ALL_Sites_by_Zone)  

ARU_MDS_scores <- separate(ARU_MDS_scores, Site_Zone, 
                          c("Site", "Zone"), 
                          sep = "_", FALSE, FALSE, "warn")


ARU_MDS_scores <- left_join(ARU_MDS_scores, ARU_elev, by = "Site_Zone")


ordisurf(ARU_MDS_scores[c("NMDS1","NMDS2")],ARU_MDS_scores$elev, add = TRUE)  #project a surface to the plot based on mean elevation. 


########Doing it all and plotting it with GGPLOT##########

#I need a geom for the topo map.

ordi <- ordisurf(ARU_MDS_scores[c("NMDS1","NMDS2")],ARU_MDS_scores$elev, add = TRUE) #butcher the function so I can see all the parts of it...

ordi.grid <- ordi$grid    #currently the ordination isn't in a form I can plot it as.

ordi.xyz <- expand.grid(x = ordi.grid$x, y = ordi.grid$y)  #Creates a matrix of XY coordinates that I can plot

ordi.xyz$z <- as.vector(ordi.grid$z) #extracts Z coordinates...

ordi.xyz <- data.frame(na.omit(ordi.xyz)) #creates a dataframe with no NAs.



################
graph <- ggplot()+
  stat_contour(data = ordi.xyz, aes(x = x, y = y, z = z, color =(..level..)), binwidth = 25)+ #adds the contour lines.
  scale_colour_gradient(high = "red", low = "green")


graph + geom_point(ARU_MDS_scores, mapping = aes(x=NMDS1, y=NMDS2))



#colors the contour lines
#  geom_point(ARU_MDS_scores, mapping = aes(x=NMDS1, y=NMDS2)) #plot survey points
 


#plots the ordisurf

  
  
#####################

#ggordi <- ggplot()+ geom_point(BorealMDS_Points, mapping = aes(x=NMDS1, y=NMDS2, color="darkgreen")) +
#  geom_point(AlpineMDS_Points,mapping = aes(x=NMDS1, y=NMDS2, color="red"))+
#  geom_point(SubalpineMDS_Points,mapping = aes(x=NMDS1, y=NMDS2, color="blue"))+
#  stat_contour(data = ordi.xyz, aes(x = x, y = y, z = z, color = rev(15)))+
#  scale_colour_gradient(high = "darkgreen", low = "darkolivegreen1")



#ggordi


#scale_color_manual(values=c("red", "blue", "darkgreen"))
