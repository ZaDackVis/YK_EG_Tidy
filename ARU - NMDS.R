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

WF_ARU_PC <- read.csv("0_Data/WF_ARU_PC.csv")
WF_ARU_PC <- column_to_rownames(WF_ARU_PC, "X")


##ALL_Sites, aggregate all point-count locations by zone, sum raw abundance...

ALL_Sites_by_Zone <- WF_ARU_PC %>% 
  group_by(WF_ARU_PC$Site_Zone) %>% 
  summarise_at(c(1:55), sum, na.rm = FALSE)

#it changes the name of the first column for some reason, change it back.
names(ALL_Sites_by_Zone)[names(ALL_Sites_by_Zone)=="WF_ARU_PC$Site_Zone"] <- "Site_Zone"


#Split the column for later...?
ALL_Sites_by_Zone <- separate(ALL_Sites_by_Zone, Site_Zone, 
                              c("Site", "Zone"), 
                              sep = "-", FALSE, FALSE, "warn")

#Run the NMDS
ALL_Zones_ARU_MDS <- metaMDS(ALL_Sites_by_Zone[4:58], "bray",k = 2, autotransform = FALSE)


#Plot it basic
plot(ALL_Zones_ARU_MDS,type="n")
ordipointlabel(ALL_Zones_ARU_MDS,
               display="sites",
               cex=.5)



ordiellipse(ALL_Zones_ARU_MDS,
            groups = ALL_Sites_by_Zone$Zone, 
            col=1:3,
            lwd=3)

#Put the spiders on it
ordispider(ALL_Zones_ARU_MDS,
           groups = ALL_Sites_by_Zone$Zone, 
           col=1:3,
           lwd=3)

#sweet topographical map!






####ALL_Sites, aggregate all point-coint locations by zone, presence/absence of species, no sqrt transform####


# ALL_Sites_by_Zone_Pres_Abs <- ALL_Sites_by_Zone

# Site_Zone <- ALL_Sites_by_Zone_Pres_Abs$`WF_ARU_PC$Site_Zone` 

# ALL_Sites_by_Zone_Pres_Abs[ALL_Sites_by_Zone_Pres_Abs > 0] <- 1
# ALL_Sites_by_Zone_Pres_Abs$`WF_ARU_PC$Site_Zone` <- NULL

# ALL_Sites_by_Zone_Pres_Abs <- ALL_Sites_by_Zone_Pres_Abs %>% cbind(Site_Zone)
# rm(Site_Zone)

# ALL_Sites_PA_MDS <- metaMDS(ALL_Sites_by_Zone_Pres_Abs[1:56], "bray",k = 2, autotransform = FALSE)

# plot(ALL_Sites_PA_MDS)

# stressplot(ALL_Sites_PA_MDS)


###Plotting all points GGPLOT#####

##Alpine

Alpine <- subset(WF_ARU_PC, zone == "A")


Alpine_means <- Alpine %>% 
  group_by(Site_Zone) %>% 
  summarise_all(funs(mean))


AlpineMDS <- metaMDS(Alpine_means[2:56],"bray",k = 2, autotransform = FALSE)
plot(AlpineMDS)



##Subalpine

Subalpine <- subset(WF_ARU_PC,zone == "S")


Subalpine <- Subalpine %>% 
  group_by(Site_Zone) %>% 
  summarise_all(funs(mean))

SubalpineMDS <- metaMDS(Subalpine[2:56], "bray",k = 2, autotransform = FALSE)
plot(SubalpineMDS)



##Boreal


Boreal <- subset(WF_ARU_PC,zone == "B")

Boreal <- Boreal %>% 
  group_by(Site_Zone) %>% 
  summarise_all(funs(mean))

BorealMDS <- metaMDS(Boreal[2:56], "bray",k = 2, autotransform = FALSE)

plot(BorealMDS)




##plot them 



plot(AlpineMDs)
stressplot(AlpineMDS)
Alpine_Stress <- AlpineMDS$stress
Alpine_Stress 

points(SubalpineMDS)
stressplot(SubalpineMDS)
Subalpine_Stress <- SubalpineMDS$stress
Subalpine_Stress 

plot(BorealMDS)
stressplot(BorealMDS)
Boreal_Stress <- BorealMDS$stress
BorealMDS_Stress 




BorealMDS_Points <- as.data.frame(BorealMDS$points)

BorealMDS_Points$zone <- "Boreal"

SubalpineMDS_Points <- as.data.frame(SubalpineMDS$points)

SubalpineMDS_Points$zone <- "Subalpine"

AlpineMDS_Points <- as.data.frame(AlpineMDS$points)

AlpineMDS_Points$zone <- "Alpine"


ggplot() + geom_point(BorealMDS_Points, mapping = aes(x=MDS1, y=MDS2, color=zone)) +
  geom_point(AlpineMDS_Points,mapping = aes(x=MDS1, y=MDS2, color=zone))+
  geom_point(SubalpineMDS_Points,mapping = aes(x=MDS1, y=MDS2, color=zone))





