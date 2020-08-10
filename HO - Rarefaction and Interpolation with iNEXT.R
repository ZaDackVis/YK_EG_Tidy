#################################################
##Species Diversity Curves using Hill Numbers####
#################################################


#The purpose of this script is to impliment the framework proposed by Chao et al 2014:
#Unifying Species DiversityThrough Hill Numbers

library(tidyverse)
library(SpadeR)
library(iNEXT)
library(ggplot2)



setwd("C:/Users/DackZ/Documents/R/Yukon_Birds/0_Data")
pipe <- "~/R/Yukon_Birds/1_Pipe"



WF_HO_PC <- read.csv("WF_HO_PC.csv")


Alpine <- subset(WF_HO_PC, zone == 'A')  #Subset by each zone
SubAlp <- subset(WF_HO_PC, zone == 'S')
Boreal <- subset(WF_HO_PC, zone == 'B')


Alpine <- Alpine[-c(1,73:77)]               #I don't require the location data.
SubAlp <- SubAlp[-c(1,73:77)] 
Boreal <- Boreal[-c(1,73:77)] 


Alpine <- t(sapply(Alpine, as.numeric))
Alpine <- as.data.frame(Alpine)

SubAlp <- t(sapply(SubAlp, as.numeric))
SubAlp <- as.data.frame(SubAlp)


Boreal <- t(sapply(Boreal, as.numeric))
Boreal <- as.data.frame(Boreal)



Alpine_iNEXT <- Alpine            #copy it so we don't fuck anything up...
Alpine_iNEXT$remove <- 0
Alpine_iNEXT$remove <- rowSums(Alpine_iNEXT)
Alpine_iNEXT <- filter(Alpine_iNEXT, Alpine_iNEXT$remove > 0)
Alpine_iNEXT$remove <- NULL
Alpine_iNEXT[Alpine_iNEXT > 0] <- 1


SubAlpine_iNEXT <- SubAlp            #copy it so we don't fuck anything up...
SubAlpine_iNEXT$remove <- rowSums(SubAlpine_iNEXT)
SubAlpine_iNEXT <- filter(SubAlpine_iNEXT, SubAlpine_iNEXT$remove > 0)
SubAlpine_iNEXT$remove <- NULL
SubAlpine_iNEXT[SubAlpine_iNEXT > 0] <- 1


Boreal_iNEXT <- Boreal            #copy it so we don't fuck anything up...
Boreal_iNEXT$remove <- rowSums(Boreal_iNEXT)
Boreal_iNEXT <- filter(Boreal_iNEXT, Boreal_iNEXT$remove > 0)
Boreal_iNEXT$remove <- NULL
Boreal_iNEXT[Boreal_iNEXT > 0] <- 1


Zones <- list(Alpine_iNEXT,SubAlpine_iNEXT,Boreal_iNEXT)
names(Zones) <- c("Alpine","Subalpine", "Boreal")




Zones_iNEXT <- iNEXT(Zones, q = c(0, 1, 2 ), datatype = "incidence_raw")

#All zones displayed in seperate graphs, each order q has its own curve.
Species_Zone <- ggiNEXT(Zones_iNEXT, type=1, facet.var="site")   

#Orders of q displayed in seperate graphs, each Zone on own curve.
ggiNEXT(Zones_iNEXT, type=1, facet.var="order")

#Completeness Curves (is this the same as the vegan "spec accum"?)
ggiNEXT(Zones_iNEXT, type=2, facet.var="none", color.var="site")

#Sample coverage‐based R/E zone graphs, orders of q curves
ggiNEXT(Zones_iNEXT, type=3, facet.var="site")

#Sample coverage based on R/E q graphs, zone curves.

ggiNEXT(Zones_iNEXT, type=3, facet.var="order", color.var="site")


#######Get the data

Alpine_Diversity <- Diversity(Zones$Alpine, datatype = "incidence_raw", q=NULL)
Alpine_Diversity



Boreal_Diversity <- Diversity(Boreal_iNEXT, datatype = "incidence_raw", q=NULL)

Boreal_Diversity <- Diversity(Boreal_iNEXT,datatype = "incidence_raw")


SubAlpine_Diversity <- Diversity(SubAlpine_iNEXT, datatype = "incidence_raw", q = NULL)

Alpine_Diversity
SubAlpine_Diversity
Boreal_Diversity
