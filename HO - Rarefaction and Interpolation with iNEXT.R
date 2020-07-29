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

WF_HO_PC$X <- NULL




################Subset each Zone and make Abundance Vectors##############
Alpine <- subset(WF_HO_PC, zone == 'A')  #Subset by each zone
SubAlp <- subset(WF_HO_PC, zone == 'S')
Boreal <- subset(WF_HO_PC, zone == 'B')

Alpine <- Alpine[-c(74:78)]               #I don't require the location data.
SubAlp <- SubAlp[-c(74:78)]
Boreal <- Boreal[-c(74:78)]

Alpine <- t(Alpine)                       #Requires an S' x N matrix
Alpine <- as.data.frame(Alpine)
Alpine$abundance <- rowSums(Alpine)
Alpine <- Alpine[c("abundance")]

SubAlp <- t(SubAlp)
SubAlp <- as.data.frame(SubAlp)
SubAlp$abundance <- rowSums(SubAlp)
SubAlp <- SubAlp[c("abundance")]

Boreal <- t(Boreal)
Boreal <- as.data.frame(Boreal)
Boreal$abundance <- rowSums(Boreal)
Boreal <- Boreal[c("abundance")]

######iNEXT interpolation and extrapolation###


Alpine_iNEXT <- Alpine            #copy it so we don't fuck anything up...
Alpine_iNEXT <- rownames_to_column(Alpine_iNEXT,"Remove") #Remove the row name
Alpine_iNEXT$Remove <- NULL
Alpine_iNEXT <- t(Alpine_iNEXT)    #Requires a vector of just abundances, no ID
Alpine_iNEXT <- as.data.frame(Alpine_iNEXT)
Alpine_iNEXT <- Alpine_iNEXT[,-(which(colSums(Alpine_iNEXT)==0))]  #Does not take kindly to 0's
Alp <- as.numeric(Alpine_iNEXT[1,])  #Create a vector of just the values


SubAlp_iNEXT <- SubAlp            #copy it so we don't fuck anything up...
SubAlp_iNEXT <- rownames_to_column(SubAlp_iNEXT,"Remove") #Remove the row name
SubAlp_iNEXT$Remove <- NULL
SubAlp_iNEXT <- t(SubAlp_iNEXT)    #Requires a vector of just abundances, no ID
SubAlp_iNEXT <- as.data.frame(SubAlp_iNEXT)
SubAlp_iNEXT <- SubAlp_iNEXT[,-(which(colSums(SubAlp_iNEXT)==0))]  #Does not take kindly to 0's
SubAlp <- as.numeric(SubAlp_iNEXT[1,])  #Create a vector of just the values



Boreal_iNEXT <- Boreal            #copy it so we don't fuck anything up...
Boreal_iNEXT <- rownames_to_column(Boreal_iNEXT,"Remove") #Remove the row name
Boreal_iNEXT$Remove <- NULL
Boreal_iNEXT <- t(Boreal_iNEXT)    #Requires a vector of just abundances, no ID
Boreal_iNEXT <- as.data.frame(Boreal_iNEXT)
Boreal_iNEXT <- Boreal_iNEXT[,-(which(colSums(Boreal_iNEXT)==0))]  #Does not take kindly to 0's
Boreal <- as.numeric(Boreal_iNEXT[1,])  #Create a vector of just the values


Zones <- list(Alp,SubAlp,Boreal)
names(Zones) <- c("Alpine","Subalpine", "Boreal")

Zones_iNEXT <- iNEXT(Zones, q = c(0, 1, 2 ), datatype = "abundance")

########AGGREGATED GRAPHS#######


#All zones displayed in seperate graphs, each order q has its own curve.
ggiNEXT(Zones_iNEXT, type=1, facet.var="site")   

#Orders of q displayed in seperate graphs, each Zone on own curve.
ggiNEXT(Zones_iNEXT, type=1, facet.var="order")

#Completeness Curves (is this the same as the vegan "spec accum"?)
ggiNEXT(Zones_iNEXT, type=2, facet.var="none", color.var="site")

#Sample coverage‐based R/E zone graphs, orders of q curves
ggiNEXT(Zones_iNEXT, type=3, facet.var="site")

#Sample coverage based on R/E q graphs, zone curves.

ggiNEXT(Zones_iNEXT, type=3, facet.var="order", color.var="site")


#######Get the data

Alpine_Diversity <- Diversity(Alpine, datatype = "abundance", q = c(0.0,1.0,2.0))
Alpine_Diversity


Boreal_Diversity <- Diversity(Boreal, datatype = "abundance", q = c(0.0,1.0,2.0))
Boreal_Diversity


SubAlpine_Diversity <- Diversity(SubAlp, datatype = "abundance", q = c(0.0,1.0,2.0))

Alpine_Diversity
SubAlpine_Diversity
Boreal_Diversity
