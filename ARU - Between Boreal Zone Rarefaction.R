#################################################
##Mountain based rarefaction for boreal zones####
#################################################


#The purpose of this script is to impliment the framework proposed by Chao et al 2014:
#Unifying Species DiversityThrough Hill Numbers

library(tidyverse)
library(SpadeR)
library(iNEXT)
library(ggplot2)



setwd("C:/Users/DackZ/Documents/R/Yukon_Birds")
pipe <- "~/R/Yukon_Birds/1_Pipe"



WF_ARU_PC <- read.csv("WF_ARU_PC.csv")

WF_ARU_PC_Boreal <- subset(WF_ARU_PC,subset = zone == "B")

rm(WF_ARU_PC)

#Subset by Boreal Zones

CARIB_B <- subset(WF_ARU_PC_Boreal, Site_Zone == 'CARIB_B')
DANE_B <- subset(WF_ARU_PC_Boreal, Site_Zone == 'DANE_B')
FRASE_B <- subset(WF_ARU_PC_Boreal, Site_Zone == 'FRASE_B')
GLAVE_B <- subset(WF_ARU_PC_Boreal, Site_Zone == 'GLAVE_B')
LORNE_B <- subset(WF_ARU_PC_Boreal, Site_Zone == 'LORNE_B')
PARTO_B <- subset(WF_ARU_PC_Boreal, Site_Zone == 'PARTO_B')
ROCK_B <- subset(WF_ARU_PC_Boreal, Site_Zone == 'ROCK_B')
SUMAN_B <- subset(WF_ARU_PC_Boreal, Site_Zone == 'SUMAN_B')

#Remove unecessary columns...


CARIB_B <- CARIB_B[-c(1,58:62)]
DANE_B <- DANE_B[-c(1,58:62)]
FRASE_B <- FRASE_B[-c(1,58:62)]
GLAVE_B <- GLAVE_B[-c(1,58:62)]
LORNE_B <- LORNE_B[-c(1,58:62)]
PARTO_B <- PARTO_B[-c(1,58:62)]
ROCK_B <- ROCK_B[-c(1,58:62)]
SUMAN_B <- SUMAN_B[-c(1,58:62)]


#Make a species x Sample (S x N')
CARIB_B <- t(sapply(CARIB_B, as.numeric))
CARIB_B <- as.data.frame(CARIB_B)

DANE_B <- t(sapply(DANE_B, as.numeric))
DANE_B <- as.data.frame(DANE_B)


FRASE_B <- t(sapply(FRASE_B, as.numeric))
FRASE_B <- as.data.frame(FRASE_B)

GLAVE_B <- t(sapply(GLAVE_B, as.numeric))
GLAVE_B <- as.data.frame(GLAVE_B)

LORNE_B <- t(sapply(LORNE_B, as.numeric))
LORNE_B <- as.data.frame(LORNE_B)

PARTO_B <- t(sapply(PARTO_B, as.numeric))
PARTO_B <- as.data.frame(PARTO_B)

ROCK_B <- t(sapply(ROCK_B, as.numeric))
ROCK_B <- as.data.frame(ROCK_B)

SUMAN_B <- t(sapply(SUMAN_B, as.numeric))
SUMAN_B <- as.data.frame(SUMAN_B)

#Remove any samples which had 0 sightings for any given species.

CARIB_B$remove <- rowSums(CARIB_B)
CARIB_B <- filter(CARIB_B, CARIB_B$remove > 0)
CARIB_B$remove <- NULL


DANE_B$remove <- rowSums(DANE_B)
DANE_B <- filter(DANE_B, DANE_B$remove > 0)
DANE_B$remove <- NULL

FRASE_B$remove <- rowSums(FRASE_B)
FRASE_B <- filter(FRASE_B, FRASE_B$remove > 0)
FRASE_B$remove <- NULL

GLAVE_B$remove <- rowSums(GLAVE_B)
GLAVE_B <- filter(GLAVE_B, GLAVE_B$remove > 0)
GLAVE_B$remove <- NULL

LORNE_B$remove <- rowSums(LORNE_B)
LORNE_B <- filter(LORNE_B, LORNE_B$remove > 0)
LORNE_B$remove <- NULL

PARTO_B$remove <- rowSums(PARTO_B)
PARTO_B <- filter(PARTO_B, PARTO_B$remove > 0)
PARTO_B$remove <- NULL

ROCK_B$remove <- rowSums(ROCK_B)
ROCK_B <- filter(ROCK_B, ROCK_B$remove > 0)
ROCK_B$remove <- NULL

SUMAN_B$remove <- rowSums(SUMAN_B)
SUMAN_B <- filter(SUMAN_B, SUMAN_B$remove > 0)
SUMAN_B$remove <- NULL

#Convert all to binary pres/abs.


CARIB_B[CARIB_B > 0] = 1
DANE_B[DANE_B > 0] = 1
FRASE_B[FRASE_B > 0] = 1
GLAVE_B[GLAVE_B > 0] = 1
LORNE_B[LORNE_B > 0] = 1
PARTO_B[PARTO_B > 0] = 1
ROCK_B[ROCK_B > 0] = 1
SUMAN_B[SUMAN_B > 0] = 1


All_Boreal <- list(CARIB_B,
                   DANE_B,
                   FRASE_B,
                   GLAVE_B,
                   LORNE_B,
                   PARTO_B,
                   ROCK_B,
                   SUMAN_B)


names(All_Boreal) <- c("CARIB_B",
                   "DANE_B",
                   "FRASE_B",
                   "GLAVE_B",
                   "LORNE_B",
                   "PARTO_B",
                   "ROCK_B",
                   "SUMAN_B")



Boreal_iNEXT <- iNEXT(All_Boreal, q = c(0, 1, 2), datatype = "incidence_raw")

Boreal_Info <- DataInfo(All_Boreal, datatype = "incidence_raw")

Boreal_Info

write.csv(Boreal_Info,"1_Pipe/Boreal_ARU_Survey_Effort.csv")

ggiNEXT(Boreal_iNEXT, type=1, facet.var="site")  


ggiNEXT(Boreal_iNEXT, type=1, facet.var="site")   

#Orders of q displayed in seperate graphs, each Zone on own curve.
ggiNEXT(Boreal_iNEXT, type=1, facet.var="order")

#Completeness Curves (is this the same as the vegan "spec accum"?)
ggiNEXT(Boreal_iNEXT, type=2, facet.var="none", color.var="site")

#Sample coverage‐based R/E zone graphs, orders of q curves
ggiNEXT(Boreal_iNEXT, type=3, facet.var="site")

#Sample coverage based on R/E q graphs, zone curves.

ggiNEXT(Boreal_iNEXT, type=3, facet.var="order", color.var="site")
