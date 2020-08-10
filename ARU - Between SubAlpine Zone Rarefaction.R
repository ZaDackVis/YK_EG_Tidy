#################################################
##Species Diversity Curves using Hill Numbers####
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

WF_ARU_PC_SA <-  subset(WF_ARU_PC, zone == 'S')

#SubAlpine Only
CANOL_S <- subset(WF_ARU_PC_SA, Site_Zone == 'CANOL_S')  #Subset by each zone
CARIB_S <- subset(WF_ARU_PC_SA, Site_Zone == 'CARIB_S')
DANE_S <- subset(WF_ARU_PC, Site_Zone == 'DANE_S')
DEADM_S <- subset(WF_ARU_PC_SA, Site_Zone == 'DEADM_S')
FRASE_S <- subset(WF_ARU_PC_SA, Site_Zone == 'FRASE_S')
GLAVE_S <- subset(WF_ARU_PC_SA, Site_Zone == 'GLAVE_S')
KUSAW_S <- subset(WF_ARU_PC_SA, Site_Zone == 'KUSAW_S')
LORNE_S <- subset(WF_ARU_PC_SA, Site_Zone == 'LORNE_S')
PARTO_S <- subset(WF_ARU_PC_SA, Site_Zone == 'PARTO_S')
QUILL_S <- subset(WF_ARU_PC_SA, Site_Zone == 'QUILL_S')
ROCK_S <- subset(WF_ARU_PC_SA, Site_Zone == 'ROCK_S')
SUMAN_S <- subset(WF_ARU_PC_SA, Site_Zone == 'SUMAN_S')


#
CANOL_S <- CANOL_S[-c(1,58:62)]
CARIB_S <- CARIB_S[-c(1,58:62)]
DANE_S <- DANE_S[-c(1,58:62)]
DEADM_S <- DEADM_S[-c(1,58:62)]
FRASE_S <- FRASE_S[-c(1,58:62)]
GLAVE_S <- GLAVE_S[-c(1,58:62)]
KUSAW_S <- KUSAW_S[-c(1,58:62)]
LORNE_S <- LORNE_S[-c(1,58:62)]
PARTO_S <- PARTO_S[-c(1,58:62)]
QUILL_S <- QUILL_S[-c(1,58:62)]
ROCK_S <- ROCK_S[-c(1,58:62)]
SUMAN_S <- SUMAN_S[-c(1,58:62)]




#Requires an S' x N matrix

CANOL_S <- t(CANOL_S)
CANOL_S <- as.data.frame(CANOL_S)
CARIB_S <- t(CARIB_S)
CARIB_S <- as.data.frame(CARIB_S)
DANE_S <- t(DANE_S)   
DANE_S <- as.data.frame(DANE_S)
DEADM_S <- t(DEADM_S)   
DEADM_S <- as.data.frame(DEADM_S)
FRASE_S <- t(FRASE_S)   
FRASE_S <- as.data.frame(FRASE_S)
GLAVE_S <- t(GLAVE_S)   
GLAVE_S <- as.data.frame(GLAVE_S)
KUSAW_S <- t(KUSAW_S)   
KUSAW_S <- as.data.frame(KUSAW_S)
LORNE_S <- t(LORNE_S)   
LORNE_S <- as.data.frame(LORNE_S)
PARTO_S <- t(PARTO_S)
PARTO_S <- as.data.frame(PARTO_S)
QUILL_S <- t(QUILL_S)   
QUILL_S <- as.data.frame(QUILL_S)
ROCK_S <- t(ROCK_S)   
ROCK_S <- as.data.frame(ROCK_S)
SUMAN_S <- t(SUMAN_S)   
SUMAN_S <- as.data.frame(SUMAN_S)



#Get rid of any species which have 0 observations
CANOL_S$remove <- rowSums(CANOL_S)
CARIB_S$remove <- rowSums(CARIB_S)
DANE_S$remove <- rowSums(DANE_S)
DEADM_S$remove <- rowSums(DEADM_S)
FRASE_S$remove <- rowSums(FRASE_S)
GLAVE_S$remove <- rowSums(GLAVE_S)
KUSAW_S$remove <- rowSums(KUSAW_S)
LORNE_S$remove <- rowSums(LORNE_S)
PARTO_S$remove <- rowSums(PARTO_S)
QUILL_S$remove <- rowSums(QUILL_S)
ROCK_S$remove <- rowSums(ROCK_S)
SUMAN_S$remove <- rowSums(SUMAN_S)



CANOL_S <- filter(CANOL_S, CANOL_S$remove > 0)
CANOL_S$remove <- NULL

CARIB_S <- filter(CARIB_S, CARIB_S$remove > 0)
CARIB_S$remove <- NULL

DANE_S <- filter(DANE_S, DANE_S$remove > 0)
DANE_S$remove <- NULL

DEADM_S <- filter(DEADM_S, DEADM_S$remove > 0)
DEADM_S$remove <- NULL

FRASE_S <- filter(FRASE_S, FRASE_S$remove > 0)
FRASE_S$remove <- NULL

GLAVE_S <- filter(GLAVE_S, GLAVE_S$remove > 0)
GLAVE_S$remove <- NULL

KUSAW_S <- filter(KUSAW_S, KUSAW_S$remove > 0)
KUSAW_S$remove <- NULL

LORNE_S <- filter(LORNE_S, LORNE_S$remove > 0)
LORNE_S$remove <- NULL

PARTO_S <- filter(PARTO_S, PARTO_S$remove > 0)
PARTO_S$remove <- NULL

QUILL_S <- filter(QUILL_S, QUILL_S$remove > 0)
QUILL_S$remove <- NULL

ROCK_S <- filter(ROCK_S, ROCK_S$remove > 0)
ROCK_S$remove <- NULL

SUMAN_S <- filter(SUMAN_S, SUMAN_S$remove > 0)
SUMAN_S$remove <- NULL


#CONVERT TO PRESENCE/ABS

CANOL_S[CANOL_S > 0] = 1
CARIB_S[CARIB_S > 0] = 1
DANE_S[DANE_S > 0] = 1
DEADM_S[DEADM_S > 0] = 1
FRASE_S[FRASE_S > 0] = 1
GLAVE_S[GLAVE_S > 0] = 1
KUSAW_S[KUSAW_S > 0] = 1
LORNE_S[LORNE_S > 0] = 1
PARTO_S[PARTO_S > 0] = 1
QUILL_S[QUILL_S > 0] = 1
ROCK_S[ROCK_S > 0] = 1
SUMAN_S[SUMAN_S > 0] = 1




SubAlpine <- list(CANOL_S,
               CARIB_S,
               DANE_S,
               DEADM_S,
               FRASE_S,
               GLAVE_S,
               KUSAW_S,
               LORNE_S,
               PARTO_S,
               QUILL_S,
               ROCK_S,
               SUMAN_S)




names(SubAlpine) <- c("CANOL_S",
                   "CARIB_S",
                   "DANE_S",
                   "DEADM_S",
                   "FRASE_S",
                   "GLAVE_S",
                   "KUSAW_S",
                   "LORNE_S",
                   "PARTO_S",
                   "QUILL_S",
                   "ROCK_S",
                   "SUMAN_S")


Basic_SubAlpine_Data <- DataInfo(SubAlpine, datatype = "incidence_raw")

Basic_SubAlpine_Data

write.csv(Basic_SubAlpine_Data,"Subalpine_ARU_Survey_Effort.csv")


#################All zones displayed in seperate graphs, each order q has its own curve.##############
ggiNEXT(Alpine_iNEXT, type=1, facet.var="site")   

#Orders of q displayed in seperate graphs, each Zone on own curve.
ggiNEXT(Alpine_iNEXT, type=1, facet.var="order")

#Completeness Curves (is this the same as the vegan "spec accum"?)
ggiNEXT(Alpine_iNEXT, type=2, facet.var="none", color.var="site")

#Sample coverage‐based R/E zone graphs, orders of q curves
ggiNEXT(Alpine_iNEXT, type=3, facet.var="site")

#Sample coverage based on R/E q graphs, zone curves.

ggiNEXT(Alpine_iNEXT, type=3, facet.var="order", color.var="site")


#######Get the data

Alpine_Diversity <- Diversity(Alpine_iNEXT , datatype = "incidence_raw", q = c(0.0,1.0,2.0))
Alpine_Diversity


Boreal_Diversity <- Diversity(Boreal_iNEXT , datatype = "incidence_raw", q = c(0.0,1.0,2.0))
Boreal_Diversity


SubAlpine_Diversity <- Diversity(SubAlpine_iNEXT, datatype = "incidence_raw", q = c(0.0,1.0,2.0))

Alpine_Diversity
SubAlpine_Diversity
Boreal_Diversity