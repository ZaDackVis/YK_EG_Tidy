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



WF_ARU_PC <- read.csv("WF_ARU_PC.csv")

WF_ARU_PC_Alpine <-  subset(WF_ARU_PC, zone == 'A')

#Alpine
CANOL_A <- subset(WF_ARU_PC, Site_Zone == 'CANOL_A')  #Subset by each zone
CARIB_A <- subset(WF_ARU_PC, Site_Zone == 'CARIB_A')
DANE_A <- subset(WF_ARU_PC, Site_Zone == 'DANE_A')
DEADM_A <- subset(WF_ARU_PC, Site_Zone == 'DEADM_A')
FRASE_A <- subset(WF_ARU_PC, Site_Zone == 'FRASE_A')
GLAVE_A <- subset(WF_ARU_PC, Site_Zone == 'GLAVE_A')
KUSAW_A <- subset(WF_ARU_PC, Site_Zone == 'KUSAW_A')
LORNE_A <- subset(WF_ARU_PC, Site_Zone == 'LORNE_A')
PARTO_A <- subset(WF_ARU_PC, Site_Zone == 'PARTO_A')
QUILL_A <- subset(WF_ARU_PC, Site_Zone == 'QUILL_A')
ROCK_A <- subset(WF_ARU_PC, Site_Zone == 'ROCK_A')
SUMAN_A <- subset(WF_ARU_PC, Site_Zone == 'SUMAN_A')


#
CANOL_A <- CANOL_A[-c(1,58:62)]
CARIB_A <- CARIB_A[-c(1,58:62)]
DANE_A <- DANE_A[-c(1,58:62)]
DEADM_A <- DEADM_A[-c(1,58:62)]
FRASE_A <- FRASE_A[-c(1,58:62)]
GLAVE_A <- GLAVE_A[-c(1,58:62)]
KUSAW_A <- KUSAW_A[-c(1,58:62)]
LORNE_A <- LORNE_A[-c(1,58:62)]
PARTO_A <- PARTO_A[-c(1,58:62)]
QUILL_A <- QUILL_A[-c(1,58:62)]
ROCK_A <- ROCK_A[-c(1,58:62)]
SUMAN_A <- SUMAN_A[-c(1,58:62)]




#Requires an S' x N matrix
 
CANOL_A <- t(CANOL_A)
CANOL_A <- as.data.frame(CANOL_A)
CARIB_A <- t(CARIB_A)
CARIB_A <- as.data.frame(CARIB_A)
DANE_A <- t(DANE_A)   
DANE_A <- as.data.frame(DANE_A)
DEADM_A <- t(DEADM_A)   
DEADM_A <- as.data.frame(DEADM_A)
FRASE_A <- t(FRASE_A)   
FRASE_A <- as.data.frame(FRASE_A)
GLAVE_A <- t(GLAVE_A)   
GLAVE_A <- as.data.frame(GLAVE_A)
KUSAW_A <- t(KUSAW_A)   
KUSAW_A <- as.data.frame(KUSAW_A)
LORNE_A <- t(LORNE_A)   
LORNE_A <- as.data.frame(LORNE_A)
PARTO_A <- t(PARTO_A)
PARTO_A <- as.data.frame(PARTO_A)
QUILL_A <- t(QUILL_A)   
QUILL_A <- as.data.frame(QUILL_A)
ROCK_A <- t(ROCK_A)   
ROCK_A <- as.data.frame(ROCK_A)
SUMAN_A <- t(SUMAN_A)   
SUMAN_A <- as.data.frame(SUMAN_A)



#Get rid of any species which have 0 observations
CANOL_A$remove <- rowSums(CANOL_A)
CARIB_A$remove <- rowSums(CARIB_A)
DANE_A$remove <- rowSums(DANE_A)
DEADM_A$remove <- rowSums(DEADM_A)
FRASE_A$remove <- rowSums(FRASE_A)
GLAVE_A$remove <- rowSums(GLAVE_A)
KUSAW_A$remove <- rowSums(KUSAW_A)
LORNE_A$remove <- rowSums(LORNE_A)
PARTO_A$remove <- rowSums(PARTO_A)
QUILL_A$remove <- rowSums(QUILL_A)
ROCK_A$remove <- rowSums(ROCK_A)
SUMAN_A$remove <- rowSums(SUMAN_A)



CANOL_A <- filter(CANOL_A, CANOL_A$remove > 0)
CANOL_A$remove <- NULL

CARIB_A <- filter(CARIB_A, CARIB_A$remove > 0)
CARIB_A$remove <- NULL

DANE_A <- filter(DANE_A, DANE_A$remove > 0)
DANE_A$remove <- NULL

DEADM_A <- filter(DEADM_A, DEADM_A$remove > 0)
DEADM_A$remove <- NULL

FRASE_A <- filter(FRASE_A, FRASE_A$remove > 0)
FRASE_A$remove <- NULL

GLAVE_A <- filter(GLAVE_A, GLAVE_A$remove > 0)
GLAVE_A$remove <- NULL

KUSAW_A <- filter(KUSAW_A, KUSAW_A$remove > 0)
KUSAW_A$remove <- NULL

LORNE_A <- filter(LORNE_A, LORNE_A$remove > 0)
LORNE_A$remove <- NULL

PARTO_A <- filter(PARTO_A, PARTO_A$remove > 0)
PARTO_A$remove <- NULL

QUILL_A <- filter(QUILL_A, QUILL_A$remove > 0)
QUILL_A$remove <- NULL

ROCK_A <- filter(ROCK_A, ROCK_A$remove > 0)
ROCK_A$remove <- NULL

SUMAN_A <- filter(SUMAN_A, SUMAN_A$remove > 0)
SUMAN_A$remove <- NULL


#CONVERT TO PRESENCE/ABS

CANOL_A[CANOL_A > 0] = 1
CARIB_A[CARIB_A > 0] = 1
DANE_A[DANE_A > 0] = 1
DEADM_A[DEADM_A > 0] = 1
FRASE_A[FRASE_A > 0] = 1
GLAVE_A[GLAVE_A > 0] = 1
KUSAW_A[KUSAW_A > 0] = 1
LORNE_A[LORNE_A > 0] = 1
PARTO_A[PARTO_A > 0] = 1
QUILL_A[QUILL_A > 0] = 1
ROCK_A[ROCK_A > 0] = 1
SUMAN_A[SUMAN_A > 0] = 1

CANOL_A_and_CARIBOU <- list(CANOL_A,
                            CARIB_A)
names(CANOL_A_and_CARIBOU) <- c("CANOL_A","CARIBOU_A")


CANOL_A_and_CARIBOU <- iNEXT(CANOL_A_and_CARIBOU,q=0,datatype = "incidence_raw")



Alpine <- list(CANOL_A,
               CARIB_A,
               DANE_A,
               DEADM_A,
               FRASE_A,
               GLAVE_A,
               KUSAW_A,
               LORNE_A,
               PARTO_A,
               QUILL_A,
               ROCK_A,
               SUMAN_A)
               

names(Alpine) <- c("CANOL_A",
                   "CARIB_A",
                   "DANE_A",
                   "DEADM_A",
                   "FRASE_A",
                   "GLAVE_A",
                   "KUSAW_A",
                   "LORNE_A",
                   "PARTO_A",
                   "QUILL_A",
                   "ROCK_A",
                   "SUMAN_A")


Basic_Alpine_Data <- DataInfo(Alpine, datatype = "incidence_raw")

Alpine_Data <- Basic_Alpine_Data[,c(1,2,4,5)]
names(Alpine_Data) <- c("Site_Zone","n Sample","Species Richness","Sample Coverage")




write.csv(Alpine_Data, "C:/Users/DackZ/Documents/R/Yukon_Birds/1_Pipe/Alpine_ARU_Survey_Efforts.csv")



#All zones displayed in seperate graphs, each order q has its own curve.
ggiNEXT(Canol_A, type=1, facet.var="site")   

#Orders of q displayed in seperate graphs, each Zone on own curve.
ggiNEXT(Alpine_iNEXT, type=1, facet.var="order")

#Completeness Curves (is this the same as the vegan "spec accum"?)
ggiNEXT(CANOL_A_and_CARIBOU, type=2, facet.var="site", color.var="site")

#Sample coverage‐based R/E zone graphs, orders of q curves
ggiNEXT(CANOL_A_and_CARIBOU, type=3, facet.var="site")

#Sample coverage based on R/E q graphs, zone curves.

ggiNEXT(CANOL_A_and_CARIBOU, type=3, facet.var="order", color.var="site")


