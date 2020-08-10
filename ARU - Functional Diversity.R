###################################
########ARU FUNCTIONAL DIV#########
###################################

#This script will compile and analyize functional traits from multiple databases
#Shread et al. 2020 - Hand-wing index
#Rickleffs 2017 - Passerine morphology: external measurements of approximately one‐quarter of passerine bird species
#Any others that may be interesting IDK.



library(tidyverse)
library(vegan)
library(FD)
library(readxl)

setwd("C:/Users/DackZ/Documents/R/Yukon_Birds")

Taxa <- read.csv("0_Data/YK_All_Species_Taxonomy.csv")

HWI <- read_excel("0_Data/FD/Sheard_2020_Global_HWI.xlsx")
names(HWI)[names(HWI)=="Tree name"] <- "Hackett_Taxa"

HWI <- HWI[!names(HWI) %in% c("Species-ID","IUCN name" ,"Synonym","Notes")]

Taxa <- left_join(Taxa,HWI,"Hackett_Taxa", keep = FALSE)


