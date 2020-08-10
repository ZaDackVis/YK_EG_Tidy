#################################################
##Mountain based rarefaction for boreal zones####
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

remove <- c("site","zone","stat_num","Site_Zone")
WF_ARU_PC_Locations <- WF_ARU_PC[,!names(WF_ARU_PC) %in% paste(remove)]

locations <- unique(WF_ARU_PC_Locations$location)

WF_ARU_PC_Locations <- group_split(WF_ARU_PC_Locations, WF_ARU_PC_Locations$location, .keep = FALSE)

names(WF_ARU_PC_Locations) <- locations

All_Locations <- list()

WF_ARU_PC_Locations <- for(i in 1:length(WF_ARU_PC_Locations)){
  
  list_in <- WF_ARU_PC_Locations[[i]]
  list_in$location <- NULL
  list_out <- as.data.frame(t(as.matrix(list_in)))
  list_out <- sapply(list_out,as.numeric) %>% as.data.frame(list_out)
  colnames(list_out)=rownames(list_in) 
  rownames(list_out)=colnames(list_in)
  list_out$remove <- rowSums(list_out)
  list_out <- filter(list_out, list_out$remove > 0)
  list_out$remove <- NULL
  #list_out[list_out > 0] = 1
  
  All_Locations[[i]] <- list_out
}

names(All_Locations) <- locations

##################

Info <- DataInfo(All_Locations, datatype = "abundance")


Point_Effort <- as.data.frame(cbind(Info$site,Info$T,Info$S.obs,Info$SC))


names(Point_Effort) <- c("Station","n Samples","Species Richness","Sample Coverage")

write.csv(Point_Effort, "Point_Efforts_With_UNKNS.csv")


All_Locations_iNEXT <- iNEXT(All_Locations, datatype = "incidence_raw")

Canol_Alpine <- All_Locations[1:4]

Canol_Alpine <- iNEXT(Canol_Alpine, datatype = "incidence_raw")

#All zones displayed in separate graphs, each order q has its own curve.

ggiNEXT(Canol_Alpine, type=1, facet.var="site")


#Orders of q displayed in seperate graphs, each Zone on own curve.
ggiNEXT(Canol_Alpine, type=1, facet.var="order")

#Completeness Curves (is this the same as the vegan "spec accum"?)
ggiNEXT(Canol_Alpine, type=1, facet.var="site")

#Completeness Curves, type=2, facet.var="none", color.var="site")

ggiNEXT(Canol_Alpine, type=2, facet.var="site")


#Sample coverage‐based R/E zone graphs, orders of q curves
ggiNEXT(Canol_Alpine, type=3, facet.var="site")

#Sample coverage based on R/E q graphs, zone curves.

ggiNEXT(Canol_Alpine, type=3, facet.var="site", color.var="site")


