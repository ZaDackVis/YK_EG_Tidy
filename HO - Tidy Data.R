############################
######HUMAN OBSERVER########
############################

###The goal of this script is to compile and tidy all Human Observer data
###from the YK Elevational Gradient project 2019.

###This data is compiled using a CSV exported from the Yukon CWS bird observations database (MicroSoft Access)
###Observations were input in to ARCGIS And merged with their site and their zone
###elevation was pulled from a raster DEM compiled from data acquired from YukonGov clearinghosue
###Using ArcGIS




##############################################START~!~!#####
library(readxl)
library(tidyverse)
library(vegan)
setwd("~/R/Yukon_Birds")
pipe <- "~/R/Yukon_Birds/1_Pipe"

# read the CSV remove
# Make everything capitalized in the loc_name section
# remove the Elev column, it'll be redundant later...

YK_HO_PC <- read.csv("0_Data/Human_Observer_Data.csv")
YK_HO_PC$loc_name <- toupper(YK_HO_PC$loc_name)
names(YK_HO_PC)[names(YK_HO_PC)=="loc_name"] <- "location" 

YK_HO_PC$Elev <- NULL

# read the CSV with elevation and GPS data


YK_Locations <- read_csv("0_Data/YK_Env.csv")

YK_Locations$location <- toupper(YK_Locations$location)


#get rid of "incidental" observations - I only want standardized surveys in my data.

YK_HO_PC <- YK_HO_PC %>% filter(data_type != "incidental")

#get rid of non-functioning columns.

YK_HO_PC$timestamp <- NULL
YK_HO_PC$temp_end <- NULL
YK_HO_PC$temp_start <- NULL
YK_HO_PC$data_source <- NULL
YK_HO_PC$direction  <- NULL
YK_HO_PC$distance.m. <- NULL
YK_HO_PC$data_type <- NULL
YK_HO_PC$longitude <- NULL
YK_HO_PC$latitude <- NULL

names(YK_HO_PC)[names(YK_HO_PC)=="species_id"] <- "species_code" 



YK_HO_PC$location <- gsub(" - ","_",x = YK_HO_PC$location)

#Create new columns for each Site, ZOne, Station Number. Reorganize again.

YK_HO_PC <- separate(YK_HO_PC, 
                     location,
                     c("site", "zone"), 
                     sep = "_", 
                     FALSE, 
                     FALSE, 
                     "warn")

YK_HO_PC <- separate(YK_HO_PC, zone, c("zone", "stat_num"), sep = 1, FALSE, FALSE, "warn")


#all survey types are now "Human observer", and we don't need to worry
#because we got rid of all of the "Incedentals"

YK_HO_PC$survey_type <- "HO"

#Create a column for time and date.

YK_HO_PC$time_start <- paste(YK_HO_PC$hour_start,":",YK_HO_PC$minute_start)
YK_HO_PC$time_start <- gsub(pattern = " : ",
                            replacement = ":",
                            YK_HO_PC$time_start)


YK_HO_PC$recording_date <- paste(YK_HO_PC$year,'_'
                                 ,YK_HO_PC$month,'_',YK_HO_PC$day)

YK_HO_PC$recording_date <- gsub(pattern = " _ ",
                            replacement = "_",
                            YK_HO_PC$recording_date)


#make a survey key column.

YK_HO_PC$surv_key <- paste(YK_HO_PC$site,"_",
                           YK_HO_PC$zone,YK_HO_PC$stat_num,"_",
                           YK_HO_PC$recording_date,"_",
                           YK_HO_PC$time_start,"_",
                           YK_HO_PC$survey_type)

YK_HO_PC$surv_key <- gsub(pattern = " _ ",
                                replacement = "_",
                                YK_HO_PC$surv_key)

YK_HO_PC$surv_key <- gsub(pattern = " ",
                          replacement = "",
                          YK_HO_PC$surv_key)


#####REMOVE ANY UNNEEDED OBSERVATIONS!#####

ALL_HO_Species <- unique(YK_HO_PC$species_code)

# A single obesrvation of "Sooty backed fox sparrow" as opposed to just "Fox Sparrow"

YK_HO_PC$species_code[YK_HO_PC$species_code == "FSPS"] = "FOSP" 

#Remove counts where nothing was observed.
YK_HO_PC = YK_HO_PC %>% filter(species_code != "NOOO")

#Non bird species removed.
YK_HO_PC = YK_HO_PC %>% filter(species_code != "SPPA") #Arctic Ground Squirrel
YK_HO_PC = YK_HO_PC %>% filter(species_code != "ALAM") #MOOSE
YK_HO_PC = YK_HO_PC %>% filter(species_code != "MACA") #Hoary Marmot
YK_HO_PC = YK_HO_PC %>% filter(species_code != "TAHU") #Red Squirrel
YK_HO_PC = YK_HO_PC %>% filter(species_code != "URAR") #Grizzly Bear
YK_HO_PC = YK_HO_PC %>% filter(species_code != "OCCO") #Collared PIKA

#Uncertain bird species removed.
YK_HO_PC = YK_HO_PC %>% filter(species_code != "UNKN") #UNKN BIRD
YK_HO_PC = YK_HO_PC %>% filter(species_code != "CTHR") #UNKN Carthus thrush
YK_HO_PC = YK_HO_PC %>% filter(species_code != "FINC") #UNKN Finch
YK_HO_PC = YK_HO_PC %>% filter(species_code != "FLYC") #UNKN Flycatcher
YK_HO_PC = YK_HO_PC %>% filter(species_code != "PASS") #Passerine Sp.
YK_HO_PC = YK_HO_PC %>% filter(species_code != "SFAL") #Small Falcon
YK_HO_PC = YK_HO_PC %>% filter(species_code != "SPAR") #Sparrow SP.
YK_HO_PC = YK_HO_PC %>% filter(species_code != "THRU") #Thrush Sp.
YK_HO_PC = YK_HO_PC %>% filter(species_code != "WARB") #Warbler Sp.
YK_HO_PC = YK_HO_PC %>% filter(species_code != "WARB") #Woodpecker Sp.

ALL_HO_Species <- unique(YK_HO_PC$species_code)

#write.csv

write.csv(YK_HO_PC,"0_Data/Raw_YK_HO_Data_Tidy.csv")

###########Make it Wide Form Data.######

names(YK_HO_PC)

HO_PC_Joiner <-  YK_HO_PC[c("surv_key","location","site","zone","stat_num")]

HO_PC_Joiner <- distinct(HO_PC_Joiner)

WF_HO_PC <- tapply(YK_HO_PC$count,list(YK_HO_PC$surv_key, YK_HO_PC$species_code), sum, na.rm=FALSE)
WF_HO_PC <- as.data.frame(WF_HO_PC)
WF_HO_PC[is.na(WF_HO_PC)] <- 0

WF_HO_PC <- rownames_to_column(WF_HO_PC, "surv_key")

WF_HO_PC <- left_join(WF_HO_PC,HO_PC_Joiner, "surv_key")

rm(HO_PC_Joiner)


WF_HO_PC <- column_to_rownames(WF_HO_PC,"surv_key")

WF_HO_PC$Site_Zone <- paste(WF_HO_PC$site,'_',WF_HO_PC$zone)

WF_HO_PC$Site_Zone <- gsub(" _ ","_",WF_HO_PC$Site_Zone)

#Write.csv
write.csv(WF_HO_PC,"0_Data/WF_HO_PC.csv")
