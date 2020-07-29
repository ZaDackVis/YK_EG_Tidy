############################
######Tidy_ARU_Data#########
############################

###The goal of this script is to compile and tidy all of the data as needed...

library(readxl)
library(tidyverse)
library(vegan)
setwd("C:/Users/DackZ/Documents/R/Yukon_Birds/0_Data")
pipe <- "~/R/Yukon_Birds/1_Pipe"

#########Long form Data Tidying########

YK_ARU_PC <- read_csv("Raw_YK_ARU_Data_2019.csv")

#JF Was the only person who transcribed the data.

YK_ARU_PC <- subset(YK_ARU_PC, transcriber == 'jf_jet@hotmail.com',)

#seperate the recording time from recording date (redundant column).


YK_ARU_PC <- separate(YK_ARU_PC, recording_time, c( "R_time", "remove"), sep = " ", TRUE, FALSE, "warn")


#get rid of unecessary variables.
YK_ARU_PC$remove <- NULL   #nothing in this colum
YK_ARU_PC$transcriber <- NULL #All are JF
YK_ARU_PC$is_buffered_location <- NULL #all are buffered
YK_ARU_PC$latitude <- NULL #empty and will be filled later.
YK_ARU_PC$longitude <- NULL #empty and will be filled later
YK_ARU_PC$method <- NULL   #All are the same
YK_ARU_PC$confidence <- NULL   #only going to accept high confidence
YK_ARU_PC$T_Id <- NULL          #unecessary.
YK_ARU_PC$min4_voc <- NULL          #surveys only go until 3 minutes.    
YK_ARU_PC$min4_start <- NULL
YK_ARU_PC$min5_voc <- NULL
YK_ARU_PC$min5_start <- NULL
YK_ARU_PC$min6_voc <- NULL
YK_ARU_PC$min6_start <- NULL
YK_ARU_PC$min7_voc <- NULL
YK_ARU_PC$min7_start <- NULL
YK_ARU_PC$min8_voc <- NULL
YK_ARU_PC$min8_start  <- NULL        
YK_ARU_PC$min9_voc     <- NULL         
YK_ARU_PC$min9_start <- NULL

##Create columns that can be used to match YK_Locations to the ARU_PC file
YK_ARU_PC$site <- as.factor(YK_ARU_PC$site) 

YK_ARU_PC$station <- as.factor(YK_ARU_PC$station)

YK_ARU_PC$location <- paste(YK_ARU_PC$site,"_",YK_ARU_PC$station, sep = "")

#Rearrange everything.

YK_ARU_PC <- YK_ARU_PC[c("location",
                         "site",
                         "station",
                         "recording_date",         
                         "R_time",                                              
                         "species_code",
                         "scientific_name",
                         "species_english_name",
                         "rain",
                         "wind",                   
                         "noise", 
                         "industry_noise",
                         "audio_quality" ,         
                         "species_individual_name",
                         "abundance",              
                         "min0_voc",
                         "min0_start",         
                         "min1_voc",
                         "min1_start",
                         "min2_voc",           
                         "min2_start",
                         "min3_voc",
                         "min3_start",           
                         "comment",
                         "species_comment",
                         "project",            
                         "data_set")]

#all of this data is from ARU recordings.
YK_ARU_PC$survey_type <- "ARU"

#Make a master key for each survey

YK_ARU_PC$surv_key <- paste(YK_ARU_PC$site,'_',
                            YK_ARU_PC$station,'_',
                            YK_ARU_PC$recording_date,'_',
                            YK_ARU_PC$R_time,'_',
                            YK_ARU_PC$survey_type,
                            sep = "")

##Split the location in to "zone"
YK_ARU_PC <- separate(YK_ARU_PC,
                      col = "station", 
                      into = c("zone", "stat_num"),
                      sep = 1, 
                      remove = TRUE, 
)

YK_ARU_PC$location  <- as.character(YK_ARU_PC$location)


ARU_Locations <- unique(YK_ARU_PC$location)
ARU_Locations <- as.data.frame(ARU_Locations)

#start sorting through the data to get rid of anything I don't want...

#Make sure all data is actually completed...
YK_ARU_PC <- subset(YK_ARU_PC, status == 'Transcribed')
YK_ARU_PC$status <- NULL

#get rid of observatins of wind, traffic, etc.
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "LIWI")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "LIAI")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "LIRA")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "MOWI")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "LITR")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "MOTR")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "HEWI")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "MOBA")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "MORA")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "HEBA")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "LIBA")


#Remove any mammal/non-bird species
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "WOLF")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNMA")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "COPI")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "RESQ")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "AGSQ")

# get rid of any unknown birds.

YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNWA")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNFI")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNPA")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNKN")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNSP")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNTH")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNTR")
YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "UNBT")

# get rid of observations of "NONE"?

YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "NONE")

# get rid of the single instance of "GCRF"?

#YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "GCRF")


YK_ARU_PC$scientific_name <- gsub(" ","_",YK_ARU_PC$scientific_name)

#write that as a new "Tidy'd" CSV.

write.csv(YK_ARU_PC,
          file = "YK_ARU_PC_Tidy.csv")

rm(YK_ARU_PC)


############ARU Observation Data from Long Form to Wide Form, ALPHA CODES########

#adds a counter to YK_ARU_PC for each observation then sums up the observations 

YK_ARU_PC <- read.csv("YK_ARU_PC_Tidy.csv")
 
YK_ARU_PC$counter <- 1

WF_ARU_Alpha_codes <- tapply(YK_ARU_PC$counter,
                    list(YK_ARU_PC$surv_key, 
                         YK_ARU_PC$species_code)
                    , sum, na.rm=TRUE)
YK_ARU_PC$counter <- NULL
WF_ARU_Alpha_codes[is.na(WF_ARU_Alpha_codes)] <- 0
WF_ARU_Alpha_codes <- as.data.frame(WF_ARU_Alpha_codes)


WF_ARU_Alpha_codes <- rownames_to_column(WF_ARU_Alpha_codes,
                                var = "surv_key")



#Add the smaller level identifiers (location, site, station)

joiner <- YK_ARU_PC[c("surv_key","location","site","zone","stat_num")]
joiner <- unique(joiner)

WF_ARU_Alpha_codes <- left_join(WF_ARU_Alpha_codes,
                       joiner,
                       by = "surv_key")

rm(joiner)

WF_ARU_Alpha_codes <- column_to_rownames(WF_ARU_Alpha_codes,
                                var = "surv_key")

WF_ARU_Alpha_codes$Site_Zone <- paste(WF_ARU_Alpha_codes$site,"_",WF_ARU_Alpha_codes$zone, sep="")


#######Same thing but columns are science names#####

YK_ARU_PC <- read.csv("YK_ARU_PC_Tidy.csv")

YK_ARU_PC$counter <- 1

WF_Sci_name_ARU_PC <- tapply(YK_ARU_PC$counter,
                    list(YK_ARU_PC$surv_key, 
                         YK_ARU_PC$scientific_name)
                    , sum, na.rm=TRUE)

YK_ARU_PC$counter <- NULL

WF_Sci_name_ARU_PC[is.na(WF_Sci_name_ARU_PC)] <- 0

WF_Sci_name_ARU_PC <- as.data.frame(WF_Sci_name_ARU_PC)

WF_Sci_name_ARU_PC <- rownames_to_column(WF_Sci_name_ARU_PC,
                                var = "surv_key")



#Add the smaller level identifiers (location, site, station)

joiner <- YK_ARU_PC[c("surv_key","location","site","zone","stat_num")]
joiner <- unique(joiner)

WF_Sci_name_ARU_PC <- left_join(WF_Sci_name_ARU_PC,
                       joiner,
                       by = "surv_key")

rm(joiner)

WF_Sci_name_ARU_PC <- column_to_rownames(WF_Sci_name_ARU_PC,
                                var = "surv_key")

WF_Sci_name_ARU_PC$Site_Zone <- paste(WF_Sci_name_ARU_PC$site,"_",WF_Sci_name_ARU_PC$zone, sep="")



#write the above as  CSV

write.csv(WF_ARU_PC, "WF_ARU_PC.csv")
write.csv(WF_Sci_name_ARU_PC, "WF_ARU_PC_Sci_Names.csv")
write.csv(WF_ARU_Alpha_codes, "WF_ARU_Alpha.csv")

