############################
######MASTER DATA FILE!########
############################

###The goal of this script is to compile and tidy ALL THE DATA!

#this is all the necessary code to do the Human Observer analysis, ARU analysis
#and then both combined

library(readxl)
library(tidyverse)
library(vegan)
setwd("~/R/Yukon_Birds")
pipe <- "~/R/Yukon_Birds/1_Pipe"

##########################ARU ANALYSIS ONLY########################
#####Start here: Importing data####
YK_ARU_PC <- read_excel("0_Data/EG_YK_For_R.xlsx")
YK_ARU_PC <- subset(YK_ARU_PC, transcriber == 'jf_jet@hotmail.com',)
YK_ARU_PC$transcriber <- NULL
YK_ARU_PC$is_buffered_location <- NULL

#seperate the recording time from recording date (redundant column).
#Remove the "status" column, as they've all bee completed.

YK_ARU_PC <- separate(YK_ARU_PC, recording_time, c("remove", "R_time"), sep = " ", TRUE, FALSE, "warn")
YK_ARU_PC$remove <- NULL
YK_ARU_PC$status <- NULL 

#Our sampling scheme only sampled for 3 minutes of ARU data. 
#Therefore remove all columns related to 4minutes or more.

YK_ARU_PC <- YK_ARU_PC[-c(30:41)]

#I also don't like that the "Project Name" and "data_set are listed first. 
#These are important variables to keep, as they may be used with other projects late on, 
#but for now let's move them to the far right of our array, so they're less noticable.

YK_ARU_PC <- YK_ARU_PC[c(3:31,1:2)]

#Read the Location with Elevation CSV. Make the names of the sites all caps.

YK_Locations <- read_csv("0_Data/All_Locations_With_Topographic_and_Climate.csv")

YK_Locations$location <- as.character(YK_Locations$location) 
YK_Locations$location <- toupper(YK_Locations$location)


##Create a column that can be used to match YK_Locations to the ARU_PC file

YK_ARU_PC$site <- as.factor(YK_ARU_PC$site) 
YK_ARU_PC$station <- as.factor(YK_ARU_PC$station)
YK_ARU_PC$location <- paste(YK_ARU_PC$site,"-",YK_ARU_PC$station)
YK_ARU_PC <- YK_ARU_PC[c(32,1:31)]
YK_ARU_PC$latitude <- NULL
YK_ARU_PC$longitude <- NULL
YK_ARU_PC <- YK_ARU_PC %>% left_join(YK_Locations,"location")
YK_ARU_PC <- YK_ARU_PC[c(1:3,34,4:33)]

#I need to be able to reference all surveys individually, so create a survey key.

YK_ARU_PC$survey_type <- "ARU"
YK_ARU_PC$surv_key <- paste(YK_ARU_PC$site,"_",YK_ARU_PC$station,"_",YK_ARU_PC$recording_date,"_",YK_ARU_PC$R_time,"_",YK_ARU_PC$survey_type)

##Split the location in to "zone"
YK_ARU_PC <- separate(YK_ARU_PC, station, c("zone", "stat_num"), sep = 1, TRUE, FALSE, "warn")


#start sorting through the data to get rid of anything I don't want... Bad weather, observations of wind/noise

#Make sure all data is actually completed...
YK_ARU_PC <- subset(YK_ARU_PC, status == 'Transcribed')

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

YK_ARU_PC = YK_ARU_PC %>% filter(species_code != "GCRF")


#Data from Long Form to Wide Form#################################

YK_ARU_PC$counter <- 1
WF_ARU_PC <- tapply(YK_ARU_PC$counter,list(YK_ARU_PC$surv_key, YK_ARU_PC$species_code), sum, na.rm=TRUE)
YK_ARU_PC$counter <- NULL
WF_ARU_PC[is.na(WF_ARU_PC)] <- 0
WF_ARU_PC <- as.data.frame(WF_ARU_PC)
WF_ARU_PC <- rownames_to_column(WF_ARU_PC,"surv_key")

#create an object with all of the survey's metadata
For_binding <- YK_ARU_PC[-c(6:32)]
For_binding <- distinct(For_binding)

#Bind the metadata to the wideform dataset.

WF_ARU_PC <- left_join(WF_ARU_PC,For_binding)
rm(For_binding)

#rearrange it a bit so it's easier to read...
WF_ARU_PC <- WF_ARU_PC[c(1,60:63,2:59,64,67)]
WF_ARU_PC <- column_to_rownames(WF_ARU_PC, "surv_key")

#Create a Site_Zone column to base the NMDS off of.
WF_ARU_PC$Site_Zone <- paste(WF_ARU_PC$site,"-",WF_ARU_PC$zone)

#####NMDS formatting####

rm(YK_ARU_PC)
rm(YK_Locations)



#ALL_Sites: aggregate all point-count locations by zone, sum raw abundance...

ALL_ARU_by_Zone <- WF_ARU_PC %>% 
  group_by(WF_ARU_PC$Site_Zone) %>% 
  summarise_at(c(1:56), sum, na.rm = FALSE)


names(ALL_ARU_by_Zone)[names(ALL_ARU_by_Zone)=="WF_ARU_PC$Site_Zone"] <- "Site_Zone"
ALL_ARU_by_Zone <- separate(ALL_ARU_by_Zone, Site_Zone, 
                            c("Site", "Zone"), 
                            sep = "-", FALSE, FALSE, "warn")


ALL_ARU_by_Zone <- column_to_rownames(ALL_ARU_by_Zone,"Site_Zone")


#Run the nmds
ALL_ARU_by_Zone_MDS <- metaMDS(ALL_ARU_by_Zone[3:58],"bray",k = 2, autotransform = FALSE)


#Using the scores function from vegan to extract the site scores and convert to a data.frame
ARU_MDS_scores <- as.data.frame(scores(ALL_ARU_by_Zone_MDS))  

# create a column of site names, from the rownames of ARU_MDS_scores
ARU_MDS_scores$site <- rownames(ARU_MDS_scores)  
ARU_MDS_scores <- separate(ARU_MDS_scores, site, 
                           c("site", "grp"), 
                           sep = " - ", FALSE, FALSE, "warn")


#######plot the NMDS######

#####Sick topographical overlay?
#Get the elevation data to be paired with its Site_Zone

WF_ARU_PC<- rownames_to_column(WF_ARU_PC, "surv_key")
elev <- WF_ARU_PC
elev <- elev[c('Site_Zone','Elev')]
elev <- distinct(elev)

elev <- elev %>% group_by(elev$Site_Zone) %>% 
  summarise_at(c('Elev'), mean, na.rm = FALSE)

names(elev)[names(elev)=="elev$Site_Zone"] <- "Site_Zone" 

ARU_MDS_scores <- rownames_to_column(ARU_MDS_scores, "Site_Zone")

ARU_MDS_scores <- left_join(ARU_MDS_scores, elev, by = "Site_Zone")


plot(ALL_ARU_by_Zone_MDS,type="n")

ordipointlabel(ALL_ARU_by_Zone_MDS,
               display="sites",
               cex=.5)


ordisurf(ARU_MDS_scores[c("NMDS1","NMDS2")],ARU_MDS_scores$Elev, add = TRUE)

#Put the spiders on it.
ordispider(ALL_ARU_by_Zone_MDS,
           groups = ALL_ARU_by_Zone$Zone, 
           col=1:3,
           lwd=3, add = TRUE)


#95% confindence intervals
ordiellipse(ALL_ARU_by_Zone_MDS,
            groups = ALL_ARU_by_Zone$Zone, 
            col=1:3,
            lwd=3, add = TRUE)





#####Are the Ecoregions different? ####

#####NMDS by ecoregion####
ALL_ARU_by_ER <- WF_ARU_PC

ALL_ARU_by_ER <- column_to_rownames(ALL_ARU_by_ER, "surv_key")

ALL_ARU_by_ER$ECOREG_EN <- as.factor(ALL_ARU_by_ER$ECOREG_EN)

ALL_ARU_by_ER <- ALL_ARU_by_ER %>% group_by(ALL_ARU_by_ER$ECOREG_EN) %>% summarise_at(c(1:56), sum, na.rm = FALSE)

names(ALL_ARU_by_ER)[names(ALL_ARU_by_ER)=="ALL_ARU_by_ER$ECOREG_EN"] <- "ecoreg" 

ALL_ARU_by_ER <- column_to_rownames(ALL_ARU_by_ER,"ecoreg")

ALL_ARU_by_ER_MDS <- metaMDS(ALL_ARU_by_ER[1:56],"bray",k = 2, autotransform = FALSE)



plot(ALL_ARU_by_ER_MDS,type="n")

stressplot(ALL_ARU_by_ER_MDS)

ordipointlabel(ALL_ARU_by_ER_MDS,
               display="sites",
               cex=.5)
#stress 0.0001480479 

#####Should birds be conserved differently by ecoregion Ecoregion_Zone?####

ALL_ARU_by_ZER <- WF_ARU_PC

ALL_ARU_by_ZER <- column_to_rownames(ALL_ARU_by_ZER, "surv_key")

ALL_ARU_by_ZER$ZER <- paste(ALL_ARU_by_ZER$zone,"_",ALL_ARU_by_ZER$ECOREG_EN)

ALL_ARU_by_ZER <- ALL_ARU_by_ZER %>% 
  group_by(ALL_ARU_by_ZER$ZER) %>% 
  summarise_at(c(1:56), sum, na.rm = FALSE)

names(ALL_ARU_by_ZER)[names(ALL_ARU_by_ZER)=="ALL_ARU_by_ZER$ZER"] <- "ZER" 

ALL_ARU_by_ZER <- column_to_rownames(ALL_ARU_by_ZER,"ZER")
#NMDS
ALL_ARU_by_ZER_MDS <- metaMDS(ALL_ARU_by_ZER[1:56],"bray",k = 2, autotransform = FALSE)
#rownames
ALL_ARU_by_ZER <- rownames_to_column(ALL_ARU_by_ZER,"ZER")

ALL_ARU_by_ZER <- separate(ALL_ARU_by_ZER, ZER, 
                           c("Zone", "Ecoregion"), 
                           sep = " _ ", FALSE, FALSE, "warn")

#plot
plot(ALL_ARU_by_ZER_MDS,type="n")

ordipointlabel(ALL_ARU_by_ZER_MDS,
               display="sites",
               cex=.5)

ordipointlabel(ALL_ARU_by_ZER_MDS,
               display="species",
               cex=.5)

ordiellipse(ALL_ARU_by_ZER_MDS,
            groups = ALL_ARU_by_ZER$Zone, 
            col=1:3,
             add = TRUE)

ordispider(ALL_ARU_by_ZER_MDS,
            groups = ALL_ARU_by_ZER$Zone, 
            col=1:3,
            add = TRUE)






##########################HUMAN OBSERVER ONLY########################
#####Start here: importing data########
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
YK_HO_PC$Elev <- NULL

# read the CSV with elevation and GPS data
#Make 'em all caps because why not? Change the names because they need to link...
#Join 'em together...

YK_Locations <- read_csv("0_Data/All_Locations_With_Elevations.csv")

YK_Locations$location <- toupper(YK_Locations$location)

names(YK_Locations)[names(YK_Locations)=="location"] <- "loc_name" 

YK_HO_PC <- left_join(YK_HO_PC, YK_Locations, by = c("loc_name"), keep = TRUE)
rm(YK_Locations)

#####tidying up.####

#rename.
names(YK_HO_PC)[names(YK_HO_PC)=="loc_name"] <- "location" 

#get rid of "incidental" observations - I only want standardized surveys in my data.

YK_HO_PC <- YK_HO_PC %>% filter(data_type != "incidental")

#get rid of non-functioning columns.

YK_HO_PC$Elev.x <- NULL
YK_HO_PC$longitude.x <- NULL
YK_HO_PC$latitude.x <- NULL
YK_HO_PC$timestamp <- NULL
YK_HO_PC$temp_end <- NULL
YK_HO_PC$temp_start <- NULL

#rename columns that are functional, but were redundant.

names(YK_HO_PC)[names(YK_HO_PC)=="Elev.y"] <- "Elev" 
names(YK_HO_PC)[names(YK_HO_PC)=="latitude.y"] <- "latitude" 
names(YK_HO_PC)[names(YK_HO_PC)=="longitude.y"] <- "longitude" 
names(YK_HO_PC)[names(YK_HO_PC)=="species_id"] <- "species_code" 

#reorganize the variables...
YK_HO_PC <- YK_HO_PC[c(1,3:42,2)]

#Create new columns for each Site, ZOne, Station Number. Reorganize again.

YK_HO_PC <- separate(YK_HO_PC, location, c("site", "zone"), sep = "-", FALSE, FALSE, "warn")

YK_HO_PC <- separate(YK_HO_PC, zone, c("zone", "stat_num"), sep = 2, FALSE, FALSE, "warn")

YK_HO_PC <- YK_HO_PC[c(1:2,4:5,3,6:45)]

#all survey types are now "Human observer", and we don't need to worry
#because we got rid of all of the "Incedentals"

YK_HO_PC$survey_type <- "HO"

#Create a column for time and date.

YK_HO_PC$time_start <- paste(YK_HO_PC$hour_start,":",YK_HO_PC$minute_start)
YK_HO_PC$recording_date <- paste(YK_HO_PC$year,'-'
                                 ,YK_HO_PC$month,'-',YK_HO_PC$day)

#make a survey key column.

YK_HO_PC$surv_key <- paste(YK_HO_PC$site,"_",
                           YK_HO_PC$zone,YK_HO_PC$stat_num,"_",
                           YK_HO_PC$recording_date,"_",
                           YK_HO_PC$time_start,"_",
                           YK_HO_PC$survey_type)


# A single obesrvation of "Sooty backed fox sparrow" as opposed to just "Fox Sparrow"

ALL_HO_Species <- as.data.frame(YK_HO_PC$species_code)
ALL_HO_Species <- unique(ALL_HO_Species)

YK_HO_PC$species_code[YK_HO_PC$species_code == "FSPS"] = "FOSP"


ALL_HO_Species <- as.data.frame(YK_HO_PC$species_code)
ALL_HO_Species <- unique(ALL_HO_Species)


#####Make it Wide Form Data.#######

YK_HO_PC$counter <- 1

names(YK_HO_PC)

HO_PC_Joiner <- YK_HO_PC[c(1:8,41:46,48)]
HO_PC_Joiner <- distinct(HO_PC_Joiner)


WF_HO_PC <- tapply(YK_HO_PC$counter,list(YK_HO_PC$surv_key, YK_HO_PC$species_code), sum, na.rm=FALSE)
WF_HO_PC <- as.data.frame(WF_HO_PC)
WF_HO_PC[is.na(WF_HO_PC)] <- 0

WF_HO_PC <- rownames_to_column(WF_HO_PC, "surv_key")

WF_HO_PC <- left_join(WF_HO_PC,HO_PC_Joiner, "surv_key")
rm(HO_PC_Binding)

#####Tidying the WF Dataframe#####

WF_HO_PC$FSPS <- NULL #I'm not sure how this guy got resurected... but it's zeros all the way down.
WF_HO_PC$ALAM <- NULL
WF_HO_PC$UNKN <- NULL
WF_HO_PC$CTHR <- NULL
WF_HO_PC$FINC <- NULL 
WF_HO_PC$FLYC <- NULL
WF_HO_PC$MACA <- NULL
WF_HO_PC$OCCO <- NULL
WF_HO_PC$PASS <- NULL
WF_HO_PC$SFAL <- NULL
WF_HO_PC$SPAR <- NULL
WF_HO_PC$SPPA <- NULL
WF_HO_PC$PASS <- NULL
WF_HO_PC$TAHU <- NULL
WF_HO_PC$THRU <- NULL
WF_HO_PC$URAR <- NULL
WF_HO_PC$WARB <- NULL
WF_HO_PC$WOOD <- NULL

#Get rid of waterfowl specifically?


#Columns to row names.

WF_HO_PC <- column_to_rownames(WF_HO_PC,"surv_key")

#make a Site_Zone variable...
WF_HO_PC$Site_Zone <- paste(WF_HO_PC$site,WF_HO_PC$zone)

#####NMDS on SUMED RAW ABUNDANCE BY ZONE, No transform#####

#Sum all raw abundance from the Sites by Zone up 
ALL_HO_by_Zone <- WF_HO_PC %>% 
  group_by(WF_HO_PC$Site_Zone) %>% 
  summarise_at(c(1:78), sum, na.rm = FALSE)

#tidy it up a smidge, and seperate out two more columns just incase I want to use later.

names(ALL_HO_by_Zone)[names(ALL_HO_by_Zone)=="WF_HO_PC$Site_Zone"] <- "Site_Zone"
ALL_HO_by_Zone <- separate(ALL_HO_by_Zone, Site_Zone, 
                           c("Site", "Zone"), 
                           sep = "   ", FALSE, FALSE, "warn")

ALL_HO_by_Zone <- ALL_HO_by_Zone %>% column_to_rownames("Site_Zone")

#run the NMDS
ALL_HO_MDS <- metaMDS(ALL_HO_by_Zone[3:80], "bray",k = 2, autotransform = FALSE)

#####PLOT THAT NMDS#####

#Plot it basic
plot(ALL_HO_MDS,type="n")
ordipointlabel(ALL_HO_MDS,
               display="sites",
               cex=.5)

ordiellipse(ALL_HO_MDS,
            groups = ALL_HO_by_Zone$Zone, 
            col=1:3,
            lwd=3)

#Put the spiders on it
ordispider(ALL_HO_MDS,
           groups = ALL_HO_by_Zone$Zone, 
           col=1:3,
           lwd=3)

#Topographical map?
HO_elev <- WF_HO_PC

HO_elev <- HO_elev[c(90,93)]
HO_elev <- distinct(HO_elev)

HO_elev <- HO_elev %>% group_by(HO_elev$Site_Zone) %>% 
  summarise_at(c('Elev'), mean, na.rm = FALSE)

names(HO_elev)[names(HO_elev)=="HO_elev$Site_Zone"] <- "Site_Zone" 

HO_MDS_scores <- as.data.frame(scores(ALL_HO_MDS))  

HO_MDS_scores$site <- rownames(HO_MDS_scores)  
HO_MDS_scores <- separate(HO_MDS_scores, site, 
                          c("site", "grp"), 
                          sep = "   ", FALSE, FALSE, "warn")

HO_MDS_scores <- rownames_to_column(HO_MDS_scores,"Site_Zone")


HO_MDS_scores <- left_join(HO_MDS_scores, HO_elev, by = "Site_Zone")


ordisurf(HO_MDS_scores[c("NMDS1","NMDS2")],HO_MDS_scores$Elev, add = TRUE)




#####Are the Ecoregions different? ####

#####NMDS by ecoregion####
ALL_HO_by_ER <- WF_ARU_PC

ALL_HO_by_ER <- column_to_rownames(ALL_HO_by_ER, "surv_key")

ALL_HO_by_ER$ECOREG_EN <- as.factor(ALL_HO_by_ER$ECOREG_EN)

ALL_HO_by_ER <- ALL_HO_by_ER %>% group_by(ALL_HO_by_ER$ECOREG_EN) %>% summarise_at(c(1:56), sum, na.rm = FALSE)

names(ALL_HO_by_ER)[names(ALL_HO_by_ER)=="ALL_HO_by_ER$ECOREG_EN"] <- "ecoreg" 

ALL_HO_by_ER <- column_to_rownames(ALL_HO_by_ER,"ecoreg")

ALL_HO_by_ER_NMDS <- metaMDS(ALL_HO_by_ER[1:56],"bray",k = 2, autotransform = FALSE)



plot(ALL_HO_by_ER_NMDS,type="n")


ordipointlabel(ALL_HO_by_ER_NMDS,
               display="sites",
               cex=.5)

#####NMDS by Ecoregion_Zone!!!####

ALL_HO_by_ZER <- WF_HO_PC

ALL_HO_by_ZER <- column_to_rownames(ALL_HO_by_ZER, "surv_key")

ALL_HO_by_ZER$ZER <- paste(ALL_HO_by_ZER$zone,"_",ALL_HO_by_ZER$ECOREG_EN)

ALL_HO_by_ZER <- ALL_HO_by_ZER %>% 
  group_by(ALL_HO_by_ZER$ZER) %>% 
  summarise_at(c(1:56), sum, na.rm = FALSE)

names(ALL_HO_by_ZER)[names(ALL_HO_by_ZER)=="ALL_HO_by_ZER$ZER"] <- "ZER" 

ALL_HO_by_ZER <- column_to_rownames(ALL_HO_by_ZER,"ZER")
#NMDS
ALL_HO_by_ZER_MDS <- metaMDS(ALL_HO_by_ZER[1:56],"bray",k = 2, autotransform = FALSE)
#rownames
ALL_HO_by_ZER <- rownames_to_column(ALL_HO_by_ZER,"ZER")

ALL_HO_by_ZER <- separate(ALL_HO_by_ZER, ZER, 
                           c("Zone", "Ecoregion"), 
                           sep = " _ ", FALSE, FALSE, "warn")

#plot
plot(ALL_HO_by_ZER_MDS,type="n")

ordipointlabel(ALL_HO_by_ZER_MDS,
               display="sites",
               cex=.5)


ordiellipse(ALL_HO_by_ZER_MDS,
            groups = ALL_HO_by_ZER$Zone, 
            col=1:3)

ordispider(ALL_HO_by_ZER_MDS,
           groups = ALL_HO_by_ZER$Zone, 
           col=1:3)





#####MISC#####
#Presence absence?


ALL_HO_by_Zone <- WF_HO_PC %>% 
  group_by(WF_HO_PC$Site_Zone) %>% 
  summarise_at(c(1:78), sum, na.rm = FALSE)

ALL_HO_by_Zone[ALL_HO_by_Zone > 0]

names(ALL_HO_by_Zone)[names(ALL_HO_by_Zone)=="WF_HO_PC$Site_Zone"] <- "Site_Zone"
ALL_HO_by_Zone <- separate(ALL_HO_by_Zone, Site_Zone, 
                           c("Site", "Zone"), 
                           sep = "   ", FALSE, FALSE, "warn")

ALL_HO_by_Zone <- ALL_HO_by_Zone %>% column_to_rownames("Site_Zone")


ALL_HO_MDS <- metaMDS(ALL_HO_by_Zone[3:80], "bray",k = 2, autotransform = FALSE)

plot(ALL_HO_MDS,type="n")
ordipointlabel(ALL_HO_MDS,
               display="sites",
               cex=.5)


ordiellipse(ALL_HO_MDS,
            groups = ALL_HO_by_Zone$Zone, 
            col=1:3,
            lwd=3)

ordispider(ALL_HO_MDS,
           groups = ALL_HO_by_Zone$Zone, 
           col=1:3,
           lwd=3)


ordipointlabel(ALL_HO_MDS,
               display="species",
               cex=.5)


###Add traits in to the mix!####
Traits <- read.csv("0_Data/Avian_Life_Histories_CSV.csv")

names(Traits)[names(Traits)=="SpeciesID"] <- "species_code"


YK_HO_Traits <- left_join(YK_HO_PC, Traits, by = "species_code")


#####BOTH DATASETS COMBINE####




##Questions####
  # Is it valid to merge all observations? Why or why not?
  # Can we derive any new information by merging all the observations? I.E. will doing so inform our conclusions anymore than if we just looked at humans?

  # 
