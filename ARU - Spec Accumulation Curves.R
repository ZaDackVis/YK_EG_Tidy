##############################
##ARU DATA SPEC ACCUM CURVES##
##############################

#Did we survey each location enough to conclude that all species which 
#were available were indeed sampled?
##More probably needs to; be done to make these  legit.
library(readxl)
library(tidyverse)
library(vegan)
library(SpadeR)


setwd("~/R/Yukon_Birds")

WF_ARU_PC <- read.csv("WF_ARU_PC.csv")

WF_ARU_PC <- as.data.frame(WF_ARU_PC)

WF_ARU_PC <- column_to_rownames(WF_ARU_PC, "X")

##ALL SITES AGGREGATED THEN SPLIT BY ZONE

Alpine <- subset(WF_ARU_PC,zone == "A")
SubAlp <- subset(WF_ARU_PC,zone == "S")
Boreal <- subset(WF_ARU_PC,zone == "B")


#Alpine
Alpine_Curve <- specaccum(Alpine[1:55])
plot(Alpine_Curve)

#SubAlpine
SubAlp_Curve <- specaccum(SubAlp[1:55])
plot(SubAlp_Curve, add = TRUE)

#Boreal
Boreal_Curve <- specaccum(Boreal[1:55])
plot(Boreal_Curve)


##All At Once##

plot(Alpine_Curve,col = "red",
     xlim=c(0,200),
     ylim=c(0,60),
     ylab = "species",
     xlab = "samples",
     main = "Species Accumulation Curves",
     sub = "All Sites By Zone")
plot(SubAlp_Curve,col = "blue", add = TRUE)
plot(Boreal_Curve, col = "green", add = TRUE)
legend(100, 20, legend=c("Boreal", "Alpine", "Subalpine"),
       col=c("Green", "Red","blue"), lty=1:2, cex=0.8)


##ALL SITES INDIVIDUALLY, NO SPLIT BY ZONE

CANOL <- subset(WF_ARU_PC,site == "CANOL")
CARIB <- subset(WF_ARU_PC,site == "CARIB")
DANE <- subset(WF_ARU_PC,site == "DANE")
DEADM <- subset(WF_ARU_PC,site == "DEADM")
FRASE <- subset(WF_ARU_PC,site == "FRASE")
GLAVE <- subset(WF_ARU_PC,site == "GLAVE")
KUSAW <- subset(WF_ARU_PC,site == "KUSAW")
LORNE <- subset(WF_ARU_PC,site == "LORNE")
MACIN <- subset(WF_ARU_PC,site == "MACIN")
PARTON <- subset(WF_ARU_PC,site == "PARTO")
QUILL <- subset(WF_ARU_PC,site == "QUILL")
ROCK <- subset(WF_ARU_PC,site == "ROCK")
SUMAN <- subset(WF_ARU_PC,site == "SUMAN")



CANOL_C <- specaccum(CANOL[1:55])
CARIB_C <- specaccum(CARIB[1:55])
DANE_C <- specaccum(DANE[1:55])
DEADM_C <- specaccum(DEADM[1:55])
FRASE_C <- specaccum(FRASE[1:55])
GLAVE_C <- specaccum(GLAVE[1:55])
KUSAW_C <- specaccum(KUSAW[1:55])
LORNE_C <- specaccum(LORNE[1:55])
MACIN_C <- specaccum(MACIN[1:55])
PARTON_C <- specaccum(PARTON[1:55])
QUILL_C <- specaccum(QUILL[1:55])
ROCK_C <- specaccum(ROCK[1:55])
SUMAN_C <- specaccum(SUMAN[1:55])

plot(CANOL_C,ci = 0,col = "red",
     ylim=c(0,40),
     xlim=c(0,59),
     xlab="samples",
     ylab= "N species", 
     main = "All Mountains")

plot(PARTON_C, 
     col = "orange",
     ci = 0, add = TRUE)

plot(SUMAN_C,
     col = "yellow",
     ci = 0, add = TRUE)

plot(CARIB_C,ci = 0,
     col = "green" ,
     add = TRUE)

plot(DEADM_C,
     ci = 0,
     col = "blue",
     add = TRUE)

plot(DANE_C,
     ci = 0, 
     col = "purple", 
     add = TRUE)

plot(FRASE_C,ci = 0,
     col = "hotpink", 
     add = TRUE)

plot(KUSAW_C,
     ci = 0, 
     col = "firebrick",
     add = TRUE)

plot(LORNE_C,ci = 0, 
     col = "darkmagenta",
     add = TRUE)

plot(MACIN_C,
     ci = 0, 
     col = "khaki4",
     add = TRUE)

plot(QUILL_C,
     ci = 0, 
     col = "springgreen4",
     add = TRUE)

plot(ROCK_C,
     ci = 0, 
     col = "gray28",
     add = TRUE)

plot(SUMAN_C,
     ci = 0, 
     col = "slateblue3",
     add = TRUE)

plot(GLAVE_C,
     ci = 0, 
     add = TRUE)


##Rarefaction GLAVE
##Looks fairly well sampled!

GLAVE <- subset(WF_ARU_PC, site == "GLAVE")

GLAVE_A <- subset(GLAVE,zone == "A")
GLAVE_S <- subset(GLAVE,zone == "S")
GLAVE_B <- subset(GLAVE,zone == "B")

GLAVE_C <- specaccum(GLAVE[1:55])
GAC <- specaccum(GLAVE_A[1:55])
GSC <- specaccum(GLAVE_S[1:55])
GBC <- specaccum(GLAVE_B[1:55])

plot(GAC, col = "red", ylim=c(0,35), xlim=c(0,58))
     
plot(GLAVE_C, add = TRUE)

plot(GSC, col = "blue", add = TRUE)
plot(GBC, col = "green", add = TRUE)


## curve SUMAN


SUMAN <- subset(WF_ARU_PC, site == "SUMAN")


SUMAN_A <- subset(SUMAN,zone == "A")
SUMAN_S <- subset(SUMAN,zone == "S")
SUMAN_B <- subset(SUMAN,zone == "B")


SUMAN_A_C <- specaccum(SUMAN_A[1:55])
SUMAN_S_C <- specaccum(SUMAN_S[1:55])
SUMAN_B_C <- specaccum(SUMAN_B[1:55])
SUMAN_C <- specaccum(SUMAN[1:55])


plot(SUMAN_A_C, col = "red", ylim=c(0,25), xlim=c(0,60), ci = 0)

plot(SUMAN_S_C, col = "blue", ci = 0, add = TRUE)
plot(SUMAN_B_C, col = "purple", ci = 0, add = TRUE)



##--- Looks like ALPINE ZONE undersampled
#But overall looks well sampled?

plot(SUMAN_C, ci = 0, add = TRUE)


##-- CURVES FOR CARIBOU -- LOOKS UNDERSAMPLED?


CARIB <- subset(WF_ARU_PC, site == "CARIB")


CARIB_A <- subset(CARIB,zone == "A")
CARIB_S <- subset(CARIB,zone == "S")
CARIB_B <- subset(CARIB,zone == "B")

CARIB_A_C <- specaccum(CARIB_A[1:55])
CARIB_S_C <- specaccum(CARIB_S[1:55])
CARIB_B_C <- specaccum(CARIB_B[1:55])
CARIB_C <- specaccum(CARIB[1:55])


plot(CARIB_A_C, col = "red", ylim=c(0,35), xlim=c(0,25))
plot(CARIB_S_C, col = "blue", add = TRUE)
plot(CARIB_B_C, col = "green", add = TRUE)

legend(100, 20, legend=c("Boreal", "Alpine", "Subalpine"),
       col=c("Green", "Red","blue"), lty=1:2, cex=0.8)









#######ALL ALPINE BY ZONE############
WF_ARU_PC$Site_Zone <- paste(WF_ARU_PC$site,sep="_",WF_ARU_PC$zone)


CANOL <- subset(WF_ARU_PC,Site_Zone == "CANOL_A")
CARIB <- subset(WF_ARU_PC,Site_Zone == "CARIB_A")
DANE <- subset(WF_ARU_PC,Site_Zone == "DANE_A")
DEADM <- subset(WF_ARU_PC,Site_Zone == "DEADM_A")
FRASE <- subset(WF_ARU_PC,Site_Zone == "FRASE_A")
GLAVE <- subset(WF_ARU_PC,Site_Zone == "GLAVE_A")
KUSAW <- subset(WF_ARU_PC,Site_Zone == "KUSAW_A")
LORNE <- subset(WF_ARU_PC,Site_Zone == "LORNE_A")
MACIN <- subset(WF_ARU_PC,Site_Zone == "MACIN_A")
PARTON <- subset(WF_ARU_PC,Site_Zone == "PARTO_A")
QUILL <- subset(WF_ARU_PC,Site_Zone == "QUILL_A")
ROCK <- subset(WF_ARU_PC,Site_Zone == "ROCK_A")
SUMAN <- subset(WF_ARU_PC,Site_Zone == "SUMAN_A")


CANOL_C <- specaccum(CANOL[1:55])
CARIB_C <- specaccum(CARIB[1:55])
DANE_C <- specaccum(DANE[1:55])
DEADM_C <- specaccum(DEADM[1:55])
FRASE_C <- specaccum(FRASE[1:55])
GLAVE_C <- specaccum(GLAVE[1:55])
KUSAW_C <- specaccum(KUSAW[1:55])
LORNE_C <- specaccum(LORNE[1:55])
MACIN_C <- specaccum(MACIN[1:55])
PARTON_C <- specaccum(PARTON[1:55])
QUILL_C <- specaccum(QUILL[1:55])
ROCK_C <- specaccum(ROCK[1:55])
SUMAN_C <- specaccum(SUMAN[1:55])

plot(CANOL_C,ci = 0,col = "red",
     ylim=c(0,25),
     xlim=c(0,40),
     xlab="samples",
     ylab= "N species", 
     main = "All Mountains' Alpine")

plot(PARTON_C, 
     col = "orange",
     ci = 0, add = TRUE)

plot(SUMAN_C,
     col = "yellow",
     ci = 0, add = TRUE)

plot(CARIB_C,ci = 0,
     col = "green" ,
     add = TRUE)

plot(DEADM_C,
     ci = 0,
     col = "blue",
     add = TRUE)

plot(DANE_C,
     ci = 0, 
     col = "purple", 
     add = TRUE)

plot(FRASE_C,ci = 0,
     col = "hotpink", 
     add = TRUE)

plot(KUSAW_C,
     ci = 0, 
     col = "firebrick",
     add = TRUE)

plot(LORNE_C,ci = 0, 
     col = "darkmagenta",
     add = TRUE)

plot(MACIN_C,
     ci = 0, 
     col = "khaki4",
     add = TRUE)

plot(QUILL_C,
     ci = 0, 
     col = "springgreen4",
     add = TRUE)

plot(ROCK_C,
     ci = 0, 
     col = "gray28",
     add = TRUE)

plot(SUMAN_C,
     ci = 0, 
     col = "slateblue3",
     add = TRUE)

plot(GLAVE_C,
     ci = 0, 
     add = TRUE)

#######ALL SubAlp BY ZONE############
WF_ARU_PC$Site_Zone <- paste(WF_ARU_PC$site,sep="_",WF_ARU_PC$zone)


CANOL <- subset(WF_ARU_PC,Site_Zone == "CANOL_S")
CARIB <- subset(WF_ARU_PC,Site_Zone == "CARIB_S")
DANE <- subset(WF_ARU_PC,Site_Zone == "DANE_S")
DEADM <- subset(WF_ARU_PC,Site_Zone == "DEADM_S")
FRASE <- subset(WF_ARU_PC,Site_Zone == "FRASE_S")
GLAVE <- subset(WF_ARU_PC,Site_Zone == "GLAVE_S")
KUSAW <- subset(WF_ARU_PC,Site_Zone == "KUSAW_S")
LORNE <- subset(WF_ARU_PC,Site_Zone == "LORNE_S")
MACIN <- subset(WF_ARU_PC,Site_Zone == "MACIN_S")
PARTON <- subset(WF_ARU_PC,Site_Zone == "PARTO_S")
QUILL <- subset(WF_ARU_PC,Site_Zone == "QUILL_S")
ROCK <- subset(WF_ARU_PC,Site_Zone == "ROCK_S")
SUMAN <- subset(WF_ARU_PC,Site_Zone == "SUMAN_S")


CANOL_C <- specaccum(CANOL[1:55])
CARIB_C <- specaccum(CARIB[1:55])
DANE_C <- specaccum(DANE[1:55])
DEADM_C <- specaccum(DEADM[1:55])
FRASE_C <- specaccum(FRASE[1:55])
GLAVE_C <- specaccum(GLAVE[1:55])
KUSAW_C <- specaccum(KUSAW[1:55])
LORNE_C <- specaccum(LORNE[1:55])
MACIN_C <- specaccum(MACIN[1:55])
PARTON_C <- specaccum(PARTON[1:55])
QUILL_C <- specaccum(QUILL[1:55])
ROCK_C <- specaccum(ROCK[1:55])
SUMAN_C <- specaccum(SUMAN[1:55])

plot(CANOL_C,ci = 0,col = "red",
     ylim=c(0,25),
     xlim=c(0,40),
     xlab="samples",
     ylab= "N species", 
     main = "All Mountains, SubAlp")

plot(PARTON_C, 
     col = "orange",
     ci = 0, add = TRUE)

plot(SUMAN_C,
     col = "yellow",
     ci = 0, add = TRUE)

plot(CARIB_C,ci = 0,
     col = "green" ,
     add = TRUE)

plot(DEADM_C,
     ci = 0,
     col = "blue",
     add = TRUE)

plot(DANE_C,
     ci = 0, 
     col = "purple", 
     add = TRUE)

plot(FRASE_C,ci = 0,
     col = "hotpink", 
     add = TRUE)

plot(KUSAW_C,
     ci = 0, 
     col = "firebrick",
     add = TRUE)

plot(LORNE_C,ci = 0, 
     col = "darkmagenta",
     add = TRUE)

plot(MACIN_C,
     ci = 0, 
     col = "khaki4",
     add = TRUE)

plot(QUILL_C,
     ci = 0, 
     col = "springgreen4",
     add = TRUE)

plot(ROCK_C,
     ci = 0, 
     col = "gray28",
     add = TRUE)

plot(SUMAN_C,
     ci = 0, 
     col = "slateblue3",
     add = TRUE)

plot(GLAVE_C,
     ci = 0, 
     add = TRUE)


#######ALL Boreal BY ZONE############
WF_ARU_PC$Site_Zone <- paste(WF_ARU_PC$site,sep="_",WF_ARU_PC$zone)


CANOL <- subset(WF_ARU_PC,Site_Zone == "CANOL_B")
CARIB <- subset(WF_ARU_PC,Site_Zone == "CARIB_B")
DANE <- subset(WF_ARU_PC,Site_Zone == "DANE_B")
DEADM <- subset(WF_ARU_PC,Site_Zone == "DEADM_B")
FRASE <- subset(WF_ARU_PC,Site_Zone == "FRASE_B")
GLAVE <- subset(WF_ARU_PC,Site_Zone == "GLAVE_B")
KUSAW <- subset(WF_ARU_PC,Site_Zone == "KUSAW_B")
LORNE <- subset(WF_ARU_PC,Site_Zone == "LORNE_B")
MACIN <- subset(WF_ARU_PC,Site_Zone == "MACIN_B")
PARTON <- subset(WF_ARU_PC,Site_Zone == "PARTO_B")
QUILL <- subset(WF_ARU_PC,Site_Zone == "QUILL_B")
ROCK <- subset(WF_ARU_PC,Site_Zone == "ROCK_B")
SUMAN <- subset(WF_ARU_PC,Site_Zone == "SUMAN_B")


CANOL_C <- specaccum(CANOL[1:55])
CARIB_C <- specaccum(CARIB[1:55])
DANE_C <- specaccum(DANE[1:55])
DEADM_C <- specaccum(DEADM[1:55])
FRASE_C <- specaccum(FRASE[1:55])
GLAVE_C <- specaccum(GLAVE[1:55])
KUSAW_C <- specaccum(KUSAW[1:55])
LORNE_C <- specaccum(LORNE[1:55])
MACIN_C <- specaccum(MACIN[1:55])
PARTON_C <- specaccum(PARTON[1:55])
QUILL_C <- specaccum(QUILL[1:55])
ROCK_C <- specaccum(ROCK[1:55])
SUMAN_C <- specaccum(SUMAN[1:55])

plot(CANOL_C,ci = 0,col = "red",
     ylim=c(0,25),
     xlim=c(0,40),
     xlab="samples",
     ylab= "N species", 
     main = "All Mountains, SubAlp")

plot(PARTON_C, 
     col = "orange",
     ci = 0, add = TRUE)

plot(SUMAN_C,
     col = "yellow",
     ci = 0, add = TRUE)

plot(CARIB_C,ci = 0,
     col = "green" ,
     add = TRUE)

plot(DEADM_C,
     ci = 0,
     col = "blue",
     add = TRUE)

plot(DANE_C,
     ci = 0, 
     col = "purple", 
     add = TRUE)

plot(FRASE_C,ci = 0,
     col = "hotpink", 
     add = TRUE)

plot(KUSAW_C,
     ci = 0, 
     col = "firebrick",
     add = TRUE)

plot(LORNE_C,ci = 0, 
     col = "darkmagenta",
     add = TRUE)

plot(MACIN_C,
     ci = 0, 
     col = "khaki4",
     add = TRUE)

plot(QUILL_C,
     ci = 0, 
     col = "springgreen4",
     add = TRUE)

plot(ROCK_C,
     ci = 0, 
     col = "gray28",
     add = TRUE)

plot(SUMAN_C,
     ci = 0, 
     col = "slateblue3",
     add = TRUE)

plot(GLAVE_C,
     ci = 0, 
     add = TRUE)
