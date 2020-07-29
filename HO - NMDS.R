
########################NMDS on SUMED RAW ABUNDANCE BY ZONE, No transform#####

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

##########################################PLOT THAT NMDS####


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
