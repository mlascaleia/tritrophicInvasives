# initialize ####

source("analysis/00_makeCaterpillars.R")

library(glmmTMB)

# eliminate unidentified and invasive caterpillars
cc <- cc[!cc$catSpecies %in% c("GEOMXX", "MIRCOX", "NOCTXX", "UNKNXX", "CORYME"), ]

# make caterpillar weights dataframe ####

# mark how host was changed
cc$changed <- as.integer(!cc$treeSpecies == cc$newHost)
cc$changeDir <- "unchanged"
cc$changeDir[cc$hostNative %in% "native" & 
               cc$newHostNative %in% "exotic"] <- "nTOe"
cc$changeDir[cc$hostNative %in% "exotic" & 
               cc$newHostNative %in% "native"] <- "eTOn"
cc$changeDir[cc$hostNative %in% "native" & 
               cc$newHostNative %in% "native" &
               cc$changed == 1] <- "nTOn"
cc$changeDir[cc$hostNative %in% "exotic" & 
               cc$newHostNative %in% "exotic" &
               cc$changed == 1] <- "eTOe"

# fix data entry errors (usually decimal point errors)

cc$initialWeight[cc$catNum %in% c("J2616")] <- 
  cc$initialWeight[cc$catNum %in% c("J2616")] * 10

cc$initialWeight[cc$catNum %in% c("J2033", "J2071", "J3183")] <- 
  cc$initialWeight[cc$catNum %in% c("J2033", "J2071", "J3183")]/10

cc$finalWeight[cc$catNum %in% c("1902", "1837")] <- 
  cc$finalWeight[cc$catNum %in% c("1902", "1837")] * 10

cc$frassWeight[cc$catNum %in% c("2157", "1618")] <- 
  cc$frassWeight[cc$catNum %in% c("2157", "1618")] * 10

cc$weirdFinal[cc$catNum %in% c("J2016", "J3218")] <- "DEAD"
cc$weirdFinal[cc$catNum %in% "K4230"] <- "PP"


# eliminate weirdfinals and unwieghed
geCats <- cc[cc$weirdFinal %in% "" & !is.na(cc$finalWeight),]

# little cats are also ending up as weird outliers

geCats <- geCats[geCats$initialWeight > 0.002, ]

# eliminate ones that were really too small to start
geCats <- geCats[geCats$initialWeight > 0.001, ]

# make weight change
geCats$wtChange <- geCats$finalWeight - geCats$initialWeight 

# make weight change percent
geCats$wtChangePer <- geCats$wtChange/geCats$initialWeight

# make growth efficiency

geCats$ge <- geCats$wtChange/geCats$frassWeight
geCats$ge[geCats$ge < 0] <- 0
# geCats[geCats$wtChangePer < 0, ]
geValid <- geCats[geCats$wtChange > 0, ]

geUnchanged <- geCats[geCats$changed == 0, ]










