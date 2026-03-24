# this script will make the data regarding the growth efficiency assay

# initialize ####

source("analysis/00b_makeCleanerCaterpillarData.R")

# fix data entry errors (decimal point errors)

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

# eliminate weirdfinals and unwieghed, make growth efficiency dataset
geCats <- cc[cc$weirdFinal %in% "" & !is.na(cc$finalWeight),]

# eliminate caterpillars that starved rather than eat
geCats <- geCats[geCats$frassWeight > 0,]

# eliminate caterpillars that were too small to start
geCats <- geCats[geCats$initialWeight > 0.002, ]

# make weight change
geCats$wtChange <- geCats$finalWeight - geCats$initialWeight 

# make weight change percent
geCats$wtChangePer <- geCats$wtChange/geCats$initialWeight

# make growth efficiency
geCats$ge <- geCats$wtChange/geCats$frassWeight

# I initially thought that ge should never be negative, but that was a mistake
# some caterpillars ate their plants and lost weight because of it
# geCats$ge[geCats$ge < 0] <- 0
# geValid <- geCats[geCats$wtChange > 0, ]

# Now make a cleaner cc object where growth data is separated

cc <- cc %>% dplyr::select(-dateInitialWeight, -initialWeight, -finalWeight,
              -frassWeight, -weirdFinal)








