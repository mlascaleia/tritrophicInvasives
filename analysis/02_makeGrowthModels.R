# initialize

source("analysis/01_makeGrowthData.R")

library(glmmTMB)
library(multcomp)

# make growth models ####

# growth efficiency

geUnchanged$hostFamily <- fct_relevel(geUnchanged$hostFamily, "Roseaceae")
geUnchanged$hostNative <- fct_relevel(geUnchanged$hostNative, "exotic")
geUnchanged <- geUnchanged[geUnchanged$hostFamily != "InvasiveOutgroups", ]

m.ge <- glmmTMB(ge ~ hostNative * hostFamily + 
                     year + log(initialWeight) + (1|catSpecies),
                data = geUnchanged)
summary(m.ge)

# look at changing caterpillars

geChanged <- geCats[geCats$changed %in% 1, ]
changedSpecies <- unique(geChanged$catSpecies)

changeCompare <- geCats[geCats$catSpecies %in% changedSpecies &
                          geCats$year %in% 2022, ]

changeCompare <- changeCompare %>%
  mutate(changeDir = ifelse(changeDir %in% "unchanged" &
                              hostNative %in% "native", 
                            "unchanged_native", changeDir)) %>%
  mutate(changeDir = ifelse(changeDir %in% "unchanged" &
                              hostNative %in% "exotic", 
                            "unchanged_exotic", changeDir)) %>%
  mutate(ge = ifelse(wtChange < 0, 0, ge)) 

#multcomp

changeCompare$changeDir <- fct_relevel(changeCompare$changeDir, "unchanged_native")
# ge.cc <- changeCompare[changeCompare$wtChange > 0, ]
m.cc <- glmmTMB(ge ~ newHostFamily + changeDir +
                  log(initialWeight) + (1|catSpecies), 
                data = changeCompare, 
                family = gaussian)
summary(m.cc)


gl.cc <- glht(m.cc, linfct = c("(Intercept) - changeDirnTOe = 0", 
                               "(Intercept) - changeDirnTOn = 0",
                               "changeDirunchanged_exotic - changeDireTOn = 0",
                               "changeDirnTOe - changeDirnTOn = 0",
                               "changeDireTOn - changeDireTOe = 0"))
summary(gl.cc, test = adjusted(type = "none"))

rm(geChanged, geValid)






