# initialize

source("analysis/01_makeGrowthData.R")

library(glmmTMB)
library(multcomp)
# library(emmeans)

# make growth models ####

# check effects of removing 2022
# geCats <- geCats[geCats$year == 2021,]

# growth efficiency

m.ge <- glmmTMB(ge ~ hostNative * hostFamily + 
                     year + log(initialWeight) + (1|catSpecies),
                data = geCats)
summary(m.ge)

# ee <- emmeans(m.ge, pairwise ~ hostNative)

gl.ge <- glht(m.ge, linfct = c("hostNativenative = 0", 
                                   "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                                   "hostNativenative + hostNativenative:hostFamilyRosaceae = 0"))

summary(gl.ge, test = adjusted(type = "none"))


