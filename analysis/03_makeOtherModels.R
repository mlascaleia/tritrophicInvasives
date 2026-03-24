# initialize ####

source("analysis/02_makeGrowthModels.R")

library(glmmTMB)

# library(lme4)

m.toid <- glmmTMB(isToid ~ hostNative * hostFamily +
                 year + (1|transect) + (1|catSpecies), 
              data = toidest, family = "binomial")

summary(m.toid)

# tests whther there's an effect in each of the three families
gl.toid <- glht(m.toid, linfct = c("hostNativenative = 0", 
                                   "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                                   "hostNativenative + hostNativenative:hostFamilyRosaceae = 0"))

summary(gl.toid, test = adjusted(type = "none"))

m.toid2 <- glmmTMB(isToid ~ hostNative + hostFamily +
                    year + jDate + (1|transect) + (1|catSpecies), 
                  data = toidest, family = "binomial")

summary(m.toid2)

rm(m.toid, gl.toid)

# make pupal model ####

m.pupal <- glmmTMB(isPupal ~ hostNative * hostFamily +
                    year + (1|transect) + (1|catSpecies), 
                  data = pupest, family = "binomial")

summary(m.pupal)

# tests whther there's an effect in each of the three families
gl.pupa <- glht(m.pupal, linfct = c("hostNativenative = 0", 
                                    "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                                    "hostNativenative + hostNativenative:hostFamilyRosaceae = 0"))

summary(gl.pupa, test = adjusted(type = "none"))

m.pupal2 <- glmmTMB(isPupal ~ hostNative + hostFamily +
                     year + (1|transect) + (1|catSpecies), 
                   data = pupest, family = "binomial")

summary(m.pupal2)

rm(m.pupal2)

# pupal weight analysis ####

m.pw <- glmmTMB(pWeightLog ~ hostNative * hostFamily +
                  (1|transect) + (1|catSpecies), 
                data = pwp2)
summary(m.pw)

# tests whther there's an effect in each of the three families
gl.pw <- glht(m.pw, linfct = c("hostNativenative = 0", 
                               "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                               "hostNativenative + hostNativenative:hostFamilyRosaceae = 0"))

summary(gl.pw, test = adjusted(type = "none"))

m.pw2 <- glmmTMB(pWeightLog ~ hostNative + hostFamily +
                  (1|transect) + (1|catSpecies), 
                data = pwp2)
summary(m.pw2)

