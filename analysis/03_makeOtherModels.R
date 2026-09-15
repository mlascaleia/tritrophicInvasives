# initialize ####

source("analysis/02_makeGrowthModels.R")

library(emmeans)
library(glmmTMB)

# library(lme4)
# not used anymore as the non-interactive model performed better
# m.toid <- glmmTMB(isToid ~ hostNative * hostFamily +
#                  year + (1|transect) + (1|catSpecies), 
#               data = toidest, family = "binomial")
# 
# summary(m.toid)
# 
# # tests whether there's an effect in each of the three families
# gl.toid <- glht(m.toid, linfct = c("hostNativenative = 0", 
#                                    "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
#                                    "hostNativenative + hostNativenative:hostFamilyRosaceae = 0"))
# 
# summary(gl.toid, test = adjusted(type = "none"))

m.toid <- glmmTMB(isToid ~ hostNative + hostFamily +
                    year + jDate + (1|transect) + (1|catSpecies), 
                  data = toidest, family = "binomial")

emmeans(m.toid, pairwise ~ hostNative)

# a simpler gl comparison than others due to no interactions
# but I want to keep it to help the future code still work
gl.toid <- glht(m.toid, linfct = c("hostNativenative = 0"))
summary(m.toid)
summary(gl.toid)



# make pupal model ####

m.pupal <- glmmTMB(isPupal ~ hostNative * hostFamily +
                   year + (1|catSpecies), 
                  data = pupest, family = "binomial")



emmeans(m.pupal, pairwise ~ hostNative)

# tests whther there's an effect in each of the three families
gl.pupa <- glht(m.pupal, linfct = c("hostNativenative = 0", 
                                    "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                                    "hostNativenative + hostNativenative:hostFamilyRosaceae = 0"))

summary(m.pupal)
summary(gl.pupa, test = adjusted(type = "none"))

# m.pupal2 <- glmmTMB(isPupal ~ hostNative + hostFamily +
#                      year + (1|transect) + (1|catSpecies), 
#                    data = pupest, family = "binomial")
# 
# summary(m.pupal2)
# 
# rm(m.pupal2)

# pupal weight analysis ####

m.pw <- glmmTMB(pWeightLog ~ hostNative * hostFamily +
                  (1|catSpecies), 
                data = pwp2)


emmeans(m.pw, pairwise ~ hostNative)

# tests whther there's an effect in each of the three families
gl.pw <- glht(m.pw, linfct = c("hostNativenative = 0", 
                               "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                               "hostNativenative + hostNativenative:hostFamilyRosaceae = 0"))


summary(m.pw)
summary(gl.pw, test = adjusted(type = "none"))
# 
# m.pw2 <- glmmTMB(pWeightLog ~ hostNative + hostFamily +
#                   (1|transect) + (1|catSpecies), 
#                 data = pwp2)
# summary(m.pw2)



# add death from all causes (except killed) analysis

m.death <- glmmTMB(isDeceased ~ hostNative * hostFamily +
                    year + jDate + (1|transect) + (1|catSpecies), 
                  data = cc[cc$isMissing == 0,], family = "binomial")


emmeans(m.death, pairwise ~ hostNative)

gl.death <- glht(m.death, linfct = c("hostNativenative = 0", 
                               "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                               "hostNativenative + hostNativenative:hostFamilyRosaceae = 0"))

summary(m.death)
summary(gl.death, test = adjusted(type = "none"))







