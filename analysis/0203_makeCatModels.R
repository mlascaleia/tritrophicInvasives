# initialize

source("analysis/01_makeGrowthData.R")

library(glmmTMB)
library(multcomp)
library(emmeans)

# make growth models ####
# remake to be clearer and align with paper methods:
m1 <- glmmTMB(ge ~ hostNative * hostFamily + 
                year + log(initialWeight) + (hostNative|catSpecies),
              data = geCats) # maximal
m2 <- update(m1, . ~ . - hostNative:hostFamily) # additive, keep random structure


aic.ge <- bbmle::AICctab(m1, m2, base = T) # m2 wins and gets the m.ge mantle
m.ge <- m2
rm(list = ls(pattern = "^m[[:digit:]]$"))

# nothing more needs to be done since an additive model won (no interactions to explore)


# per-family native vs exotic tests, used wherever an interaction model wins
fam.tests <- c("hostNativenative = 0",
               "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
               "hostNativenative + hostNativenative:hostFamilyRosaceae = 0")

# parasitoid models ####

t1 <- glmmTMB(isToid ~ hostNative * hostFamily +
                year + jDate + (1|transect) + diag(hostNative|catSpecies),
              data = toidest, family = "binomial") # maximal
t2 <- update(t1, . ~ . - hostNative:hostFamily) # additive

aic.toid <- bbmle::AICctab(t1, t2, base = T) # additive wins
m.toid <- t2
rm(list = ls(pattern = "^t[[:digit:]]$"))

# no interaction to explore so only single test kept
gl.toid <- glht(m.toid, linfct = c("hostNativenative = 0"))
summary(m.toid)
summary(gl.toid)

# pupation (survivalish) model ####

p1 <- glmmTMB(isPupal ~ hostNative * hostFamily +
                year + (hostNative|catSpecies),
              data = pupest, family = "binomial") # maximal
p2 <- update(p1, . ~ . - hostNative:hostFamily) # additive

aic.pupa <- bbmle::AICctab(p1, p2, base = T)

m.pupa <- p1 # set to the winner
rm(list = ls(pattern = "^p[[:digit:]]$"))

ee.pupa <- emmeans(m.pupa, pairwise ~ hostNative)
gl.pupa <- glht(m.pupa, linfct = fam.tests)

summary(m.pupa)
summary(gl.pupa, test = adjusted(type = "none"))

# pupal weight models ####

w1 <- glmmTMB(pWeightLog ~ hostNative * hostFamily +
                (hostNative|catSpecies),
              data = pwp2) # maximal
w2 <- update(w1, . ~ . - hostNative:hostFamily)

aic.pw <- bbmle::AICctab(w1, w2, base = TRUE)

m.pw <- w1 # set to the winner
rm(list = ls(pattern = "^w[[:digit:]]$"))

ee.pw <- emmeans(m.pw, pairwise ~ hostNative)
gl.pw <- glht(m.pw, linfct = fam.tests)

summary(m.pw)
summary(gl.pw, test = adjusted(type = "none"))

# death from all causes (except killed) models ####

d1 <- glmmTMB(isDeceased ~ hostNative * hostFamily +
                year + jDate + (1|transect) + (hostNative|catSpecies),
              data = cc[cc$isMissing == 0, ], family = "binomial") # maximal
d2 <- update(d1, . ~ . - hostNative:hostFamily)

aic.death <- bbmle::AICctab(d1, d2, base = TRUE)
m.death <- d1 # set to the winner
rm(list = ls(pattern = "^d[[:digit:]]$"))

ee.death <- emmeans(m.death, pairwise ~ hostNative)
gl.death <- glht(m.death, linfct = fam.tests)
summary(m.death)
summary(gl.death, test = adjusted(type = "none"))