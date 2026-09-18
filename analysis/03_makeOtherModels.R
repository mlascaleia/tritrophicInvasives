# initialize ####

source("analysis/02_makeGrowthModels.R")

# per-family native vs exotic tests, used wherever an interaction model wins
fam.tests <- c("hostNativenative = 0",
               "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
               "hostNativenative + hostNativenative:hostFamilyRosaceae = 0")

# parasitoid models ####

t1 <- glmmTMB(isToid ~ hostNative * hostFamily +
                year + jDate + (1|transect) + diag(hostNative|catSpecies),
              data = toidest, family = "binomial") # maximal
t2 <- update(t1, . ~ . - hostNative:hostFamily) # additive
t3 <- update(t1, . ~ . - diag(hostNative|catSpecies) + (1|catSpecies)) # interactive, no slope
t4 <- update(t2, . ~ . - diag(hostNative|catSpecies) + (1|catSpecies)) # additive, no slope
t0 <- update(t4, . ~ . - hostNative - hostFamily)

bbmle::AICctab(t1, t2, t3, t4, t0, base = T) # additive (no slope) wins
m.toid <- t4
rm(list = ls(pattern = "^t[[:digit:]]$"))

# no interaction to explore; single test kept so downstream code still works
gl.toid <- glht(m.toid, linfct = c("hostNativenative = 0"))
summary(m.toid)
summary(gl.toid)

# pupation models ####

p1 <- glmmTMB(isPupal ~ hostNative * hostFamily +
                year + (hostNative|catSpecies),
              data = pupest, family = "binomial") # maximal
p2 <- update(p1, . ~ . - hostNative:hostFamily) # additive
p3 <- update(p1, . ~ . - (hostNative|catSpecies) + (1|catSpecies)) # interactive no slope
p4 <- update(p2, . ~ . - (hostNative|catSpecies) + (1|catSpecies)) # additive no slope
p0 <- update(p4, . ~ . - hostNative - hostFamily)

bbmle::AICctab(p1, p2, p3, p4, p0, base = T)

m.pupa <- p3 # set to the winner
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
w3 <- update(w1, . ~ . - (hostNative|catSpecies) + (1|catSpecies))
w4 <- update(w2, . ~ . - (hostNative|catSpecies) + (1|catSpecies))
w0 <- update(w4, . ~ . - hostNative - hostFamily)

bbmle::AICctab(w1, w2, w3, w4, w0, base = TRUE)

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
d3 <- update(d1, . ~ . - (hostNative|catSpecies) + (1|catSpecies))
d4 <- update(d2, . ~ . - (hostNative|catSpecies) + (1|catSpecies))
d0 <- update(d4, . ~ . - hostNative - hostFamily)

bbmle::AICctab(d1, d2, d3, d4, d0 , base = TRUE)
m.death <- d1 # set to the winner
rm(list = ls(pattern = "^d[[:digit:]]$"))

ee.death <- emmeans(m.death, pairwise ~ hostNative)
gl.death <- glht(m.death, linfct = fam.tests)
summary(m.death)
summary(gl.death, test = adjusted(type = "none"))



