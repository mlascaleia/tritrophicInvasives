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
m3 <- update(m1, . ~ . - (hostNative|catSpecies) + (1|catSpecies)) # interactive with old random structure
m4 <- update(m2, . ~ . - (hostNative|catSpecies) + (1|catSpecies)) # addictive with old random structure
m0 <- update(m4, . ~ . - hostNative - hostFamily)  # null


bbmle::AICctab(m1, m2, m3, m4, m0, base = T) # m2 wins and gets the m.ge mantle
m.ge <- m2
rm(list = ls(pattern = "^m[[:digit:]]$"))

# nothing more needs to be done since an additive model won (no interactions to explore)








