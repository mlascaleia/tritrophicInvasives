# intitialize ####

source("analysis/04_makePanTrapData.R")

td.mush$ratio <- scale(td.mush$vol.exo - td.mush$vol.nat)
td.mush$vegVolume <- td.mush$vol.exo + td.mush$vol.nat

td.mush <- td.mush %>%
  rename(flies = Tachinidae)

m.tf <- glmmTMB(wasps ~ ratio + ratio:scale(vegVolume) +
                upDate +
                scale(fail_amount) +
                offset(log(daysOut)) + (1|block),
              data = td.mush, family = nbinom2)

summary(m.tf)

clays$ratio <- scale(clays$vol.exo - clays$vol.nat)
clays$vegVolume <- clays$vol.exo + clays$vol.nat

m.clay <- glmmTMB(cbind(strikes, trials) ~ ratio + ratio:scale(vegVolume) +
                    upDate + clay +
                    (1|trap),
                  data = clays, family = binomial)

summary(m.clay)
# diagnose(m.clay)



