# initialize
# goodness I love having fast-running models

rm(list = ls())
source("analysis/03_makeOtherModels.R")

library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggpp)

# makeOne <- geUnchanged[10, ]
# makeOne$hostFamily <- "InvasiveOutgroups"
# makeOne$ge <- .5
# makeOne$hostNative <- "native"
# 
# geUnchanged <- rbind(geUnchanged, makeOne)

geu.new <- geUnchanged %>%
  mutate(hostNative = "exotic")

geUnchanged$base <- predict(m.ge, newdata = geu.new)
geUnchanged$reFit <- geUnchanged$ge - geUnchanged$base

# make figure for growth efficiency ####

mid <- summary(m.ge)
midi <- as.data.frame(mid$coefficients$cond[c(1:4,7:8),1:2])
midi$Estimate[1] <- 0
midi$q95 <- midi$Estimate + (1.96 * midi$`Std. Error`)
midi$q05 <- midi$Estimate - (1.96 * midi$`Std. Error`)
midi$hostNative <- c("native", "exotic", "native", "native", "exotic", "exotic")
midi$hostFamily <- c("Roseaceae", "Roseaceae", 
                     "Caprifoliaceae", "Oleaceae", 
                     "Caprifoliaceae", "Oleaceae")

ggplot(data = geUnchanged, aes(x = hostFamily, y = reFit, fill = hostNative)) +
  geom_split_violin(drop = F, width = 1.15) +
  geom_point(data = geUnchanged[geUnchanged$hostNative == "native" &
                                  !geUnchanged$hostFamily %in% "InvasiveOutgroups", ],
              position = position_jitternudge(width = 0.1,
                                              seed = 1234, x = -0.15,
                                              nudge.from = "jittered"),
             color = "darkgreen", alpha = .5, size = 1.1) +
  geom_point(data = geUnchanged[geUnchanged$hostNative == "exotic", ],
             position = position_jitternudge(width = 0.1,
                                             seed = 1234, x = 0.15,
                                             nudge.from = "jittered"),
             color = "darkblue", alpha = .5, size = 1.1) +
  theme_tufte() +
  ylab("Scaled Growth Efficiency\n") +
  xlab("\nHost Family") +
  scale_fill_manual(values = c("#9CB380", "#586A6A")) +
  theme(legend.position = "none")

# make figure for growth efficiency change ####

changeCompare$changeDir <- fct_relevel(changeCompare$changeDir, 
                                       "eTOe","eTOn", 
                                       "unchanged_exotic",
                                       "nTOe","nTOn", 
                                       "unchanged_native",)

cc.newdata <- changeCompare %>%
  mutate(changeDir = "unchanged_native")
cc.base <- predict(m.cc, newdata = cc.newdata)
changeCompare$reFit <- changeCompare$ge - cc.base
mrf <- median(changeCompare$reFit[changeCompare$changeDir %in% "unchanged_native"])
changeCompare$reFit <- changeCompare$reFit - mrf

labs <- c("Exotic", "Native", "Unchanged", 
          "Exotic", "Native", "Unchanged")

pr <- summary(m.cc)
did <- as.data.frame(pr$coefficients$cond[c(1,4:8) , 1:2])
did$changeDir <- c("unchanged_native",
                   "eTOe", "eTOn",
                   "nTOe", "nTOn",
                   "unchanged_exotic")
did$Estimate[1] <- 0

did$q95 <- did$Estimate + (1.96 * did$`Std. Error`)
did$q05 <- did$Estimate - (1.96 * did$`Std. Error`)

ggplot(data = changeCompare, aes(x = changeDir, y = reFit)) +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", color = "red", linewidth = .75) +
  geom_violin(aes(fill = changeDir, color = hostNative), 
              linewidth = 1, alpha = 1) +
  # geom_boxplot(width = .1, linewidth = .75, fill = NA) +
  geom_point(data = did, aes(y = Estimate), size = 4) +
  geom_errorbar(data = did, aes(y = Estimate, ymin = q05, ymax = q95),
                  linewidth = 1.5, width = .2) +
  geom_jitter(width = .075, alpha = .5, size = 2, stroke = 1, 
              aes(fill = changeDir, shape = hostFamily)) +
  scale_color_manual(values = c("grey75", "#253031")) +
  scale_fill_manual(values = c("#E0A890", "#F0D3F7", "#70B77E", 
                                "#E0A890", "#F0D3F7", "#70B77E"),
                    labels = labs) +
  scale_shape_manual(values = c(21, 22, 23)) + 
  scale_x_discrete(labels = labs, name = "") +
  theme_tufte() +
  coord_flip() +
  theme(legend.position = "none")

# make toided figure

toid.fig <- toidest %>%
  group_by(catSpecies) %>%
  mutate(species_mean = mean(isToid)) %>%
  mutate(toid_resid = isToid - species_mean) %>%
  ungroup() %>%
  group_by(hostFamily, hostNative) %>%
  summarise(cats = n(), 
            toids = sum(toid_resid),
            rate.toids = sum(toid_resid)/n(),
            sd.toids = sd(toid_resid),
            se.toids = sd(toid_resid)/sqrt(n())) %>%
  mutate(q95 = rate.toids + (1.96 * se.toids),
         q05 = rate.toids - (1.96 * se.toids))

ggplot(data = toid.fig, aes(x = hostFamily, y = rate.toids, group = hostNative)) +
  geom_errorbar(aes(ymin = q05, ymax = q95), position = position_dodge(width = .5))

# make pupal figure

pupal.fig <- pupest %>%
  group_by(catSpecies) %>%
  mutate(species_mean = mean(isPupal)) %>%
  mutate(pupal_resid = isPupal - species_mean) %>%
  ungroup() %>%
  group_by(hostFamily, hostNative) %>%
  summarise(cats = n(), 
            pupals = sum(pupal_resid),
            rate.pupals = sum(pupal_resid)/n(),
            sd.pupals = sd(pupal_resid),
            se.pupals = sd(pupal_resid)/sqrt(n())) %>%
  mutate(q95 = rate.pupals + (1.96 * se.pupals),
         q05 = rate.pupals - (1.96 * se.pupals))

ggplot(data = pupal.fig, aes(x = hostFamily, y = rate.pupals, group = hostNative)) +
  geom_errorbar(aes(ymin = q05, ymax = q95), position = position_dodge(width = .5))

# make pupal weight figure

pw.fig <- pwp2 %>%
  group_by(catSpecies) %>%
  mutate(species_mean = mean(pWeightLog)) %>%
  mutate(weight_resid = pWeightLog - species_mean) %>%
  ungroup() %>%
  group_by(hostFamily, hostNative) %>%
  summarise(mean.w = mean(weight_resid),
            sd.w = sd(weight_resid),
            se.w = sd(weight_resid)/sqrt(n())) %>%
  mutate(q95 = mean.w + (1.96 * se.w),
         q05 = mean.w - (1.96 * se.w))

ggplot(data = pw.fig, aes(x = hostFamily, y = mean.w, group = hostNative)) +
  geom_pointrange(aes(ymin = q05, ymax = q95), position = position_dodge(width = .5))


