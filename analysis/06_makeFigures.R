# initialize
# goodness I love having fast-running models

rm(list = ls())
source("analysis/05_makePanTrapModels.R")

library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggpp)
library(RColorBrewer)
library(ggtext)
library(ggbeeswarm)

geu.new <- geUnchanged %>%
  mutate(hostNative = "exotic", hostFamily = "Caprifoliaceae")

geUnchanged$base <- predict(m.ge, newdata = geu.new)
geUnchanged$reFit <- geUnchanged$ge - geUnchanged$base

# make figure for growth efficiency ####

sum.ge <- summary(gl.ge, test = adjusted(type = "none"))
pval.ge <- sum.ge$test$pvalues
pval.ge[1] <- 0.001

mid <- summary(m.ge)
midi <- as.data.frame(mid$coefficients$cond[c(1:4,7:8),1:2])
midi$Estimate[1] <- 0
midi$Estimate[5:6] <- sum.ge$test$coefficients[2:3]
midi$`Std. Error`[5:6] <- sum.ge$test$sigma[2:3]
midi$q95 <- midi$Estimate + (1.96 * midi$`Std. Error`)
midi$q05 <- midi$Estimate - (1.96 * midi$`Std. Error`)
midi$hostNative <- c("exotic", "native", "exotic", "exotic", "native", "native")
midi$hostFamily <- c("Caprifoliaceae","Caprifoliaceae",
                     "Oleaceae", "Rosaceae", 
                     "Oleaceae", "Rosaceae")

midi

bold <- c("***", "", "***")
ekal <- c(" < ", " = ", " = ")
empha <- c("\\*\\*\\*", "", "\\*\\*")

siglab.ge <- data.frame(hostFamily = levels(factor(midi$hostFamily)),
                        pValue = pval.ge) %>%
  mutate(pValue = paste0(bold, "p", ekal, round(pValue, 3), bold, empha))

geUnchanged$hostNative <- fct_relevel(geUnchanged$hostNative, "native")
midi$hostNative <- fct_relevel(midi$hostNative, "native")

vio1 <- ggplot(data = geUnchanged, aes(x = hostFamily, y = reFit, fill = hostNative)) +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", color = "grey20", alpha = 1) +
  geom_split_violin(drop = F, width = .9, alpha = .8) +
  geom_point(data = geUnchanged[geUnchanged$hostNative == "native" &
                                  !geUnchanged$hostFamily %in% "InvasiveOutgroups", ],
              position = position_jitternudge(width = 0.1,
                                              seed = 1234, x = -0.15,
                                              nudge.from = "jittered"),
             color = "darkgreen", alpha = .3, size = 1.2) +
  geom_point(data = geUnchanged[geUnchanged$hostNative == "exotic", ],
             position = position_jitternudge(width = 0.1,
                                             seed = 1234, x = 0.15,
                                             nudge.from = "jittered"),
             color = "darkblue", alpha = .3, size = 1.2) +
  theme_tufte() +
  scale_x_discrete(position = "top") +
  # scale_y_continuous(trans = "log") +
  ylab("Scaled Growth Efficiency\n") +
  xlab("\nHost Family") +
  scale_fill_manual(values = c("#9CB380", "#586A6A")) +
  geom_errorbar(data = midi, aes(ymin = q05, ymax = q95, 
                                   x = hostFamily, group = hostNative), 
                width = .3, linewidth = 1.2,
                position = position_dodge(width = .5),
                inherit.aes = F) +
  geom_point(data = midi, aes(y = Estimate, 
                                x = hostFamily, group = hostNative), 
             size = 3,
             position = position_dodge(width = .5),
             inherit.aes = F) +
  geom_richtext(data = siglab.ge,
                aes(y = 1.25, x = hostFamily,
                    label = pValue),
                inherit.aes = F) +
  theme(legend.position = "none", 
        text = element_text(size = 18),
        axis.ticks = element_blank(),
        axis.title.x = element_blank())

# make pupal weight figure

sum.pw <- summary(gl.pw, test = adjusted(type = "none"))
pval.pw <- sum.pw$test$pvalues

pwp2$hostNative <- fct_relevel(pwp2$hostNative, "native")

pw.fig.init <- pwp2 %>%
  group_by(catSpecies) %>%
  mutate(species_mean = mean(pWeightLog)) %>%
  mutate(weight_resid = pWeightLog - species_mean) %>%
  ungroup()

pw.fig <- pw.fig.init %>%
  group_by(hostFamily, hostNative) %>%
  summarise(mean.w = mean(weight_resid),
            sd.w = sd(weight_resid),
            se.w = sd(weight_resid)/sqrt(n())) %>%
  mutate(q95 = mean.w + (1.96 * se.w),
         q05 = mean.w - (1.96 * se.w))

bold <- c("***", "", "***")
empha <- c("\\*\\*", "", "\\*")

siglab.pw <- data.frame(hostFamily = levels(factor(pw.fig$hostFamily)),
                        pValue = pval.pw) %>%
  mutate(pValue = paste0(bold, "p", ekal, round(pValue, 3), bold, empha))

vio2 <- ggplot(data = pw.fig.init, aes(x = hostFamily, y = weight_resid, fill = hostNative)) +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", color = "grey20", alpha = 1) +
  geom_split_violin(drop = F, width = .9, alpha = .8) +
  geom_point(data = pw.fig.init[pw.fig.init$hostNative == "native", ],
             position = position_jitternudge(width = 0.1,
                                             seed = 1234, x = -0.15,
                                             nudge.from = "jittered"),
             color = "darkgreen", alpha = .3, size = 1.2) +
  geom_point(data = pw.fig.init[pw.fig.init$hostNative == "exotic", ],
             position = position_jitternudge(width = 0.1,
                                             seed = 1234, x = 0.15,
                                             nudge.from = "jittered"),
             color = "darkblue", alpha = .3, size = 1.2) +
  scale_y_continuous(breaks = c(-1.5, -1, -.5, 0, .5)) +
  scale_x_discrete(position = "top") +
  theme_tufte() +
  ylab("Residual Pupal Weight\n") +
  scale_fill_manual(values = c("#9CB380", "#586A6A"),
                    name = "Hostplant\norigin",
                    labels = c("Native", "Exotic")) +
  geom_errorbar(data = pw.fig, aes(ymin = q05, ymax = q95, 
                                   x = hostFamily, group = hostNative), 
                width = .3, linewidth = 1.2,
                position = position_dodge(width = .5),
                inherit.aes = F) +
  geom_point(data = pw.fig, aes(y = mean.w, 
                                x = hostFamily, group = hostNative), 
             size = 3,
             position = position_dodge(width = .5),
             inherit.aes = F) +
  geom_richtext(data = siglab.pw,
                aes(y = 1, x = hostFamily,
                    label = pValue),
                inherit.aes = F) +
  theme(text = element_text(size = 18),
        legend.title = element_text(hjust = .5),
        axis.ticks = element_blank(),
        axis.title.x = element_blank())

vio2 <- vio2 + 
  theme(axis.text.x = element_blank(),
        legend.position = "none")

# vio1/vio2 + plot_layout(guides = "collect")

# make toided figure

# summary(m.toid)

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

toid.fig$hostNative <- fct_relevel(toid.fig$hostNative, "native")

sum.toid <- summary(gl.toid, test = adjusted(type = "none"))
pval.toid <- sum.toid$test$pvalues

bold <- c("*", "", "")
empha <- c("", "", "")

siglab.toid <- data.frame(hostFamily = levels(factor(toid.fig$hostFamily)),
                        pValue = pval.toid) %>%
  mutate(pValue = paste0(bold, "p = ", round(pValue, 3), bold, empha))


bp1 <- ggplot(data = toid.fig, aes(x = hostFamily, y = rate.toids, group = hostNative)) +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", color = "grey20", alpha = 1) +
  geom_errorbar(aes(ymin = q05, ymax = q95, color = hostNative),
                width = .4, linewidth = 1.2,
                position = position_dodge(width = .5)) +
  geom_point(position = position_dodge(width = .5),
             aes(color = hostNative), size = 3) +
  theme_tufte() +
  ylab("Residual Parasitoid Rate\n") +
  scale_x_discrete(position = "top") +
  scale_y_continuous(position = "right") +
  scale_color_manual(values = c("#9CB380", "#586A6A")) +
  geom_richtext(data = siglab.toid,
                aes(y = .3, x = hostFamily,
                    label = pValue),
                inherit.aes = F) +
  theme(text = element_text(size = 18),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title.x = element_blank())

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

pupal.fig$hostNative <- fct_relevel(pupal.fig$hostNative, "native")

sum.pupal <- summary(gl.pupa, test = adjusted(type = "none"))
pval.pupal <- sum.pupal$test$pvalues

pval.pupal[1] <- 0.001

bold <- c("***", "", "")
ekal <- c(" < ", " = ", " = ")
empha <- c("\\*\\*\\*", "", "")

siglab.pupal <- data.frame(hostFamily = levels(factor(pupal.fig$hostFamily)),
                          pValue = pval.pupal) %>%
  mutate(pValue = paste0(bold, "p = ", round(pValue, 3), bold, empha))

bp2 <- ggplot(data = pupal.fig, aes(x = hostFamily, y = rate.pupals, group = hostNative)) +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", color = "grey20", alpha = 1) +
  geom_errorbar(aes(ymin = q05, ymax = q95, color = hostNative),
                width = .4, linewidth = 1.2,
                position = position_dodge(width = .5)) +
  geom_point(position = position_dodge(width = .5),
             aes(color = hostNative), size = 3) +
  theme_tufte() +
  ylab("Residual Pupation Rate\n") +
  scale_y_continuous(breaks = c(-.2, 0, .2), 
                     limits = c(-.25, .35),
                     position = "right") +
  # scale_x_discrete(position = "top") +
  scale_color_manual(values = c("#9CB380", "#586A6A")) +
  geom_richtext(data = siglab.pupal,
                aes(y = .35, x = hostFamily,
                    label = pValue),
                inherit.aes = F) +
  theme(text = element_text(size = 18),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

(vio1/vio2) | (bp1/bp2)

# ggsave("figures/bigOne.png")

# make figure for growth efficiency change ####

changeCompare$changeDir <- fct_relevel(changeCompare$changeDir, 
                                       "eTOe","eTOn", 
                                       "unchanged_exotic",
                                       "nTOe","nTOn", 
                                       "unchanged_native",)

changeCompare$facetGroup <- ifelse(changeCompare$changeDir %in% c("eTOe","eTOn", 
                                                                  "unchanged_exotic"),
                                   1, 0)

cc.newdata <- changeCompare %>%
  mutate(changeDir = "unchanged_native")
cc.base <- predict(m.cc, newdata = cc.newdata)
changeCompare$reFit <- changeCompare$ge - cc.base
mrf <- median(changeCompare$reFit[changeCompare$changeDir %in% "unchanged_native"])
changeCompare$reFit <- changeCompare$reFit - mrf

labs <- c("To Novel\nExotic Host", "To\nNative Host", "To\nSame Host", 
          "To\nExotic Host", "To Novel\nNative Host", "To\nSame Host")

pr <- summary(m.cc)
did <- as.data.frame(pr$coefficients$cond[c(1,4:8) , 1:2])
did$changeDir <- c("unchanged_native",
                   "eTOe", "eTOn",
                   "nTOe", "nTOn",
                   "unchanged_exotic")
did$Estimate[1] <- 0

did$q95 <- did$Estimate + (1.96 * did$`Std. Error`)
did$q05 <- did$Estimate - (1.96 * did$`Std. Error`)
did$facetGroup <- c(0,1,1,0,0,1)

glabs <- c(`0` = "From\nNative Host",
           `1` = "From\nExotic Host")

ggplot(data = changeCompare, aes(x = changeDir, y = reFit)) +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", color = "red", linewidth = .75) +
  geom_violin(aes(fill = changeDir, color = hostNative), 
              linewidth = .75, alpha = .5) +
  geom_quasirandom(width = .3, alpha = 1, size = 3, stroke = 1, 
                   aes(fill = changeDir, shape = hostFamily)) +
  geom_errorbar(data = did, aes(y = Estimate, ymin = q05, ymax = q95),
                linewidth = 1.5, width = .2) +
  geom_point(data = did, aes(y = Estimate), size = 6) +
  scale_color_manual(values = c("grey75", "#253031"),
                     guide = "none") +
  scale_fill_manual(values = c("#586A6A", "#9CB380", "#F0D3F7", 
                               "#586A6A", "#9CB380", "#F0D3F7"),
                    guide = "none") +
  scale_shape_manual(values = c(22, 23, 24),
                     name = "Host Family\n(never changed)") + 
  scale_x_discrete(labels = labs,
                   breaks = c("eTOe","eTOn", 
                              "unchanged_exotic",
                              "nTOe","nTOn", 
                              "unchanged_native")) +
  coord_flip() +
  ylab("\nScaled growth efficiency") +
  theme_tufte() +
  facet_wrap(~facetGroup, nrow = 2, ncol = 1,
             drop = T, scales = "free_y",
             strip.position = "left",
             labeller = labeller(facetGroup = glabs))  +
  theme(text = element_text(size = 18),
        legend.title = element_text(hjust = .5),
        legend.box.spacing = unit(-1, "in"),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(hjust = .5),
        axis.ticks.y = element_blank(),
        strip.placement = "outside",
        strip.text = element_textbox(
          size = 18,  orientation = "left-rotated",
          color = "white", fill = "#5D729D", box.color = "black",
          halign = 0.5, linetype = 1, r = unit(10, "pt"), width = unit(1, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 15, 3)
        ))

# ggsave("figures/hostSwitching.png")

# make bird and toid foraging figure

mtw <- summary(m.tf.wasps)
eff.w <- mtw$coefficients$cond[1:2,1:2]

toidSlope.w <- function(x) {
  exp(eff.w[1,1]) + exp(eff.w[1,1] + (x * eff.w[2,1]))
}

rat <- seq(min(td.mush$ratio), max(td.mush$ratio), by = 0.01)
byrat <- toidSlope.w(rat)

toidRibbon.top <- exp(log(byrat) + eff.w[2,2])
toidRibbon.bottom <- exp(log(byrat) - eff.w[2,2])

toidRibbon.frame <- data.frame(top = toidRibbon.top,
                               bottom = toidRibbon.bottom,
                               ratio = rat)

waspo <- ggplot(data = td.mush, aes(x = ratio, y = wasps)) +
  geom_point(color = "gold3") +
  geom_ribbon(data = toidRibbon.frame, 
              aes(ymin = bottom, ymax = top, x = ratio), 
              inherit.aes = F,
              alpha = .1, fill = "gold4") +
  geom_line(stat = "function", fun = toidSlope.w) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 20)) +
  theme_tufte() +
  ylab("Total parasitoid wasps\n") +
  xlab("\nExotic plant volume - Native plant volume") +
  theme(legend.position = "none",
        text = element_text(size = 18))

mtf <- summary(m.tf.flies)
eff.f <- mtf$coefficients$cond[1:2,1:2]

toidSlope.f <- function(x) {
  exp(eff.f[1,1]) + exp(eff.f[1,1] + (x * eff.f[2,1]))
}

byrat.f <- toidSlope.f(rat)

toidRibbon.top.f <- exp(log(byrat.f) + eff.f[2,2])
toidRibbon.bottom.f <- exp(log(byrat.f) - eff.f[2,2])

toidRibbon.frame.f <- data.frame(top = toidRibbon.top.f,
                               bottom = toidRibbon.bottom.f,
                               ratio = rat)

ggplot(data = td.mush, aes(x = ratio, y = flies)) +
  geom_jitter(color = "darkblue", shape = 19) +
  geom_ribbon(data = toidRibbon.frame.f, 
              aes(ymin = bottom, ymax = top, x = ratio), 
              inherit.aes = F,
              alpha = .1, fill = "blue") +
  geom_line(stat = "function", fun = toidSlope.f, alpha = .8) +
  theme_tufte() +
  ylab("Parasitoids captured\n") +
  xlab("\nVegetation origin") +
  scale_x_continuous(breaks = c(-2.5, -.5, 1.5),
                     labels = c("more\nnative", "equal", "more\nexotic")) +
  geom_jitter(aes(y = wasps), color = "gold3") +
  geom_ribbon(data = toidRibbon.frame, 
              aes(ymin = bottom, ymax = top, x = ratio), 
              inherit.aes = F,
              alpha = .1, fill = "gold4") +
  geom_line(stat = "function", fun = toidSlope.w, alpha = .8) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 20)) +
  theme(legend.position = "none",
        text = element_text(size = 18),
        axis.ticks.x = element_blank())


# ggsave("figures/toidForage.png")

# make bird strike figure

summary(m.clay)

clays.new <- clays %>%
  mutate(upDate = 0)

clays$base <- plogis(predict(m.clay, newdata = clays.new))

clays$effect <- clays$strikes - clays$base

mtb <- summary(m.clay)
eff.b <- mtb$coefficients$cond[c(1,3),1:2]

birdSlope <- function(x) {
  exp(eff.b[1,1]) + exp(eff.b[1,1] + (x * eff.b[2,1]))
}

rat <- seq(min(clay.fig$eve), max(clay.fig$eve), by = 0.01)
byrat <- birdSlope(rat)

toidRibbon.top.b <- exp(log(byrat) + eff.b[2,2])
toidRibbon.bottom.b <- exp(log(byrat) - eff.b[2,2])

toidRibbon.frame.b <- data.frame(top = toidRibbon.top.b,
                                 bottom = toidRibbon.bottom.b,
                                 ratio = rat)

clay.fig <- clays %>%
  mutate(eve = upDate,
         rate = strikes/trials) %>%
  group_by(eve) %>%
  summarise(trials.total = sum(trials),
            strikes.total = sum(strikes),
            rate.avg = sum(strikes)/sum(trials),
            strikes.sd = sd(strikes),
            strikes.se = sd(strikes)/(n() * 3),
            rate.sd = sd(rate),
            rate.se = sd(rate)/n(),
            ratio.avg = mean(ratio)) %>%
  mutate(q95 = rate.avg + (1.96 * rate.se),
         q05 = rate.avg - (1.96 * rate.se))

rampo <- colorRampPalette(c("#9CB380", "#586A6A"))

ggplot(data = clay.fig, aes(x = eve, y = rate.avg, group = eve)) +
  geom_errorbar(aes(ymin = q05, ymax = q95, color = ratio.avg),
                width = .15, linewidth = 1.1) +
  geom_point(aes(color = ratio.avg),
             size = 2) +
  geom_ribbon(data = toidRibbon.frame.b, 
              aes(ymin = bottom, ymax = top, x = ratio), 
              inherit.aes = F,
              alpha = .1, fill = "grey20") +
  geom_line(stat = "function", fun = birdSlope, alpha = .8) +
  theme_tufte() +
  scale_color_gradient(low = "lightgreen", high = "darkblue",
                       breaks = c(-.4, .5), labels = c("more\nexotic",
                                                       "more\nnative"),
                       name = "Vegetation\norigin\n") +
  scale_x_continuous(breaks = c(-.85, -.35, .15, .65, 1.15),
                     labels = c("Jun. 1", "Jun. 15", "Jul. 1",
                                "Jul. 15", "Jul. 30")) +
  ylab("Proportion predated\n") +
  xlab("\nDate") +
  theme(text = element_text(size = 18),
        legend.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("figures/birdPred.png")

# summary(m.clay)

# make summary figure (network)

library(bipartite)

cc.bip <- cc %>%
  filter(!treeSpecies %in% c("BERTH", "EUOAL", "CELOR", "RHOSC")) %>%
  filter(catSpecies %in% cats.good$catSpecies) %>%
  filter(!catSpecies %in% "MICROX") %>%
  mutate(treeSpecies = ifelse(treeSpecies %in% c("MALFL", "MALPR", "MALPU", "MALSI"),
                              "MALXX", treeSpecies)) %>%
  mutate(treeSpecies = ifelse(treeSpecies %in% c("VIBOP", "SAMCA"),
                              "VIBDE", treeSpecies)) %>%
  group_by(catSpecies, treeSpecies) %>%
  summarise(total = n()) %>%
  pivot_wider(id_cols = catSpecies, 
              names_from = treeSpecies, 
              values_from = total,
              values_fill = 0) %>%
  column_to_rownames("catSpecies") %>%
  as.matrix() %>%
  log1p()

spec <- c("FRAAM", "LIGOB", 
          "MALXX", "PRUSE", "ROSMU", 
          "VIBAC", "VIBDI", 
          "VIBLE", "CRAXX", "LONMO", 
          "RUBXX", "VIBPL", "VIBDE", 
          "AMEXX", "VIBSI")
fam <- c(3,3,
         2,2,2,
         1,1,
         1,2,1,
         2,1,1,
         2,1)
nat <- c(1,0,
         0,1,0,
         1,0,
         1,1,0,
         1,0,1,
         1,0)

values = c("#6C9A8B", "#840032", "#020887")

spec.high <- data.frame(treeSpecies = spec,
                        hostFamily = fam,
                        hostNative = nat) %>%
  arrange(hostFamily, -hostNative, treeSpecies)

spec.high$color <- c(rep("#020887", 3),
                     rep("#585A87", 4),
                     rep("#840032", 4),
                     rep("#844D5F", 2),
                     rep("#FFA630", 1),
                     "#F1D7B5")

catso <- c("ACHADI", "ACROSU", "BISTBE", "CAMPPE", "CATOUL", "CROCNO", 
  "ECTRCR", "ELAPVE", "ENNOSU", "EUPIMI", "EUPISW", "EUPSXX", "EUTRCL", 
  "HETEGU", "HIMEFI", "HYPAUN", "HYPESC", "IRIDEP", "IRIDLA", "LAMBFI", 
  "LITHXX", "LOMOVE", "MELACA", "MESORU", "MORRCO", "MORRLA", "NEMARE", 
  "ORGYLE", "ORTHRU", "PALEVE", "PALTAN", "PROCLI", "PROTPO", "PSEUCY", 
  "SCHIUN", "ZALEHO", "ZALEXX", "ZANCXX")

fam.cat <- c("n", "n", "g", "g", "e", "n",
             "g", "n", "g", "g", "g", "n", "g",
             "not", "n", "g", "e", "g", "g", "g",
             "n", "g", "g", "g", "n", "n", "g", 
             "e", "n", "g", "n", "g", "g", "d",
             "not", "e", "e", "n")

spec.low <- data.frame(catSpecies = catso,
           catFamily = fam.cat) %>%
  arrange(catFamily, catSpecies)

spec.low$colors <- c(
  "#152F15",
  rep("#31572C", 5),
  rep("#4F772D", 18,),
  rep("#90A955", 12,),
  rep("#ECF39E", 2)
)

cc.bip <- cc.bip[spec.low$catSpecies, spec.high$treeSpecies]

catlabs <- c("Pseu. cy.", "Cato. ul.", "Hype. sc.", "Orgy. le.", 
  "Zale ho.", "Zale lu.", "Bist. be.", "Camp. pe.", "Ectr. cr.", 
  "Enno. su.", "Eupi. mi.", "Eupi. sw.", "Eutr. cl.", "Hypa. un.", 
  "Irid. ep.", "Irid. la.", "Lamb. fi.", "Lomo. ve.", "Mela. ca.", 
  "Meso. ru.", "Nema. re.", "Pale. ve.", "Proc. li.", "Prot. po.", 
  "Acha. di.", "Acro. su.", "Croc. no.", "Elap. ve.", "Eups.", "Hime. fi.", 
  "Lith.", "Morr. co.", "Morr. la.", "Orth.", "Palt. an.", 
  "Zanc.", "Hete. gu.", "Schi. un.")

row.names(cc.bip) <- catlabs

tlabs <- c("V. acerifolium", "V. dentatum", "V. lentago", "L. morrowii", "V. dillitatum", 
  "V. plicatum", "V. sieboldii", "Amelanchier", "Crataegus", "P. serotina", "Rubus", 
  "Malus", "R. multiflora", "F. americanum", "L. obtusifolium")

colnames(cc.bip) <- tlabs

par(font = 3)

plotweb(cc.bip, 
        method = "normal",
        col.high = spec.high$color,
        col.low = spec.low$color,
        text.rot = 90, 
        labsize =  1.8,
        ybig = 1.1)

yo <- cbind.data.frame(spec.low, tot = rowSums(exp(cc.bip) - 1))

yy <- yo %>%
  group_by(catFamily) %>%
  summarise(total = sum(tot))
  

