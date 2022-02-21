# initialize ####

rm(list = ls())
library(lme4)
library(tidyverse)
library(lmerTest)
library(lubridate)
library(sjPlot)
library(ggthemes)
load("data/clean/catWeights.rdata")

# organize data ####

# eliminate unecessary columns

cw <- cw %>%
  dplyr::select(catNum, date, catSpecies, treeSpecies, fate, hostNative, hostFamily,
         dateInitialWeight, initialWeight, finalWeight, frassWeight, weirdFinal, aged)

# eliminate unidentified and invasive caterpillars

dontInclude <- c("LYMADI", "MICROX", "NOT ORTHRU", "GEOMXX", "NOT ZALE", "NOCTXX",
                 "UNKNXX", "UNIDXX", "FOURLX", "CERAUN", "HYALCE", "EREBXX", "RHEUPR")
cw <- cw[!cw$catSpecies %in% dontInclude, ]

# make date julian
cw$jDate <- paste0(20210, cw$date) %>%
  ymd() %>%
  yday() %>%
  scale()

# make caterpillar weights dataframe ####

# eliminate weirdfinals and unwieghed
cwc <- cw[cw$weirdFinal %in% "" & !is.na(cw$finalWeight),]

# make weight change
cwc$wtChange <- cwc$finalWeight - cwc$initialWeight 

# make weight change percent
cwc$wtChangePer <- cwc$wtChange/cwc$initialWeight

# make grwoth efficiency
cwc$ge <- cwc$wtChange/cwc$frassWeight

# ignore shrinking caterpillars (rejecting host)
cwc <- cwc[cwc$ge >= 0, ]

# boxplot(log(wtChange) ~ hostNative, data = cwc)
# boxplot(ge ~ hostNative, data = cwc)

# get growth efficiency residuals by frass produced
cwc$geResid <- resid(lm(log(wtChange) ~ log(frassWeight), data = cwc))

# hist(cwc$geResid)
# table(cwc$catSpecies, cwc$hostNative)

# # remove invasive plant outgroups (BERTH + EUOAL)
# cwc <- cwc[!cwc$hostFamily %in% "InvasiveOutgroups", ]

# # remove apple
# cwc <- cwc[!cwc$treeSpecies %in% "MALXX",]

# give host families contrast sums
cwc$hostFamily <- factor(cwc$hostFamily)
contrasts(cwc$hostFamily) <- "contr.sum"

# make models ####

m1 <- lmer(log(ge) ~ hostNative + log(initialWeight) + hostFamily + (1|catSpecies), data = cwc)
# anova(m1)
summary(m1)

m1b <- lmer(log(ge) ~ log(initialWeight) + (1|hostFamily) + (1|catSpecies), data = cwc)
summary(m1b)
cwc$m1bResid <- resid(m1b)
boxplot(m1bResid ~ hostNative, data = cwc)

m1c <- lmer(ge ~ hostNative + initialWeight + (1|hostFamily) + (1|catSpecies), data = cwc)
summary(m1c)
performance::check_model(m1c)


(ggplot(data = cwc, aes(x = hostNative, y = m1bResid)) +
  geom_violin()) +
(ggplot(data = cwc, aes(x = hostNative, y = ge)) +
  geom_violin())

performance::check_model(m1c)

cw <- cw %>%
  mutate(isToid = ifelse(fate == "T", 1, 0)) %>%
  mutate(isPupal = ifelse(grepl("O|P", fate, ignore.case = T), 1, 0)) %>%
  mutate(isDead = ifelse(fate == "D", 1, 0)) %>%
  mutate(isMissing = ifelse(fate %in% c("MIA","K"), 1, 0)) 

biToid <- cw %>%
  filter(isDead == 0 & isMissing == 0) %>%
  group_by(treeSpecies) %>%
  summarise(tot = n(), toided = sum(isToid), family = first(hostFamily), native = first(hostNative))

biToid2 <- cw %>%
  filter(isDead == 0 & isMissing == 0) %>%
  group_by(catSpecies, hostNative, hostFamily) %>%
  summarise(tot = n(), toided = sum(isToid))
  

biPupal <- cw %>%
  filter(isToid == 0 & isMissing == 0) %>%
  group_by(treeSpecies) %>%
  summarise(tot = n(), pupal = sum(isPupal), family = first(hostFamily), native = first(hostNative)) %>%
  filter(tot > 5) %>%
  arrange(family)

biPupal2 <- cw %>%
  filter(isToid == 0 & isMissing == 0) %>%
  group_by(catSpecies, hostNative, hostFamily) %>%
  summarise(tot = n(), pupal = sum(isPupal)) %>%
  mutate(rate = pupal/tot)


m2 <- glm(cbind(toided, tot - toided) ~ native + family, data = biToid, family = "binomial")
summary(m2)

m2b <- glmer(cbind(toided, tot - toided) ~ hostNative + hostFamily + (1|catSpecies), data = biToid2, family = "quasibinomial")
summary(m2b)

m3 <- glmer(cbind(pupal, tot - pupal) ~ native + (1|family), data = biPupal, family = "binomial")
summary(m3)

m3b <- glmer(cbind(pupal, tot - pupal) ~ hostNative + (1|hostFamily) + (1|catSpecies), data = biPupal2, family = "binomial")
summary(m3b)

performance::check_model(m2)



ggplot(data = biPupal2, aes(x = hostNative, y = rate)) +
  geom_violin()


pwp <- pwp[!grepl("C|L", pwp$catNum), ]
pwp <- pwp[!pwp$catSpecies %in% c("GEOMXX", "MICROX"), ]
hist(pwp$pWeight[!pwp$catSpecies %in% "CERAUN"])
boxplot(pWeight ~ hostNative, data = pwp[!pwp$catSpecies %in% "CERAUN",])
pwp$pWeightLog <- log(pwp$pWeight)

m4 <- lmer(pWeightLog ~ hostNative + (1|hostFamily) + (1|catSpecies), data = pwp[!pwp$catSpecies %in% "CERAUN",])
summary(m4)

plot(m4)
performance::check_model(m4)

ggplot(data = pwp[!pwp$catSpecies %in% "CERAUN",], aes(x = hostNative, y = pWeightLog)) +
  geom_violin() +
  theme_bw()

nate <- data.frame(table(pwp$catSpecies, pwp$hostNative)) %>%
  pivot_wider(names_from = Var2, values_from = Freq) %>%
  filter(exotic != 0 & native != 0) %>%
  rename(catSpecies = Var1)

ggplot(data = pwp[pwp$catSpecies %in% nate$catSpecies, ], aes(x = hostNative, y = pWeight, group = catSpecies, color = catSpecies)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") + 
  geom_point(aes(group = NA), stat = "summary", fun = "mean", size = 3) +
  geom_line(aes(group = NA), stat = "summary", fun = "mean") +
  theme_tufte()

# gonna try to make the 4 part plot robi wanted I wonder how many species it will fit ####
# let's try to make it on all

cws <- cw[!cw$catSpecies %in% c(dontInclude, "MAYBE PSEUCY OR PALTAN", "NOT LITHXX", "PALEXX", "BUTTTX"), ]

cws$wtChange <- cws$finalWeight - cws$initialWeight 
cws$wtChangePer <- cws$wtChange/cws$initialWeight
cws$ge <- cws$wtChange/cws$frassWeight
cws <- cws[cws$ge >= 0 | is.na(cws$ge), ]

cac <- cws %>%
  group_by(catSpecies, hostNative) %>%
  summarise(toidRate = round(sum(isToid)/n(), 2),
            growthRate = round(mean(ge, na.rm = T), 2),
            count = n(),
            toids = sum(isToid)) %>%
  # filter(!is.nan(growthRate))
  filter(!catSpecies %in% "RHEUPR")

goodCats <- cac$catSpecies[duplicated(cac$catSpecies)]

cac2 <- cac %>%
  filter(catSpecies %in% goodCats) %>%
  group_by(catSpecies) %>%
  summarise(dToid = last(toidRate) - first(toidRate),
         dGrowth = last(growthRate) - first(growthRate),
         dCount = last(count) - first(count),
         size = first(count) + last(count),
         totToid = first(toids) + last(toids))
  # filter(totToid != 0)

ggplot(data = cac2, aes(x = dGrowth, y = dToid)) +
  geom_point(aes(size = size)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(label = catSpecies), hjust = -.15, vjust = -.15) +
  xlab("Efficiency") +
  ylab("Parasitoids") +
  theme_tufte() +
  scale_y_continuous(breaks = c(-.2, .2),
                     labels = c("More on invasive", "More on native")) +
  scale_x_continuous(breaks = c(-.2, .2),
                     labels = c("Efficient on invasive", "Efficient on native"))
  

summary(lm(dGrowth ~ dToid, data = cac2))





ggplot(data = cwc, aes(x = hostNative, y = ge)) +
  geom_boxplot()

pwp$hostNative <- relevel(as.factor(pwp$hostNative), "native")

ggplot(data = pwp[!pwp$hostFamily %in% "InvasiveOutgroups",], aes(x = hostFamily, y = pWeight, group = hostNative, fill = hostNative)) +
  geom_bar(stat = "summary", position = "dodge",  fun = "mean", color = "white") +
  geom_errorbar(stat = "summary", position = position_dodge(.9), size = .2, width = .25, color = "white") +
  theme_tufte(base_size = 24) +
  theme(panel.border = element_rect(colour = "#0d0d0d", fill = NA),
        panel.background = element_rect(fill = "#0d0d0d"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#0d0d0d"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        text = element_text(colour = "white")) +
  labs(x = "\nHost plant family",
       y = "Mean pupal weight (g)\n") +
  scale_fill_manual(name = "Host nativeness", 
                      labels = c("Native", "Exotic"),
                      values = c("grey", "#0d0d0d"))

cwc$hostNative <- relevel(as.factor(cwc$hostNative), "native")

ggplot(data = cwc[!cwc$hostFamily %in% "InvasiveOutgroups",], aes(x = hostFamily, y = ge, group = hostNative, fill = hostNative)) +
  geom_bar(stat = "summary", position = "dodge",  fun = "mean", color = "white") +
  geom_errorbar(stat = "summary", position = position_dodge(.9), size = .2, width = .25, color = "white") +
  theme_tufte(base_size = 24) +
  theme(panel.border = element_rect(colour = "#0d0d0d", fill = NA),
        panel.background = element_rect(fill = "#0d0d0d"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#0d0d0d"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        text = element_text(colour = "white")) +
  labs(x = "\nHost plant family",
       y = "Growth efficiency\n") +
  scale_fill_manual(name = "Host nativeness", 
                    labels = c("Native", "Exotic"),
                    values = c("grey", "#0d0d0d"))

biPupal2$hostNative <- relevel(as.factor(biPupal2$hostNative), "native")

ggplot(data = biPupal2[!biPupal2$hostFamily %in% "InvasiveOutgroups",], aes(x = hostFamily, y = rate, group = hostNative, fill = hostNative)) +
  geom_bar(stat = "summary", position = "dodge",  fun = "mean", color = "white") +
  geom_errorbar(stat = "summary", position = position_dodge(.9), size = .2, width = .25, color = "white") +
  theme_tufte(base_size = 24) +
  theme(panel.border = element_rect(colour = "#0d0d0d", fill = NA),
        panel.background = element_rect(fill = "#0d0d0d"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#0d0d0d"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        text = element_text(colour = "white")) +
  labs(x = "\nHost plant family",
       y = "Pupation rate\n") +
  scale_fill_manual(name = "Host nativeness", 
                    labels = c("Native", "Exotic"),
                    values = c("grey", "#0d0d0d"))
  
biToid2$hostNative <- relevel(as.factor(biToid2$hostNative), "native")
biToid2$rate <- biToid2$toided/biToid2$tot

ggplot(data = biToid2[!biToid2$hostFamily %in% "InvasiveOutgroups",], aes(x = hostFamily, y = rate, group = hostNative, fill = hostNative)) +
  geom_bar(stat = "summary", position = "dodge",  fun = "mean", color = "white") +
  geom_errorbar(stat = "summary", position = position_dodge(.9), size = .2, width = .25, color = "white") +
  theme_tufte(base_size = 24) +
  theme(panel.border = element_rect(colour = "#0d0d0d", fill = NA),
        panel.background = element_rect(fill = "#0d0d0d"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#0d0d0d"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        text = element_text(colour = "white")) +
  labs(x = "\nHost plant family",
       y = "Parasitoid rate\n") +
  scale_fill_manual(name = "Host nativeness", 
                    labels = c("Native", "Exotic"),
                    values = c("grey", "#0d0d0d"))

