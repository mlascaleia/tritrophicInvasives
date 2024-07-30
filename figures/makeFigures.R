# ggplot(data = pwp[pwp$catSpecies %in% nate$catSpecies, ], aes(x = hostNative, y = pWeight, group = catSpecies, color = catSpecies)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line(stat = "summary", fun = "mean") + 
#   geom_point(aes(group = NA), stat = "summary", fun = "mean", size = 3) +
#   geom_line(aes(group = NA), stat = "summary", fun = "mean") +
#   theme_tufte()

# gonna try to make the 4 part plot robi wanted I wonder how many species it will fit ####
# let's try to make it on all

ccs <- cc[!cc$catSpecies %in% c(dontInclude, "MAYBE PSEUCY OR PALTAN", "NOT LITHXX", "PALEXX", "BUTTTX"), ]

ccs$wtChange <- ccs$finalWeight - ccs$initialWeight 
ccs$wtChangePer <- ccs$wtChange/ccs$initialWeight
ccs$ge <- ccs$wtChange/ccs$frassWeight
ccs <- ccs[ccs$ge >= 0 | is.na(ccs$ge), ]

cac <- ccs %>%
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





ggplot(data = geValid, aes(x = hostNative, y = ge)) +
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

geValid$hostNative <- relevel(as.factor(geValid$hostNative), "native")
geValidi <- geValid[!geValid$hostFamily %in% "InvasiveOutgroups",]

ggplot(data = geValidi, aes(x = hostFamily, y = ge, fill = hostNative)) +
  geom_boxplot(position = "dodge", color = "white") + 
  # geom_errorbar(stat = "summary", position = position_dodge(.9), size = .2, width = .25, color = "white") +
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

# make real figures ####


geValid$hostNative <- relevel(as.factor(geValid$hostNative), "native")
geValidi <- geValid[!geValid$hostFamily %in% "InvasiveOutgroups",]

bu1 <- ggplot(data = geValidi, aes(x = hostFamily, y = ge, fill = hostNative)) +
  geom_pointrange(position = "dodge", color = 'black') +
  theme_tufte(base_size = 24) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = "\nHost plant family",
       y = "Growth efficiency\n") +
  scale_fill_manual(name = "Host nativeness", 
                    labels = c("Native", "Exotic"),
                    values = c("#3A577E", "#6FBCEB"))

pwp$hostNative <- relevel(as.factor(pwp$hostNative), "native")
pwpi <- pwp[!pwp$hostFamily %in% "InvasiveOutgroups" &
              !pwp$catSpecies %in% "CERAUN",]

bu2 <- ggplot(data = pwpi, aes(x = hostFamily, y = pWeight, fill = hostNative)) +
  geom_pointrange(position = "dodge", color = 'black') +
  theme_tufte(base_size = 24) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = "\nHost plant family",
       y = "Pupal Weight\n") +
  scale_fill_manual(name = "Host nativeness", 
                    labels = c("Native", "Exotic"),
                    values = c("#3A577E", "#6FBCEB"))

cc$hostNative <- relevel(as.factor(cc$hostNative), "native")
cci <- cc[!cc$hostFamily %in% "InvasiveOutgroups",]
ccip <- cc[cc$isToid == 0 & cc$isMissing == 0, ]

ggPup <- ccip %>%
  filter(hostFamily != "InvasiveOutgroups") %>%
  group_by(hostFamily, hostNative) %>%
  summarise(pRate = mean(isPupal),
            pSE = sd(isPupal)/sqrt(n()))


bu3 <- ggplot(data = ggPup,
              aes(x = hostFamily, y = pRate , color = hostNative)) +
  geom_pointrange(stat = "identity", position = position_dodge(width = 1),
                  mapping = aes(ymin = pRate - pSE, ymax = pRate + pSE)) +
  theme_tufte(base_size = 24) +
  theme(legend.position = "none") +
  labs(x = "\nHost plant family",
       y = "Pupation rate\n") +
  scale_color_manual(name = "Host nativeness", 
                     labels = c("Native", "Exotic"),
                     values = c("#3A577E", "#6FBCEB")) +
  ylim(0,1)

bu3

ccit <- cc[cc$isDead == 0 & cc$isMissing == 0, ]

ggToid <- ccit %>%
  filter(hostFamily != "InvasiveOutgroups") %>%
  group_by(hostFamily, hostNative) %>%
  summarise(tRate = mean(isToid),
            tSE = sd(isPupal)/sqrt(n()))


td1 <- ggplot(data = ggToid,
              aes(x = hostFamily, y = tRate , color = hostNative)) +
  geom_pointrange(stat = "identity", position = position_dodge(width = 1),
                  mapping = aes(ymin = tRate - tSE, ymax = tRate + tSE)) +
  theme_tufte(base_size = 24) +
  theme(legend.position = "none") +
  labs(x = "\nHost plant family",
       y = "Parasitoid rate\n") +
  scale_color_manual(name = "Host nativeness", 
                     labels = c("Native", "Exotic"),
                     values = c("red", "orange")) +
  ylim(0,1)


(bu1 + bu2) / (bu3 + td1)






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
