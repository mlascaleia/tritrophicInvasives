
library(tidyverse)
library(ggthemes)


# This script is for making quick and dirty figures of all my data

c21 <- load("data/clean/catWeights21.rdata")

cw

c22 <- read.csv("data/badButFast/cat22.csv")

# c22 is a disaster and at least needs a once-over


bads <- c("BROWNX", "LEPXX", "MINEXX", "PALEXX", "ROUGEX", "PUPAXX", "ROUGEXX", "SHELLX", "SPIKEX", "TOIDXX", "UNKNXX", "WIGGLX", "MICROX")
badTrees <- c("BERTH", "CELOR", "ELEAN", "PRUVI", "SAMCA")


c22 <- c22 %>%
  filter(!catSpecies %in% "") %>%
  filter(!catSpecies %in% bads) %>%
  filter(!treeSpecies %in% badTrees) %>%
  filter(collected. %in% "Y") %>%
  filter(!fate %in% c("","U"))

table(c22$treeSpecies)

invasives <- c("BERTH", 'EUOAL', 'LIGOB',"LONTA", 'LONMO', 'MALXX', 'ROSMU','VIBDI','VIBPL','VIBSI', 'RHOSC', "RUBPH",
               "MALFL", 'MALPR', 'MALPU', 'MALSI')
c22$hostNative <- "native"
c22$hostNative[c22$treeSpecies %in% invasives] <- "exotic"

c22$hostFamily <- "Rosaceae"
c22$hostFamily[c22$treeSpecies %in% c("LONTA","LONMO","VIBDE","VIBDI","VIBAC", "VIBLE", "VIBOP", "VIBPL","VIBSI")] <- 'Caprifoliaceae'
c22$hostFamily[c22$treeSpecies %in% c("FRAAM", "LIGOB")] <- 'Oleaceae'

c22$fate[c22$fate %in% c("E", "P ", "PP")] <- "P"
c22$fate[c22$fate %in% c("MIA", "G")] <- "K"

table(cw$fate)

cw$fate[cw$fate %in% c("PP", "OW", "LPP")] <- "P"
cw$fate[cw$fate %in% c("K", "MIA")] <- "K"

# total toid data

t22 <- c22 %>%
  group_by(hostFamily, hostNative, fate) %>%
  summarise(total = n())

t21 <- cw %>%
  group_by(hostFamily, hostNative, fate) %>%
  summarise(total = n())

yo <- rbind(t21, t22) %>%
  group_by(hostFamily, hostNative, fate) %>%
  summarise(total = sum(total)) %>%
  filter(!hostFamily %in% "InvasiveOutgroups")

yo_jt <- yo %>%
  filter(fate %in% c("T", "P")) %>%
  pivot_wider(names_from = fate, values_from = total) %>%
  mutate(toidRate = (`T`)/(`T` + P)) %>%
  mutate(se = sqrt(toidRate * (1 - toidRate)/(`T` + P)))

yo_dt <- yo %>%
  filter(fate %in% c("D","T", "P")) %>%
  pivot_wider(names_from = fate, values_from = total) %>%
  mutate(toidRate = (D + `T`)/(D + `T` + P)) %>%
  mutate(se = sqrt(toidRate * (1 - toidRate)/(D + `T` + P)))

ggplot(data = yo_jt, aes(x = hostFamily, y = toidRate, group = hostNative, fill = hostNative)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  geom_errorbar(aes(ymin = toidRate - se, ymax = toidRate + se),
                stat = "identity", position = position_dodge(.9), size = .2, width = .25, color = "white") +
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









