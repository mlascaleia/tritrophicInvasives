# initialize ####

source("analysis/01_makeGrowthData.R")

library(glmmTMB)

# organize data for non-ge models ####

uc <- cc %>%
  filter(changeDir == "unchanged") %>%
  # some cats I know we dumped...
  filter(!(fate == "" & catSpecies %in% c("MORRCO", "HEMATH", 
                                        "MICROX", "CROCNO",
                                        "PAONEX", "CALLDR",
                                        "ELAPVE", "LAMBFI",
                                        "ZALEXX"))) %>%
  # Others I suspect died
  mutate(fate = ifelse(fate == "", "D", fate))

# double check we didn't miss some toids?

toidCheck <- read.csv("data/dirty/cat/toidCheck.csv") %>%
  filter(!str_detect(Number, "[[:alpha:]]{2}[[:digit:]]{5}")) %>%
  mutate(Number = gsub("T", "", Number))

actuallyToided <- c("J2843", "J2539")
uc$fate[uc$catNum %in% actuallyToided] <- "T"

# make some things clearer...
uc$fate[uc$fate %in% c("E", "U", "OW", "P", "PP", "LPP")] <- "pupal"
uc$fate[uc$fate %in% c("G", "K", "MIA")] <- "killed"
uc$fate[uc$fate %in% c("D")] <- "died"
uc$fate[uc$fate %in% c("T")] <- "toided"

uc <- uc %>%
  mutate(isToid = ifelse(fate == "toided", 1, 0)) %>%
  mutate(isPupal = ifelse(fate == "pupal", 1, 0)) %>%
  mutate(isDead = ifelse(fate == "died", 1, 0)) %>%
  mutate(isMissing = ifelse(fate == "killed", 1, 0)) %>%
  filter(!hostFamily %in% "InvasiveOutgroups")

biToid <- uc %>%
  filter(isDead == 0 & isMissing == 0) %>%
  group_by(catSpecies, hostNative, hostFamily) %>%
  summarise(tot = n(), 
            toided = sum(isToid),
            noToid = n() - sum(isToid),
            avgDate = mean(jDate)) %>%
  mutate(rate = toided/tot)

biPupal <- uc %>%
  filter(isToid == 0 & isMissing == 0) %>%
  group_by(catSpecies, hostNative, hostFamily) %>%
  summarise(tot = n(), 
            pupal = sum(isPupal),
            noPupa = n() - sum(isPupal),
            avgDate = mean(jDate)) %>%
  mutate(rate = pupal/tot)

# run binomial model ####
# 
# m2 <- glmmTMB(cbind(toided, noToid) ~ hostNative + hostFamily + (1|catSpecies), 
#               data = biToid, family = "binomial")
# summary(m2)

# make bernouli model

toidest <- uc %>%
  filter(fate %in% c("pupal", "toided")) %>%
  mutate(hostFamily = factor(hostFamily),
         hostNative = factor(hostNative)) %>%
  mutate(jDate = scale(jDate)) %>%
  dplyr::select(catNum, catSpecies, isToid, transect, 
         treeSpecies, hostFamily, hostNative,
         year, jDate)

# library(lme4)

m2a <- glmmTMB(isToid ~ hostNative * hostFamily +
                 (1|transect) + (1|catSpecies), 
              data = toidest, family = "binomial")

summary(m2a)

m2.rose <- glmmTMB(isToid ~ hostNative +
                     year + jDate + 
                 (1|transect) + (1|catSpecies), 
               data = toidest[toidest$hostFamily %in% "Roseaceae", ], 
               family = "binomial")

summary(m2.rose)

m2.olive <- glmmTMB(isToid ~ hostNative +
                     year + jDate + 
                     (1|transect) + (1|catSpecies), 
                   data = toidest[toidest$hostFamily %in% "Oleaceae", ], 
                   family = "binomial")

summary(m2.olive)

m2.caprid <- glmmTMB(isToid ~ hostNative +
                      year + jDate + 
                      (1|transect) + (1|catSpecies), 
                    data = toidest[toidest$hostFamily %in% "Caprifoliaceae", ], 
                    family = "binomial")

summary(m2.caprid)

# run pupal model ####

m3 <- glmmTMB(cbind(pupal, noPupa) ~ hostNative + hostFamily + (1|catSpecies), 
            data = biPupal, family = "binomial")
summary(m3)

# organize data for pupal weight ####
pwp <- pwp[!grepl("C|L", pwp$catNum), ]
pwp <- pwp[!pwp$catSpecies %in% c("GEOMXX", "MICROX"), ]
hist(pwp$pWeight[!pwp$catSpecies %in% "CERAUN"])
# boxplot(pWeight ~ hostNative, data = pwp[!pwp$catSpecies %in% "CERAUN",])
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

