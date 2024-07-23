# initialize ####

source("analysis/02_makeGrowthModels.R")

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

# get good cat species
cats.good <- table(uc$catSpecies, uc$hostNative) %>%
  as.data.frame() %>%
  pivot_wider(id_cols = Var1, names_from = Var2, values_from = Freq) %>%
  rename(catSpecies = Var1) %>%
  filter(exotic > 0 & native > 0) 

uc <- uc %>%
  filter(catSpecies %in% cats.good$catSpecies)

# make toid model

toidest <- uc %>%
  filter(fate %in% c("pupal", "toided")) %>%
  mutate(hostFamily = factor(hostFamily),
         hostNative = factor(hostNative)) %>%
  mutate(jDate = scale(jDate)) %>%
  dplyr::select(catNum, catSpecies, isToid, transect, 
         treeSpecies, hostFamily, hostNative,
         year, jDate)

# library(lme4)

m.toid <- glmmTMB(isToid ~ hostNative * hostFamily +
                 year + (1|transect) + (1|catSpecies), 
              data = toidest, family = "binomial")

summary(m.toid)

gl.toid <- glht(m.toid, linfct = c("hostNativenative = 0", 
                                   "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                                   "hostNativenative + hostNativenative:hostFamilyRoseaceae = 0"))

summary(gl.toid, test = adjusted(type = "none"))

m.toid2 <- glmmTMB(isToid ~ hostNative + hostFamily +
                    year + (1|transect) + (1|catSpecies), 
                  data = toidest, family = "binomial")

summary(m.toid2)

# make pupal model ####

pupest <- uc %>%
  filter(fate %in% c("died", "pupal")) %>%
  mutate(hostFamily = factor(hostFamily),
         hostNative = factor(hostNative)) %>%
  mutate(jDate = scale(jDate)) %>%
  dplyr::select(catNum, catSpecies, isPupal, transect, 
                treeSpecies, hostFamily, hostNative,
                year, jDate)

m.pupal <- glmmTMB(isPupal ~ hostNative * hostFamily +
                    year + (1|transect) + (1|catSpecies), 
                  data = pupest, family = "binomial")

summary(m.pupal)

gl.pupa <- glht(m.pupal, linfct = c("hostNativenative = 0", 
                                    "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                                    "hostNativenative + hostNativenative:hostFamilyRoseaceae = 0"))

summary(gl.pupa, test = adjusted(type = "none"))

m.pupal2 <- glmmTMB(isPupal ~ hostNative + hostFamily +
                     year + (1|transect) + (1|catSpecies), 
                   data = pupest, family = "binomial")

summary(m.pupal2)

# organize data for pupal weight ####
load("data/clean/catWeights21.rdata")
rm(cw)
pwp <- pwp[!grepl("C|L", pwp$catNum), ]
pwp <- pwp[!pwp$catSpecies %in% c("GEOMXX", "MICROX", "CERAUN"), ]
pwp$pWeightLog <- log(pwp$pWeight)

pwp2 <- cc %>%
  dplyr::select(catNum, transect) %>%
  right_join(pwp) %>%
  filter(hostFamily != "InvasiveOutgroups")

pw.good <- table(pwp2$catSpecies, pwp2$hostNative) %>%
  as.data.frame() %>%
  pivot_wider(id_cols = Var1, names_from = Var2, values_from = Freq) %>%
  rename(catSpecies = Var1) %>%
  filter(exotic > 0 & native > 0) 

pwp2 <- pwp2 %>%
  filter(catSpecies %in% pw.good$catSpecies)

m.pw <- glmmTMB(pWeightLog ~ hostNative * hostFamily +
                (1|transect) + (1|catSpecies), 
                data = pwp2)
summary(m.pw)

gl.pw <- glht(m.pw, linfct = c("hostNativenative = 0", 
                               "hostNativenative + hostNativenative:hostFamilyOleaceae = 0",
                               "hostNativenative + hostNativenative:hostFamilyRoseaceae = 0"))

summary(gl.pw, test = adjusted(type = "none"))

m.pw2 <- glmmTMB(pWeightLog ~ hostNative + hostFamily +
                  (1|transect) + (1|catSpecies), 
                data = pwp2)
summary(m.pw2)



