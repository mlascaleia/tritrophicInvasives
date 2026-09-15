# the purpose of this script is to create the caterpillar data that I will
# use throughout the script.

# as things currently stand I swap up this data like 9 times
# and simply cannot keep track of anything

# initialize ####

source("analysis/00_makeCaterpillars.R")

cc <- cc %>%
  # remove unnecessary columns
  dplyr::select(-newHost, -newHostFamily, -newHostNative,
                -captureWeight) %>%
  # remove plant species that will not be included
  filter(!treeSpecies %in% c("BERTH", "CELOR", "EUOAL", "RHOSC", "SAMCA")) %>%
  mutate(hostFamily = factor(hostFamily),
         hostNative = factor(hostNative))

# establish caterpillar species that are being kept
cats.good <- table(cc$catSpecies, cc$hostNative) %>%
  as.data.frame() %>%
  pivot_wider(id_cols = Var1, names_from = Var2, values_from = Freq) %>%
  rename(catSpecies = Var1) %>%
  filter(exotic > 0 & native > 0) 

# eliminate cats that are known mistakes or purposefully excluded
# CORYME = Rheumaptera meadii, known barberry specialist
# MICROX = Unidentified microlepidopteran to be excluded
# GEOMXX = Unidentified geometrid to be excluded
# NOCTXX = Unidentified noctuid to be excluded
# UNKNXX = Unidentified lepidopteran to be excluded
cats.good <- cats.good %>%
  filter(!catSpecies %in% c("CORYME", "MICROX", "GEOMXX", "NOCTXX","UNKNXX"))

# remove caterpillars that were not found on both natives and invasives

cc <- cc %>%
  filter(catSpecies %in% cats.good$catSpecies)

# Now clarify what happened to all the caterpillars for
# the parasitoid and pupal mass models

# stands for emerged, pUpal, overwintered, photographed, prepupal, and likely prepupal
cc$fate[cc$fate %in% c("E", "U", "OW", "P", "P ", "PP", "LPP")] <- "pupal"

# caterpillars that will be eliminated. stands for gone, killed, missing in action, or unknown
cc$fate[cc$fate %in% c("G", "K", "MIA", "")] <- "killed"

# straightforward catergories
cc$fate[cc$fate %in% c("D")] <- "died"
cc$fate[cc$fate %in% c("T")] <- "toided"

# columns for the binomial analyses to come
cc <- cc %>%
  mutate(isToid = ifelse(fate == "toided", 1, 0)) %>%
  mutate(isPupal = ifelse(fate == "pupal", 1, 0)) %>%
  mutate(isDead = ifelse(fate == "died", 1, 0)) %>%
  mutate(isMissing = ifelse(fate == "killed", 1, 0))

# add one more for the overall death analysis

cc <- cc %>%
  mutate(isDeceased = ifelse(fate %in% c("toided", "died"), 1, 0))

# make the parasitoid dataset
toidest <- cc %>%
  # only those that pupated or were parasitoided are included
  filter(fate %in% c("pupal", "toided"))

# make the pupation (survival) dataset
pupest <- cc %>%
  # only those that died of unknown causes or pupated count for survival as a "bottom-up" consideration
  filter(fate %in% c("died", "pupal"))

# One thing to consider is that some species (PSEUCY, ELAPVE in toidest)
# no longer have both exotic and native representation in these datasets,
# as they are subsets of the larger datasets based on fate

# make the pupal weight dataset

# organize data for pupal weight ####
load("data/clean/catWeights21.rdata")
pwp$hostFamily[pwp$hostFamily %in% "Roseaceae"] <- "Rosaceae"
rm(cw)
pwp <- pwp[!grepl("C|L", pwp$catNum), ]
pwp <- pwp[pwp$catSpecies %in% cats.good$catSpecies, ] %>%
  filter(!treeSpecies %in% c("BERTH", "CELOR", "EUOAL", "RHOSC", "SAMCA"))


# fix 10x issues...
pwp$pWeight[pwp$catNum %in% c("K4249")] <- 
  pwp$pWeight[pwp$catNum %in% c("K4249")]/10

# remove dead (dreid/failed) pupa
pwp <- pwp[!pwp$catNum %in% c("J2171", "J2173"), ]

# set up as log analysis
pwp$pWeightLog <- log(pwp$pWeight)

pwp <- cc %>%
  dplyr::select(catNum, transect) %>%
  right_join(pwp) %>%
  filter(hostFamily != "InvasiveOutgroups")

# fix persistent issue with missing transects
pwp$transect[pwp$catNum %in% "J2746"] <- 130
pwp$transect[pwp$catNum %in% "K4241"] <- 207

pw.good <- table(pwp$catSpecies, pwp$hostNative) %>%
  as.data.frame() %>%
  pivot_wider(id_cols = Var1, names_from = Var2, values_from = Freq) %>%
  rename(catSpecies = Var1) %>%
  filter(exotic > 0 & native > 0) 

pwp2 <- pwp %>%
  filter(catSpecies %in% pw.good$catSpecies)

# I'm at a loss here. Did we really not record pupal weight in 2022??

# anyway, I can't believe I have to do this but I need to establish families
# for the summary stats section

cats.good$catSpecies
fams <- c("No", "Ac", "Ge", "Ge", "Er", "No", "Ge", "No", "Ge", "Ge", "Ge",
          "No", "Ge", "Nt", "No", "Ge", "Er", "Ge", "Ge", "Ge", "No", "Ge",
          "Ge", "Ge", "No", "No", "Ge", "Er", "No", "Ge", "No", "Ge", "Ge",
          "Dr", "Er", "Nt", "Er", "Er", "He")
cats.good$fam <- fams

cats.good %>%
  group_by(fam) %>%
  summarise(total = sum(native) + sum(exotic)) %>%
  mutate(prop = total/1114)

cc %>% 
  group_by(fate) %>%
  summarise(total = n()) %>%
  mutate(prop = total/1114)





