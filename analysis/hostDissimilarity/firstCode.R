# initialize ####

rm(list = ls())

library(tidyverse)
library(vegan)
library(igraph)

# load data #####

load("data/clean/cleanBranch.rdata")
load("data/clean/catWeights.rdata")

# make interaction matrix

se <- sameB %>%
  group_by(treeSpecies) %>%
  summarise(nBranches = n_distinct(bid))

dontInclude <- c("LYMADI", "MICROX", "NOT ORTHRU", "GEOMXX", "NOT ZALE", "NOCTXX",
                 "UNKNXX", "UNIDXX", "FOURLX", "CERAUN", "HYALCE", "EREBXX", "RHEUPR")

catCount <- cw %>%
  filter(!catSpecies %in% dontInclude) %>%
  filter(!treeSpecies %in% c("RUBPH", "RHOSC")) %>%
  group_by(treeSpecies, catSpecies) %>%
  summarise(count = n()) %>%
  left_join(se) %>%
  mutate(count = count/nBranches) %>%
  select(-nBranches) %>%
  pivot_wider(names_from = catSpecies, values_from = count, values_fill = 0) %>%
  remove_rownames %>% 
  column_to_rownames(var = "treeSpecies") %>%
  as.matrix()

disses <- vegdist(catCount)%>%
  as.matrix()
  
disses[lower.tri(disses)] <- 0 
  
disses <- disses %>%  
  as.data.frame() %>%
  mutate(tree1 = row.names(catCount)) %>%
  pivot_longer(cols = AMEXX:VIBSI, names_to = "tree2", values_to = "dissim") %>%
  filter(!tree1 == tree2) %>%
  filter(dissim != 0)


mean(disses[(disses$tree1 %in% "VIBDI" & disses$tree2 %in% c("LONMO","SAMCA", "VIBAC", "VIBDE", 
"VIBDI", "VIBLE", "VIBPL")) |
  (disses$tree2 %in% "VIBDI" & disses$tree1 %in% c("LONMO","SAMCA", "VIBAC", "VIBDE", 
                                                   "VIBDI", "VIBLE", "VIBPL")) , ]$dissim)

mean(disses[(disses$tree1 %in% "VIBDI" & disses$tree2 %in% c("LONMO", "VIBPL", "ROSMU", "BERTH",
                                                        "EUOAL", "LIGOB")) |
         (disses$tree2 %in% "VIBDI" & disses$tree1 %in% c("LONMO", "VIBPL", "ROSMU", "BERTH",
                                                          "EUOAL", "LIGOB")) , ]$dissim)

dput(unique(disses$tree1))



