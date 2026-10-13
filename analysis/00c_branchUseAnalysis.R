# make analysis of how much caterpillars are using native vs. exotic
# low-stakes claim, just trying to say that they use them to a meaningful degree
# without fully and absolutely defining what "meaningful" is

# make the cc object, which need bid added back in
# source("analysis/00b_makeCleanerCaterpillarData.R")

library(glmmTMB)

# get 2021 observations because no effort was emasured in 2022 (ugh)
cc21 <- cc %>% 
  filter(year == 2021)

# get only branches of species that actually made it to analysis
br <- branches21 %>%
  filter(!treeSpecies %in% c(treesDropped, "VIBOP"), # cant forget vibop
         hostFamily != "InvasiveOutgroups")

# counts per branch by caterpillar species (big mostly empty table)
counts <- cc21 %>% count(bid, catSpecies, name = "count")

# limit host range per caterpillar (defined here)
catFam <- cc %>%
  distinct(catSpecies, hostFamily) %>%
  mutate(hostFamily = as.character(hostFamily))

# make the big empty loooong table with
# 0s only for caterpillars that /could/ have been found somewhere
bu <- br %>%
  inner_join(catFam, by = "hostFamily", relationship = "many-to-many") %>%
  left_join(counts, by = c("bid", "catSpecies")) %>%
  mutate(count = replace_na(count, 0L)) %>%
  # drop species with no 2021 records
  group_by(catSpecies) %>%
  filter(sum(count) > 0) %>%
  ungroup() %>%
  mutate(hostNative = factor(hostNative, levels = c("native", "exotic")),
         hostFamily = factor(hostFamily),
         transect = factor(transect))

# model ####
m.use <- glmmTMB(count ~ hostNative + hostFamily +
                   (1 + hostNative | catSpecies) +
                   (1 | treeSpecies) + (1 | transect),
                 family = nbinom2, data = bu)

summary(m.use)
# no obvious or measurable difference between native or exotic 
# (with many monstrous caveats)







