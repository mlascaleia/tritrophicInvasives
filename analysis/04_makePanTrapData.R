# initialize

# rm(list = ls())

source("analysis/03_makeOtherModels.R")

library(vegan)

cover <- read.csv("data/dirty/traps/trapCover.csv", fileEncoding="UTF-8-BOM")
info <- read.csv("data/dirty/traps/trapInfo.csv", fileEncoding="UTF-8-BOM")
clays <- read.csv("data/dirty/traps/trapClays.csv", fileEncoding="UTF-8-BOM")

# clean cover

cover$treeID[cover$treeID %in% "CALLA"] <- "KALLA"
cover$treeID[cover$treeID %in% "CARXX"] <- "CARYA"
cover$treeID[cover$treeID %in% c("CUEVE", "CEUEVE")] <- "QUEVE"
cover$treeID[cover$treeID %in% "FILUL"] <- "VITAL"
cover$treeID[cover$treeID %in% "LIBE"] <- "LINBE"
cover$treeID[cover$treeID %in% "LIBGOB"] <- "LINBE"
cover$treeID[cover$treeID %in% c("MALFL", "MALPR",
                                 "MALPU", "MALSI",
                                 "CALFL")] <- "MALXX"
cover$treeID[cover$treeID %in% "PRUVE"] <- "PRUSE"
cover$treeID[cover$treeID %in% "QUERA"] <- "QUERU"
cover$treeID[cover$treeID %in% "RUBSS"] <- "RUBXX"

cover$trap[cover$trap %in% "TQO2B"] <- "TQ02B"

# dput(unique(cover$treeID))

trees <- c("PRUSE", "ELEAN", "CARCA", "BERTH", "ROSMU", "CELOR", "AMEXX", 
           "MALXX", "PARQU", "EUOAL", "LIGOB", "VIBDI", "RUBXX", "CARYA", 
           "TOXRA", "CRAXX", "VIBAC", "QUEAL", "VACCO", "ACERU", "PINST", 
           "TSUCA", "VITAL", "ACESA", "FRAAM", "FAGGR", "LINBE", "ULMRU", 
           "CORSA", "QUERU", "HAMVI", "QUEVE", "LONGL", "KALLA", "BETLE", 
           "OSTVI", "COMPE", "LONMO", "ZZ", "VIBLE", "BETPO", "VIBSI", "VIBPL"
)

inv <- c(1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, NA, 1, 
         1, 0, 0)

treeCodes <- cbind.data.frame(treeID = trees, invasive = as.integer(!inv))

cover <- merge(cover, treeCodes, by = "treeID")

cmat.full <- cover %>%
  filter(!cover == -99) %>%
  group_by(trap, treeID) %>%
  summarise(cover = sum(cover)) %>%
  ungroup() %>%
  pivot_wider(id_cols = trap, 
              values_from = cover, 
              values_fill = 0,
              names_from = treeID) %>%
  column_to_rownames("trap") %>%
  as.matrix()

cmat.nat <- cover %>%
  filter(!cover == -99) %>%
  filter(invasive == 0) %>%
  group_by(trap, treeID) %>%
  summarise(cover = sum(cover)) %>%
  ungroup() %>%
  pivot_wider(id_cols = trap, 
              values_from = cover, 
              values_fill = 0,
              names_from = treeID) %>%
  column_to_rownames("trap") %>%
  as.matrix()

cmat.exo <- cover %>%
  filter(!cover == -99) %>%
  filter(invasive == 1) %>%
  group_by(trap, treeID) %>%
  summarise(cover = sum(cover)) %>%
  ungroup() %>%
  pivot_wider(id_cols = trap, 
              values_from = cover, 
              values_fill = 0,
              names_from = treeID) %>%
  column_to_rownames("trap") %>%
  as.matrix()

div.full <- data.frame(div.full = vegan::diversity(cmat.full, index = "shannon")) %>%
  rownames_to_column("trap")
div.nat <- data.frame(div.nat = vegan::diversity(cmat.nat, index = "shannon")) %>%
  rownames_to_column("trap")
div.exo <- data.frame(div.exo = vegan::diversity(cmat.exo, index = "shannon")) %>%
  rownames_to_column("trap")

div.some <- merge(div.full, div.nat, by = "trap", all = T)
div.all <- merge(div.some, div.exo, by = "trap", all = T)
div.all[,2:4] <- exp(div.all[,2:4])

vol.exo <- data.frame(vol.exo = rowSums(cmat.exo)) %>%
  rownames_to_column("trap")
vol.nat <- data.frame(vol.nat = rowSums(cmat.nat)) %>%
  rownames_to_column("trap")

rich.exo <- data.frame(rich.exo = rowSums(cmat.exo > 0)) %>%
  rownames_to_column("trap")

rich.nat <- data.frame(rich.nat = rowSums(cmat.nat > 0)) %>%
  rownames_to_column("trap")

div.all <- merge(div.all, vol.exo, by = "trap", all = T)
div.all <- merge(div.all, vol.nat, by = "trap", all = T)
div.all <- merge(div.all, rich.exo, by = "trap", all = T)
div.all <- merge(div.all, rich.nat, by = "trap", all = T)


div.all <- div.all %>%
  mutate(across(div.full:rich.nat, \(x) ifelse(is.na(x), 0, x)))
div.all$rich <- div.all$rich.exo + div.all$rich.nat

# now do info

# fix canopy:
miss <- 8 - str_length(as.character(info$canopy))
front <- sapply(miss, function(x) paste0(rep(0, x), collapse = ""))
info$canopy <- paste0(front, info$canopy)

trapDat <- info %>%
  mutate(daysOut = downDate - upDate,
         lat = lat/10000000 + 41,
         lon = -72 - lon/10000000,
         cano1 = as.integer(str_extract(canopy, "..$")),
         cano2 = as.integer(str_extract(canopy, "..(?=.{2}$)")),
         cano3 = as.integer(str_extract(canopy, "..(?=.{4}$)")),
         cano4 = as.integer(str_extract(canopy, "..(?=.{6}$)"))) %>%
  mutate(daysOut = ifelse(daysOut > 50, daysOut - 70, daysOut)) %>%
  mutate(upDate = ymd(paste0("20220", upDate)),
         canoCover = ((cano1 + cano2 + cano3 + cano4)/4) * 1.04) %>%
  dplyr::select(-canopy, -cano1, -cano2, -cano3, -cano4, -downDate) %>%
  full_join(div.all, by = "trap") %>%
  pivot_longer(cols = starts_with("order"), 
               names_to = "position",
               values_to = "color")


# BRING IN THE CONTENTS

contents <- read.csv("data/dirty/traps/trapContents.csv", fileEncoding="UTF-8-BOM")

# okay let's do what I can with this...
contents$ImageName <- gsub("O", 0, contents$ImageName)
contents$ImageName[contents$ImageName %in% "SM01C_W"] <- "SV01C_W"
contents$ImageName[contents$ImageName %in% "PL10B"] <- "PL10_B"
contents$ImageName[contents$ImageName %in% "TQ02B_01"] <- "TQ02B_B_01"
contents$ImageName[contents$ImageName %in% "TQ02B_02"] <- "TQ02B_B_02"
trapNames <- str_extract(contents$ImageName, "^[[:alpha:]]{2}[[:digit:]]{2}(B|C|_)")
trapNames <- gsub("_", "", trapNames)

otherStuff <- str_extract(contents$ImageName, "(?<=_).*")
otherStuff <- gsub("\\d*", "", otherStuff)
otherStuff <- gsub("_", "", otherStuff)
otherStuff <- gsub("\\.", "", otherStuff)

conto <- contents %>%
  dplyr::select(-ImageName, -Notes, -Others, -Non.Tachinid.flies) %>%
  mutate(trap = trapNames, 
         color = otherStuff) %>%
  relocate(trap, color) %>%
  mutate(wasps = Ichneumoninae + Chalcidoidea + Proctophoridae) %>%
  mutate(total = wasps + Tachinidae) %>%
  group_by(trap, color) %>%
  summarise(across(Ichneumoninae:total, sum)) %>%
  ungroup()

conto$toidDiv <- vegan::diversity(conto[,3:6])

td <- merge(trapDat, conto, by = c("trap", "color"), all = T) %>%
  mutate(trap_col = paste(trap, color, sep = "_"))

# now get out trap failures

ok <- c("B75", "CHECKSHEET", "COMPLETE", "S", "UNSURE", "WL", "Y75", "Y85")

potFail <- td[!td$fail %in% ok,]

potFail$fail.B <- str_extract(potFail$fail, "B.{2}")
potFail$fail.Y <- str_extract(potFail$fail, "Y.{2}")
potFail$fail.W <- str_extract(potFail$fail, "W.{2}")

emptyFail <- potFail %>%
  filter((color == "B" & !is.na(fail.B)) |
           (color == "W" & !is.na(fail.W)) |
           (color == "Y" & !is.na(fail.Y))) %>%
  dplyr::select(trap, color, trap_col, starts_with("fail")) %>%
  dplyr::select(-fail) %>%
  pivot_longer(starts_with("fail"), 
               names_to = "fail_col",
               values_to = "fail_amount") %>%
  mutate(fail_col = gsub("^fail.","",fail_col),
         fail_amount = gsub("^.", "", fail_amount)) %>%
  filter(color == fail_col) %>%
  filter(!fail_amount %in% "WL") %>%
  mutate(fail_amount = as.numeric(fail_amount))

completeFailure <- emptyFail$trap_col[emptyFail$fail_amount < 10]
partialFailure <- emptyFail[emptyFail$fail_amount >= 10, ] %>%
  dplyr::select(trap_col, fail_amount)

td.solid <- td %>%
  filter(!trap_col %in% completeFailure) %>%
  left_join(partialFailure, by = "trap_col") %>%
  mutate(fail_amount = ifelse(is.na(fail_amount), 1, fail_amount/100)) %>%
  mutate(fail = ifelse(trap_col %in% emptyFail$trap_col, "partial", "no_fail")) %>%
  filter(!is.na(trap)) %>%
  mutate(toidDiv = exp(toidDiv)) %>%
  mutate(across(Ichneumoninae:toidDiv, \(x) ifelse(is.na(x), 0, x)),
         upDate = scale(yday(upDate))) %>%
  filter(!trap %in% "BM02B") %>%
  mutate(block = str_extract(trap, "^..")) %>%
  mutate(grouping = str_extract(trap, "[[:alpha:]]$")) %>%
  mutate(grouping = ifelse(is.na(grouping), str_extract(trap, "..$"), grouping)) %>%
  mutate(grouping = ifelse(grouping %in% c("01", "02", "03"), "A", grouping)) %>%
  mutate(grouping = ifelse(grouping %in% c("04", "05", "06","07", "08", "09"), "D", grouping)) %>%
  mutate(grouping = ifelse(grouping %in% c("10", "11"), "E", grouping))


td.solid$ratio <- (td.solid$vol.exo + 1)/
(td.solid$vol.nat + 1)


td.solid$ratio <- log1p(td.solid$vol.nat) - log1p(td.solid$vol.exo)

td.mush <- td.solid %>%
  group_by(trap) %>%
  summarise(across(upDate:rich, first),
            across(Ichneumoninae:total, sum),
            across(block:grouping, first),
            fail_amount = sum(fail_amount)) %>%
  dplyr::select(-fail)

td.mush$div.mush <- vegan::diversity(td.mush[,c("Ichneumoninae", 
                                       "Chalcidoidea", "Proctophoridae", "Tachinidae")])

td.mush <- td.mush %>%
  mutate(div.mush = ifelse(total == 0, 0, exp(div.mush)))

# clays 

info2 <- info %>%
  mutate(daysOut = downDate - upDate,
         lat = lat/10000000 + 41,
         lon = -72 - lon/10000000,
         cano1 = as.integer(str_extract(canopy, "..$")),
         cano2 = as.integer(str_extract(canopy, "..(?=.{2}$)")),
         cano3 = as.integer(str_extract(canopy, "..(?=.{4}$)")),
         cano4 = as.integer(str_extract(canopy, "..(?=.{6}$)"))) %>%
  mutate(daysOut = ifelse(daysOut > 50, daysOut - 70, daysOut)) %>%
  mutate(upDate = ymd(paste0("20220", upDate)),
         canoCover = ((cano1 + cano2 + cano3 + cano4)/4) * 1.04) %>%
  dplyr::select(-canopy, -cano1, -cano2, -cano3, -cano4, -downDate) %>%
  full_join(div.all, by = "trap")

do.clay <- td.solid %>%
  group_by(block, trap) %>%
  summarise(daysOut = mean(daysOut)) %>%
  group_by(block) %>%
  summarise(do.clay = mean(daysOut) * 3)

clays <- read.csv("data/dirty/traps/trapClays.csv", fileEncoding="UTF-8-BOM") %>%
  rename(clay = color) %>%
  mutate(strikes = str_length(fateDate)/3) %>%
  mutate(strikes = ifelse(is.na(strikes), 0, strikes)) %>%
  mutate(hostFamily = "Rosaceae",
         hostNative = "native") %>%
  dplyr::select(-fateDate) %>%
  filter(!trap %in% c("HH01", "HH02", "HH03")) %>%
  left_join(info2) %>%
  mutate(block = str_extract(trap, "^.."),
         trials = 3) %>%
  left_join(do.clay)

# clays$strikes <- clays$strikes - str_count(clays$fate, pattern = "m")
# clays$strikes <- ifelse(clays$strikes > 0, 1, 0)

# rename a few...
clays$treeID[clays$treeID %in% "CRAPH"] <- "CRAXX"
clays$treeID[clays$treeID %in% "LONTA"] <- "LONMO"
clays$treeID[clays$treeID %in% c("MALFL", "MALPR", 
                                       "MALPU", "MALSI")] <- "MALXX"
clays$treeID[clays$treeID %in% "PRUVI"] <- "PRUSE"
clays$treeID[clays$treeID %in% "RUBAL"] <- "RUBXX"

invasives <- c("BERTH", 'EUOAL', 'LIGOB', 'LONMO', 'CELOR',
               'MALXX', 'ROSMU','VIBDI','VIBPL',
               'VIBSI', 'RHOSC', "RUBPH")
clays$hostNative[clays$treeID %in% invasives] <- "exotic"
clays$hostFamily[clays$treeID %in% c("BERTH", 'EUOAL', "CELOR")] <- 'InvasiveOutgroups'
clays$hostFamily[clays$treeID %in% c("VIBLE","LONMO","VIBDE","VIBDI","VIBAC", "VIBPL","VIBSI","SAMCA")] <- 'Caprifoliaceae'
clays$hostFamily[clays$treeID %in% c("FRAAM", "LIGOB")] <- 'Oleaceae'

clays$upDate <- scale(yday(clays$upDate))

rm(rich.nat, trapDat, treeCodes, vol.exo, vol.nat, 
   completeFailure, front, inv, invasives, miss, ok,
   otherStuff, trapNames, trees, cmat.exo, cmat.full, cmat.nat,
   contents, cover, div.all, div.exo, div.full, div.nat, div.some, emptyFail,
   info, info2, partialFailure, potFail, rich.exo, conto, td)


