# This script, the first in the series, should combine the 
# mostly cleaned 2021 and 2022 data 
# then prep them for analysis

# considerable changes are made to the dataset throughout
# to reflect the goals of analysis vs. those goal when the data were
# first collected

# Initialize ####

rm(list = ls())

library(tidyverse)

# load 2021 data
load("data/clean/catsWithBranch.Rdata")

# load 2022 data
cats <- read.csv("data/dirty/cat/cat22.csv")

# Light data modifcation to standardize both datasets ####

# remove data regarding caterpillars observed but not collected in 2022
cats <- cats %>%
  filter(!collected. %in% c("N","")) %>% 
  rename(date = DATE) %>%
  dplyr::select(-collected., -X)

# fix small spelling error in loaded 2021 data
cwbb$hostFamily[cwbb$hostFamily %in% "Roseaceae"] <- "Rosaceae"

# do considerable reshaping of 2021 data
c21 <- cwbb %>%
  # select useful columns
  dplyr::select(catNum, date = date.x, catSpecies, fate, fateDate,
         dateInitialWeight, initialWeight, finalWeight, frassWeight,
         weirdFinal, transect, treeSpecies, hostFamily, hostNative) %>%
  # select only rows that contain caterpillars
  filter(!is.na(catNum)) %>%
  # add columns that identify year and set-up host-switching experiment
  mutate(year = 2021, newHost = treeSpecies,
         newHostFamily = hostFamily, 
         newHostNative = hostNative,
         captureWeight = NA)

# reshape 2022 data to look like 2021 data
c22 <- cats %>%
  dplyr::select(catNum, date, catSpecies, fate, fateDate, 
         initialWeight = geInitialWeight,
         finalWeight = geFinalWeight,
         frassWeight = geFrassWeight, 
         transect, treeSpecies, newHost,
         captureWeight) %>%
  mutate(year = 2022, weirdFinal = "",
         hostNative = "native",
         hostFamily = "Rosaceae",
         newHostNative = "native",
         newHostFamily = "Rosaceae",
         dateInitialWeight = date + 1)

rm(cats, cwbb)

# fix obvious issues in c22
# renames dates that don't exist into their correct dates
c22$dateInitialWeight[c22$dateInitialWeight %in% 532] <- 601
c22$dateInitialWeight[c22$dateInitialWeight %in% 631] <- 701

# rename tree species based on knowledge not known in the field
# Crataegus were non-identifiable
c22$treeSpecies[c22$treeSpecies %in% "CRAPH"] <- "CRAXX"
# All Lonicera were Lonicera morowii or Bell's, but the distinction was not made
c22$treeSpecies[c22$treeSpecies %in% "LONTA"] <- "LONMO"
# All apples were Malus sieboldii
c22$treeSpecies[c22$treeSpecies %in% "MALXX"] <- "MALSI"
# All cherries were prunus serotina
c22$treeSpecies[c22$treeSpecies %in% "PRUVI"] <- "PRUSE"
# All Rubus were unidentifiable
c22$treeSpecies[c22$treeSpecies %in% "RUBAL"] <- "RUBXX"

# caterpillars switched to an invasive viburnum were moved to V. dilitatum
c22$newHost[c22$newHost %in% "INV"] <- "VIBDI"
# change the "newHost" column to reflect changes made above
c22$newHost[c22$newHost %in% "LONTA"] <- "LONMO"
c22$newHost[c22$newHost %in% "MALXX"] <- "MALSI"
c22$newHost[c22$newHost %in% "PRUVI"] <- "PRUSE"
# This caterpillar was not host swapped
c22$newHost[c22$newHost %in% "PROTO"] <- ""
# make the "new host" the old host for unswapped caterpillars
c22$newHost[c22$newHost %in% "" |
              is.na(c22$newHost)] <- c22$treeSpecies[c22$newHost %in% "" |
                                                       is.na(c22$newHost)]

# label all invasives as such, including those ultimately dropped
invasives <- c("BERTH", 'EUOAL', 'LIGOB', 'LONMO', 'CELOR',
               'MALXX',"MALFL", "MALPR", "MALPU", "MALSI", 
               'ROSMU','VIBDI','VIBPL',
               'VIBSI', 'RHOSC', "RUBPH")
# label which caterpillars were found on invasives
c22$hostNative[c22$treeSpecies %in% invasives] <- "exotic"

# properly label each family
c22$hostFamily[c22$treeSpecies %in% c("BERTH", 'EUOAL', "CELOR")] <- 'InvasiveOutgroups'
c22$hostFamily[c22$treeSpecies %in% c("VIBLE","LONMO","VIBDE","VIBDI","VIBAC", "VIBPL","VIBSI","SAMCA")] <- 'Caprifoliaceae'
c22$hostFamily[c22$treeSpecies %in% c("FRAAM", "LIGOB")] <- 'Oleaceae'

# label host-swaps
c22$newHostNative[c22$newHost %in% invasives] <- "exotic"
c22$newHostFamily[c22$newHost %in% c("BERTH", 'EUOAL', "CELOR")] <- 'InvasiveOutgroups'
c22$newHostFamily[c22$newHost %in% c("VIBLE","LONMO","VIBDE","VIBDI","VIBAC", "VIBPL","VIBSI","SAMCA")] <- 'Caprifoliaceae'
c22$newHostFamily[c22$newHost %in% c("FRAAM", "LIGOB")] <- 'Oleaceae'

cc <- rbind(c21, c22)
rm(c21,c22,invasives)

# Intensive data cleaning to reflect analysis goals ####

# all Acronicta found ultimately were A. superans, due to significant sampling of Roseaceae
cc$catSpecies[cc$catSpecies %in% "ACROXX"] <- "ACROSU"

# all species here were exotic, unidentifiable, or otherwise useless
cc <- cc[!cc$catSpecies %in% c("BROWNX", "LEPXX", "LYMADI", "ROUGEX",
                               "ROUGEXX", "PUPAXX", "SHELLX", "SPIKEX",
                               "TOIDXX", "UNIDXX", "PALEXX"), ]

# The species called BUTTXX was identified later as C. cerintha
cc$catSpecies[cc$catSpecies %in% c("BUTTXX", "BUTTTX")] <- "CERMCE"

# Eupithecia, a hyper generalist taxon with poorly-resolved systematics,
# was treated as a single species E. swettii, as prior barcoding has shown
# the overwhelming majority of those found in this system are this species
cc$catSpecies[cc$catSpecies %in% c("EUISW", "EUPIXX")] <- "EUPISW"

# With exceptions - E. miserulata is distinct
cc$catSpecies[cc$catSpecies %in% c("EUPIPMI")] <- "EUPIMI"

# spelling correction, though GEOMXX will later be eliminated
cc$catSpecies[cc$catSpecies %in% "GOMXX"] <- "GEOMXX"

# Spelling
cc$catSpecies[cc$catSpecies %in% "HYAUN"] <- "HYPAUN"

# All Lithophane were treated as a single species. Most were likely L. antenatta
cc$catSpecies[cc$catSpecies %in% "LITHAN"] <- "LITHXX"
# Spelling
cc$catSpecies[cc$catSpecies %in% "MEALCA"] <- "MELACA"
cc$catSpecies[cc$catSpecies %in% "MHIMEFI"] <- "HIMEFI"

# All Orthosia were treated as the same species, as the two generalists 
# were difficult to distinguish
cc$catSpecies[cc$catSpecies %in% c("ORTHXX", "ORTHHI")] <- "ORTHRU"

# All papilio were P. glaucus
cc$catSpecies[cc$catSpecies %in% "PAPIXX"] <- "PAPIGL"

# All Pero were P. ancetaria
cc$catSpecies[cc$catSpecies %in% "PEROXX"] <- "PEROAN"

# All Pterophoridae were G. bucksii
cc$catSpecies[cc$catSpecies %in% "PTEROXX"] <- "GEINBU"

# This caterpillar was later identified as Calledapteryx dryopterata
cc$catSpecies[cc$catSpecies %in% "SPECLX"] <- "CALLDR"

# This caterpillar was later identified as Hemaris thysbe
cc$catSpecies[cc$catSpecies %in% "SPHIXX"] <- "HEMATH"

# All Schizura captured were S. unicornis
cc$catSpecies[cc$catSpecies %in% "SCHIXX"] <- "SCHIUN"

# This species was later identified as Hypena scabra
cc$catSpecies[cc$catSpecies %in% c("WIGGLX", "EREBXX")] <- "HYPESC"

# This species was later identified as Z. horribilis
cc$catSpecies[cc$catSpecies %in% "ZALEPG"] <- "ZALEHO"

# fix data entry errors

# two issues with dates and location mixed up during initial recording
cc$date[cc$catNum %in% c(1409:1464)] <- 614
cc$dateInitialWeight[cc$catNum %in% c(1409:1464)] <- 615
cc$transect[cc$catNum %in% c(1409:1464)] <- 321

cc$date[cc$catNum %in% c(2224, 2225, 2228)] <- 715
cc$dateInitialWeight[cc$catNum %in% c(2224, 2225, 2228)] <- 716
cc$transect[cc$catNum %in% c(2224, 2225, 2228)] <- 359

# Many Zale were rechecked and IDed as Z. horribilis

cc$catSpecies[cc$catSpecies %in% "ZALEXX" &
                cc$date < 616 &
                cc$hostFamily %in% "Rosaceae" &
                !is.na(cc$catSpecies)] <- "ZALEHO"

# Fix a known error where multiple issues combined
# (I believe this is actually an R. meadii found on B. thunbergii)
cc$catSpecies[cc$catNum %in% "K4013"] <- "GEOMXX"

# make date julian
cc$jDate <- paste0(cc$year,"0", cc$date) %>%
  ymd() %>%
  yday()

# relevel factors
cc$hostFamily <- factor(cc$hostFamily, levels = c('InvasiveOutgroups',
                                                  'Caprifoliaceae',
                                                  'Oleaceae',
                                                  'Rosaceae'))

cc$year <- factor(cc$year)

# and then just randomly make my violin function here for some reason
# (highly likely to move)

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}



