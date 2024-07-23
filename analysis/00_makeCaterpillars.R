# this script should make the 2022 cat data look
# at least a bit like the 2021 cat data

# then combine them all for analysis

rm(list = ls())

library(tidyverse)

cats <- read.csv("data/dirty/cat/cat22.csv")

cats <- cats %>%
  filter(!collected. %in% c("N","")) %>% 
  rename(date = DATE) %>%
  dplyr::select(-collected., -X)

# load("data/clean/catWeights21.rdata")
# load("data/clean/cleanBranch21.rdata")
load("data/clean/catsWithBranch.Rdata")

# okay so this is going to get confusing 
# but I think I'm going to bring everything in...
# then change the 2021 data to get rid of a bunch of junk
# and add some of that junk to 2022

# get rid of 2021 junk

c21 <- cwbb %>%
  dplyr::select(catNum, date = date.x, catSpecies, fate, fateDate,
         dateInitialWeight, initialWeight, finalWeight, frassWeight,
         weirdFinal, transect, treeSpecies, hostFamily, hostNative) %>%
  filter(!is.na(catNum)) %>%
  mutate(year = 2021, newHost = treeSpecies,
         newHostFamily = hostFamily, 
         newHostNative = hostNative,
         captureWeight = NA)



# make 2022 look like 2021

c22 <- cats %>%
  dplyr::select(catNum, date, catSpecies, fate, fateDate, 
         initialWeight = geInitialWeight,
         finalWeight = geFinalWeight,
         frassWeight = geFrassWeight, 
         transect, treeSpecies, newHost,
         captureWeight) %>%
  mutate(year = 2022, weirdFinal = "",
         hostNative = "native",
         hostFamily = "Roseaceae",
         newHostNative = "native",
         newHostFamily = "Roseaceae",
         dateInitialWeight = date + 1)

c22$dateInitialWeight[c22$dateInitialWeight %in% 532] <- 601
c22$dateInitialWeight[c22$dateInitialWeight %in% 631] <- 701

# rename a few...
c22$treeSpecies[c22$treeSpecies %in% "CRAPH"] <- "CRAXX"
c22$treeSpecies[c22$treeSpecies %in% "LONTA"] <- "LONMO"
# c22$treeSpecies[c22$treeSpecies %in% c("MALFL", "MALPR", 
#                                        "MALPU", "MALSI")] <- "MALXX"
c22$treeSpecies[c22$treeSpecies %in% "MALXX"] <- "MALSI"
c22$treeSpecies[c22$treeSpecies %in% "PRUVI"] <- "PRUSE"
c22$treeSpecies[c22$treeSpecies %in% "RUBAL"] <- "RUBXX"

# table(c22$newHost)
# c22[c22$newHost %in% "PROTO", ]

c22$newHost[c22$newHost %in% "INV"] <- "VIBDI"
c22$newHost[c22$newHost %in% "LONTA"] <- "LONMO"
# c22$newHost[c22$newHost %in% c("MALFL", "MALPR", 
#                                "MALPU", "MALSI")] <- "MALXX"
c22$newHost[c22$newHost %in% "MALXX"] <- "MALSI"
c22$newHost[c22$newHost %in% "PRUVI"] <- "PRUSE"
c22$newHost[c22$newHost %in% "PROTO"] <- ""
c22$newHost[c22$newHost %in% "" |
              is.na(c22$newHost)] <- c22$treeSpecies[c22$newHost %in% "" |
                                                       is.na(c22$newHost)]

invasives <- c("BERTH", 'EUOAL', 'LIGOB', 'LONMO', 'CELOR',
               'MALXX',"MALFL", "MALPR", "MALPU", "MALSI", 
               'ROSMU','VIBDI','VIBPL',
               'VIBSI', 'RHOSC', "RUBPH")
c22$hostNative[c22$treeSpecies %in% invasives] <- "exotic"
c22$hostFamily[c22$treeSpecies %in% c("BERTH", 'EUOAL', "CELOR")] <- 'InvasiveOutgroups'
c22$hostFamily[c22$treeSpecies %in% c("VIBLE","LONMO","VIBDE","VIBDI","VIBAC", "VIBPL","VIBSI","SAMCA")] <- 'Caprifoliaceae'
c22$hostFamily[c22$treeSpecies %in% c("FRAAM", "LIGOB")] <- 'Oleaceae'

c22$newHostNative[c22$newHost %in% invasives] <- "exotic"
c22$newHostFamily[c22$newHost %in% c("BERTH", 'EUOAL', "CELOR")] <- 'InvasiveOutgroups'
c22$newHostFamily[c22$newHost %in% c("VIBLE","LONMO","VIBDE","VIBDI","VIBAC", "VIBPL","VIBSI","SAMCA")] <- 'Caprifoliaceae'
c22$newHostFamily[c22$newHost %in% c("FRAAM", "LIGOB")] <- 'Oleaceae'


# table(c22$hostFamily, c22$treeSpecies)
# table(c22$hostNative, c22$treeSpecies)

cc <- rbind(c21, c22)
rm(c21,c22, cats, cwbb, invasives)

# now go through the columns and fix weird things...

table(cc$catSpecies)
# ugh a lot to fix here

cc[cc$catSpecies %in% "PEROXX", ]

cc$catSpecies[cc$catSpecies %in% "ACROXX"] <- "ACROSU"
cc <- cc[!cc$catSpecies %in% c("BROWNX", "LEPXX", "LYMADI", "ROUGEX",
                               "ROUGEXX", "PUPAXX", "SHELLX", "SPIKEX",
                               "TOIDXX", "UNIDXX", "PALEXX"), ]
cc$catSpecies[cc$catSpecies %in% c("BUTTXX", "BUTTTX")] <- "CERMCE"
cc$catSpecies[cc$catSpecies %in% c("EUISW", "EUPIXX")] <- "EUPISW"
cc$catSpecies[cc$catSpecies %in% c("EUPIPMI")] <- "EUPISW"
cc$catSpecies[cc$catSpecies %in% "GOMXX"] <- "GEOMXX"
cc$catSpecies[cc$catSpecies %in% "HYAUN"] <- "HYPAUN"
cc$catSpecies[cc$catSpecies %in% "LITHAN"] <- "LITHXX"
cc$catSpecies[cc$catSpecies %in% "MEALCA"] <- "MELACA"
cc$catSpecies[cc$catSpecies %in% "MHIMEFI"] <- "HIMEFI"
cc$catSpecies[cc$catSpecies %in% c("ORTHXX", "ORTHHI")] <- "ORTHRU"
cc$catSpecies[cc$catSpecies %in% "PAPIXX"] <- "PAPIGL"
cc$catSpecies[cc$catSpecies %in% "PEROXX"] <- "PEROAN"
cc$catSpecies[cc$catSpecies %in% "PTEROXX"] <- "GEINBU"
cc$catSpecies[cc$catSpecies %in% "SPECLX"] <- "CALLDR"
cc$catSpecies[cc$catSpecies %in% "SPHIXX"] <- "HEMATH"
cc$catSpecies[cc$catSpecies %in% "SCHIXX"] <- "SCHIUN"
cc$catSpecies[cc$catSpecies %in% c("WIGGLX", "EREBXX")] <- "HYPESC"
cc$catSpecies[cc$catSpecies %in% "ZALEPG"] <- "ZALEHO"

# fix data entry error(s)
cc$date[cc$catNum %in% c(1409:1464)] <- 614
cc$dateInitialWeight[cc$catNum %in% c(1409:1464)] <- 615
cc$transect[cc$catNum %in% c(1409:1464)] <- 321

cc$date[cc$catNum %in% c(2224, 2225, 2228)] <- 715
cc$dateInitialWeight[cc$catNum %in% c(2224, 2225, 2228)] <- 716
cc$transect[cc$catNum %in% c(2224, 2225, 2228)] <- 359

# look at zome zale that I'm sussy of

# I think any Zale found on roseaceae prior to 615 can reliably be
# called a zaleho

# mainly interested to see whether that affects the change experiment
# (it probably doesn't)

cc$catSpecies[cc$catSpecies %in% "ZALEXX" &
                cc$date < 616 &
                cc$hostFamily %in% "Roseaceae"] <- "ZALEHO"

# make date julian
cc$jDate <- paste0(cc$year,"0", cc$date) %>%
  ymd() %>%
  yday()

# relevel factors
cc$hostFamily <- factor(cc$hostFamily, levels = c('InvasiveOutgroups',
                                                  'Caprifoliaceae',
                                                  'Oleaceae',
                                                  'Roseaceae'))

cc$year <- factor(cc$year)

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



