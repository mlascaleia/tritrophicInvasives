### make a dataframe of intro time for each plant

rm(list = ls())

load("data/clean/cleanBranch.rdata")
load("data/clean/catWeights.rdata")

exo <- unique(sameB$treeSpecies[sameB$hostNative %in% "exotic"])
intro <- data.frame(treeSpecies = exo, 
                    introYear = c(1623,  # apple, basic search
                                  1875, # barberry, basic search
                                  1845, # VIBDI, https://monativeplants.org/wp-content/uploads/missouriensis/missouriensis-38/MONPS_38_1-3.pdf
                                  1860, # EUOAL
                                  1866, # ROSMU
                                  1825, # LIGOB
                                  1860, # LONMO
                                  1860,
                                  1860,
                                  1890,
                                  1866))

