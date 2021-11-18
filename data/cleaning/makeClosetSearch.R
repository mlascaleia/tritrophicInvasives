i <- 1
theseProbs <- data.frame(catNum = character(0),
                         catSpecies = character(0),
                         treeSpecies = character(0),
                         fate = character(0),
                         photograph = numeric(0),
                         issue = character(0))

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2873') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'ReID was recorded as Y; what does it actually say?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4532') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'ReID was recorded as Y; what does it actually say?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2099') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2327') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2609') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2616') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2617') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2754') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2843') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J3450') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J3451') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4322') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4624') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4627') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4726') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'search SD card for photo or see if cup actually says photo')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2096') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'what is the host species?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4069') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'what is the host species?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2017') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'what is the host species?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2265') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2276') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2439') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2612') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J2821') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J3130') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'J3554') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4027') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4233') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4234') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4235') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4346') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4571') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4631') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K4632') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K5007') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'What is the fate?')
      i <- i + 1

theseProbs[i,] <- cats %>%
      filter(catNum %in% 'K5006') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = 'Any chance we can figure out what species this was?')
      i <- i + 1
