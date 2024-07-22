source("data/cleaning/dfCompareBranches.R")
source("data/cleaning/combineDFs.R")
load("data/clean/catWeights21.rdata")
library(ggthemes)
library(patchwork)
library(bipartite)
library(igraph)

# dontInclude <- c("LYMADI", "MICROX", "NOT ORTHRU", "GEOMXX", "NOT ZALE", "NOCTXX",
#                  "UNKNXX", "UNIDXX", "FOURLX", "CERAUN", "HYALCE", "EREBXX", "RHEUPR")
# 
# cw <- cw[!cw$catSpecies %in% dontInclude, ]

nrow(sameB) # number of branches sampled
nrow(sameB[sameB$branchNum == 1,])


sameB$treeSpecies[sameB$treeSpecies %in% "AMECA"] <- "AMEXX"
sameB$treeSpecies[sameB$treeSpecies %in% "AMELA"] <- "AMEXX"
sameB$treeSpecies[sameB$treeSpecies %in% "AMEAR"] <- "AMEXX"
sameB$treeSpecies[sameB$treeSpecies %in% "FRAMM"] <- "FRAAM"
sameB$treeSpecies[sameB$treeSpecies %in% "FRAXX"] <- "FRAAM"
sameB$treeSpecies[sameB$treeSpecies %in% "LIGVU"] <- "LIGOB"
sameB$treeSpecies[sameB$treeSpecies %in% "LONTA"] <- "LONMO"
sameB$treeSpecies[sameB$treeSpecies %in% "LONXX"] <- "LONMO"
sameB$treeSpecies[sameB$treeSpecies %in% "ROSNU"] <- "ROSMU"
sameB$treeSpecies[sameB$treeSpecies %in% "RUBAL"] <- "RUBXX"
sameB$treeSpecies[sameB$treeSpecies %in% "VIBPL" & sameB$date < 621] <- "VIBSI"
sameB$treeSpecies[sameB$treeSpecies %in% "VIBLA"] <- "VIBPL"
sameB$treeSpecies[sameB$treeSpecies %in% "VIBPI"] <- "VIBPL"


sameB <- sameB %>%
  filter(!treeSpecies %in% c("ROSXX", "ULMRU"))

sameB$treeSpecies[sameB$treeSpecies %in% "CELOR"] <- "VIBSI"

sameB$treeSpecies[sameB$treeSpecies %in% ""] <- sameB$treeSpecies[which(sameB$treeSpecies %in% "") - 1]

table(sameB$treeSpecies)
length(table(sameB$treeSpecies))


invasives <- c("BERTH", 'EUOAL', 'LIGOB', 'LONMO', 'MALXX', 'ROSMU','VIBDI','VIBPL','VIBSI', 'RHOSC', "RUBPH")
sameB$hostNative <- "native"
sameB$hostNative[sameB$treeSpecies %in% invasives] <- "exotic"

sameB$hostFamily <- "Roseaceae"
sameB$hostFamily[sameB$treeSpecies %in% c("BERTH", 'EUOAL')] <- 'InvasiveOutgroups'
sameB$hostFamily[sameB$treeSpecies %in% c("VIBLE","LONMO","VIBDE","VIBDI","VIBAC", "VIBPL","VIBSI","SAMCA")] <- 'Caprifoliaceae'
sameB$hostFamily[sameB$treeSpecies %in% c("FRAAM", "LIGOB")] <- 'Oleaceae'

sameB$bid <- 800001:(800000 + nrow(sameB))

updateBranch <- function(){
  sameB$catNumMin <- gsub("-","",sameB$catNumMin)
  sameB$catNumMax <- gsub("-","",sameB$catNumMax)
  
  sameB$minn <- gsub("(J|K)","", sameB$catNumMin ,ignore.case = T)
  sameB$maxn <- gsub("(J|K)","", sameB$catNumMax ,ignore.case = T)
  
  bb <- data.frame(catNum = character(0), bid = integer(0))
  for(i in 1:nrow(sameB)){
    if(sameB$catNumMin[i] != ""){
      catsIncl <- sameB$minn[i]:sameB$maxn[i]
      catsIncl <- gsub("^2", "J2", catsIncl)
      catsIncl <- gsub("^3", "J3", catsIncl)
      catsIncl <- gsub("^4", "K4", catsIncl)
      catsIncl <- gsub("^5", "K5", catsIncl)
      bforc <- cbind.data.frame(catNum = catsIncl, bid = sameB$bid[i])
      bb <- rbind(bb, bforc)
    }
  }
  cwb <- merge(cw, bb, by = "catNum", all.x = T)
  return(cwb)
}

# start here

# dupsc <- bb$catNum[duplicated(bb$catNum)]
# dupsb <- bb$bid[bb$catNum %in% dupsc]
# 
# sameB[sameB$bid %in% dupsb, ] # sheets 4-6; 58; 60-61
# 
# sameB[sameB$sheet %in% c(4,5,6), ] # issue is 2103
# cw[cw$treeSpecies %in% "ROSMU" & cw$date %in% 602, ]
# 
# 
# 
# bb[bb$catNum %in% dupsc, ]
# 
# 
# 
# cw[cw$catNum %in% dupsc, ]
# 
# 
# cwb <- merge(cw, bb, by = "catNum", all.x = T)

# alright so there's actually a lot of errors here, 
# going to need to go through a bit more systematically
cwb <- updateBranch()

cwb[is.na(cwb$bid), ]

# going with the number on the cup as sacred


sameB[sameB$catNumMin %in% "J2103", ]
sameB$catNumMin[sameB$line %in% 126] <- "J2104"
sameB$catNumMax[sameB$line %in% 126] <- "J2104"

# cw[cw$catNum %in% c("J2220", "J2221"), ]
# sameB[sameB$catNumMin %in% "J2220", ]
# sameB$catNumMin[sameB$line %in% 172] <- "J2221"
sameB$catNumMax[sameB$line %in% 172] <- "J2221"


# sameB[sameB$catNumMin %in% "J2353", ]

# sameB[sameB$catNumMin %in% "J2993", ]
sameB[sameB$sheet %in% 40 & sameB$treeSpecies %in% "ROSMU" & sameB$lfCount %in% 200, ] 
# this gives line 1193
sameB$catNumMin[sameB$line %in% 1193] <- "J2993" # I suspect this maybe was supposed to be J2943?
sameB$catNumMax[sameB$line %in% 1193] <- "J2993" # I suspect this maybe was supposed to be J2943?

# sameB[sameB$sheet %in% 54, ]
sameB$catNumMin[sameB$line %in% 1617] <- "J3059"
sameB$catNumMax[sameB$line %in% 1617] <- "J3060"

# sameB[sameB$sheet %in% 44, ]
sameB$catNumMin[sameB$line %in% 1313] <- "J3248"
sameB$catNumMax[sameB$line %in% 1313] <- "J3250"

# sameB[sameB$sheet %in% 53, ] # the madlads actually did it
sameB$catNumMin[sameB$line %in% 1610] <- "J3603"


# sameB[sameB$sheet %in% 58, ]
sameB$catNumMin[sameB$line %in% 1745] <- "K4086"
sameB$catNumMax[sameB$line %in% 1745] <- "K4086"

# next one seems tricky...
cw[cw$catNum %in% c("K4104", "K4105", "K4106", "K4107", "K4108"), ]
sameB[sameB$date == 702 & sameB$recorder == "DR", ]
# absolutely no clue where this one came from
# well not no clue, there's only 3-4 possible branches really

sameB[grepl("K4107", sameB$notes), ] # nothing

# okay I think I found it there's one point where it changes to my handwriting
# my b RIP
# I think this is moments after Keisha got stung by a bee
sameB[sameB$date == 702 & sameB$recorder == "DR", ]
sameB$catCount[sameB$line %in% 1752] <- 1
sameB$catNumMin[sameB$line %in% 1752] <- "K4107"
sameB$catNumMax[sameB$line %in% 1752] <- "K4107"

# THERES NO SCAN OF SHEET 63 OR 64 !!!!!
# there's only one error there but UGH

# looking for info on K4298
sameB[sameB$sheet %in% c(63,64), ]

# found it
# but this is a weird fix... I think it needs to go below
# it does. find this line below
# cwb$bid[cwb$catNum %in% "K4298"] <- sameB$bid[grepl("K4298", sameB$notes)]

# sameB[sameB$sheet %in% 65, ]
sameB$catNumMin[sameB$line %in% 1980] <- "K4317"
sameB$catNumMax[sameB$line %in% 1980] <- "K4321"

sameB$catNumMin[sameB$line %in% 1973] <- "K4312"


#algright this last ones a doozy
sameB[sameB$catNumMin %in% "K4237", ]

# NO SCAN OF 61!

sameB[sameB$sheet %in% 61, ]

# okay this actually is very clear

sameB$catNumMin[sameB$line %in% 1844] <- "K5999"
sameB$catNumMax[sameB$line %in% 1844] <- "K5999"


cwb <- updateBranch()

cwb[is.na(cwb$bid), ]


# this edit goes last!
cwb$bid[cwb$catNum %in% "K4298"] <- sameB$bid[grepl("K4298", sameB$notes)]

cwb[is.na(cwb$bid), ] #clear!

# now let's spend a lot of lines making a df that's only marginally better
# wow this was so prescient this took me so damn long

cwb$treeSpecies <- as.factor(cwb$treeSpecies)
levels(cwb$treeSpecies) <- c(levels(cwb$treeSpecies), "RUBPH")


tempCwbb <- merge(cwb, sameB, by = "bid", all = T)
xyMiss <- tempCwbb$catNum[tempCwbb$treeSpecies.x != tempCwbb$treeSpecies.y & 
                          !is.na(tempCwbb$treeSpecies.x)]
cwb <- cwb[!cwb$catNum %in% xyMiss, ]
cwbb <- merge(cwb, sameB, by = "bid", all = T)

cwbb <- cwbb %>%
  mutate(treeSpecies = ifelse(is.na(treeSpecies.x), treeSpecies.y, as.character(treeSpecies.x))) %>%
  mutate(hostFamily = ifelse(is.na(hostFamily.x), hostFamily.y, hostFamily.x)) %>%
  mutate(hostNative = ifelse(is.na(hostNative.x), hostNative.y, hostNative.x)) %>%
  select(-hostNative.x, -hostNative.y, 
         -treeSpecies.x, -treeSpecies.y, 
         -hostFamily.x, -hostFamily.y)

save(cwbb, file = "data/clean/catsWithBranch.Rdata")

# THE DATASET IS STILL MISSING 6 CATS IF THIS MESSAGE STILL EXISTS
# #PRAY FOR xyMiss (and by pray I mean look at the physical cups)
xyMiss

# # only relevant if the lines before the cwbb merge is removed (should happen eventually) ####
# # unfortunately there's one more error check
# #
# # wow an actual use for indexing with != instead of !%in%... incredible!
# cwbb[cwbb$treeSpecies.x != cwbb$treeSpecies.y & !is.na(cwbb$treeSpecies.x), ]
# 
# # I did this wrong at first and thought there were none RIP
# # that being said there's only 6 so it's all good
# 
# xyMiss <- cwbb$catNum[cwbb$treeSpecies.x != cwbb$treeSpecies.y & !is.na(cwbb$treeSpecies.x)]
# 
# # no usual suspects... how do I investigate this?
# # I think it's back to the sheets ugh
# 
# cwbb$sheet[cwbb$catNum %in% xyMiss]
# 
# # okay looks like the best way to check these will be physically
# # going to remove them for now, but they should be added back in!!!
# # they will be removed before the creation of cwbb (above)

# make graphs ####

# okay so now that I spent an eternity making cwbb what do I have the power to do?
# I can eliminate caterpillars I don't like as not having existed in the summary stats
# for instance remove micros
cwbb$catSpecies[cwbb$catSpecies %in% "MICROX"] <- NA
table(cwbb$catSpecies)

# and now my graphs change

btab <- cwbb %>%
  filter(!treeSpecies %in% c("RHOSC", "RUBPH")) %>%
  group_by(treeSpecies, hostFamily, hostNative, .drop = F) %>%
  summarise(total = sum(!is.na(catSpecies)), 
            rate = sum(!is.na(catSpecies))/length(unique(bid)),
            error = sd(!is.na(catSpecies))/(sqrt(length(unique(bid))))) %>%
  mutate(rate = ifelse(is.nan(rate), 0, rate)) %>%
  arrange(hostFamily, -rate)

level_order <- btab$treeSpecies

p1 <- ggplot(data = btab, aes(x = factor(treeSpecies, level = level_order),
                           y = rate, 
                           fill = hostFamily)) +
  geom_bar(stat = "identity", aes(alpha = hostNative)) +
  # geom_bar(data = btab[btab$hostNative == "native", ], stat = "identity") +
  geom_errorbar(aes(ymin = rate - error, ymax = rate + error), width = .25) +
  theme_tufte() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line = element_line(color = "black", size = .1),
    legend.position = "none"
  ) +
  xlab("Tree species") +
  ylab("Caterpillars per branch") +
  scale_fill_manual(name = "Host Family",
                    labels = c("Caprifoliaceae", "Invasive outgroups",
                               "Oleaceae", "Roseaceae"),
                    values = c("#6C9A8B", "#020887", "#FFA630", "#840032")) +
  scale_alpha_manual(values = c(.3, 1),
                     breaks = c("exotic", "native"))

p1

# perTree <- perTree %>%
#   arrange(hostFamily, -total)
# 
# level_order <- perTree$treeSpecies
p2 <- ggplot(data = btab, aes(x = factor(treeSpecies, level = level_order),
                           y = total, 
                           fill = hostFamily)) +
  geom_bar(stat = "identity", alpha = .3) +
  geom_bar(data = btab[btab$hostNative == "native", ], stat = "identity") +
  theme_tufte() +
  theme(
    axis.text.x.bottom = element_text(angle = 45, hjust = 1.1),
    legend.position = "none",
    axis.line = element_line(color = "black", size = .1)
  ) +
  scale_y_continuous(trans = "log1p", 
                     breaks = c(0,2, 10, 25, 50, 100, 200)) +
  xlab("Tree species") +
  ylab("Total observed caterpillars") +
  scale_fill_manual(name = "Host Family",
                    labels = c("Caprifoliaceae", "Invasive outgroups",
                               "Oleaceae", "Roseaceae"),
                    values = c("#6C9A8B", "#020887", "#FFA630", "#840032")) +
  scale_alpha_manual(values = c(.3, 1),
                     breaks = c("exotic", "native"))


p1 / p2 



sum(btab$total)

btab %>%
  group_by(hostNative) %>%
  summarise(sum(total))

nrow(cw[!cw$catSpecies %in% "MICROX", ])

cwb %>%
  dplyr::select(catSpecies, treeSpecies)

intMat <- as.matrix(table(cwb$treeSpecies, cwb$catSpecies))
exMat <- intMat[, colnames(intMat) %in% c("UNIDXX", "UNKNXX", "NOCTXX", "GEOMXX")]
intMat <- intMat[, !colnames(intMat) %in% c("MICROX", "UNIDXX", "UNKNXX", "NOCTXX", "GEOMXX")]

sum(exMat)

exos <- btab$treeSpecies[btab$hostNative %in% "exotic"]
nats <- btab$treeSpecies[!btab$hostNative %in% "exotic"]

intMat <- intMat[c(nats, exos), ]

plotweb(t(intMat),
        # method = "normal",
        col.high = ifelse(row.names(intMat) %in% exos, "#3A577E", "#6FBCEB"),
        col.low = "forestgreen",
        text.rot = 90, 
        labsize =  1.8
        )

load("data/clean/catWeights21.rdata")

cwMat <- cw %>%
  mutate(isToid = ifelse(fate == "T", 1, 0)) %>%
  mutate(isPupal = ifelse(grepl("O|P", fate, ignore.case = T), 1, 0)) %>%
  mutate(isDead = ifelse(fate == "D", 1, 0)) %>%
  mutate(isMissing = ifelse(fate %in% c("MIA","K"), 1, 0)) %>% 
  select(catSpecies, starts_with("is")) %>%
  group_by(catSpecies) %>%
  summarise(toided = sum(isToid),
            pupal = sum(isPupal),
            dead = sum(isDead),
            missing = sum(isMissing)) %>%
  filter(catSpecies %in% colnames(intMat)) %>%
  column_to_rownames(var = "catSpecies") %>%
  as.matrix()



plotweb2(intMat, cwMat)



# try igraph ####

g <- graph_from_incidence_matrix(t(intMat))

layer <- rep(2, length(V(g)$name))
layer[grep("^.{6}$",V(g)$name)] <- 1
layout = layout_with_sugiyama(g, layers=layer)
plot(g, 
     layout=cbind(layer,layout$layout[,1]),
     vertex.shape=c("square","circle")[layer],
     vertex.size=c(50,20)[layer],
     vertex.label.dist=c(0,0)[layer],
     vertex.label.degree=0)

# end ####
save(sameB, file = "data/clean/cleanBranch.rdata")
  





