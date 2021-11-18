table(cats$treeSpecies)

cw <- merge(cats, wt, by = "catNum")
cwc <- cw[cw$weirdFinal %in% "" & !is.na(cw$finalWeight),]

cwc$wtChange <- cwc$finalWeight - cwc$initialWeight 
cwc$ge <- cwc$wtChange/cwc$frassWeight
cwc <- cwc[!is.nan(cwc$ge),]

boxplot(log(wtChange) ~ nat, data = cwc)
boxplot(ge ~ nat, data = cwc[cwc$ge < 3,])

bigCats <- names(table(cwc$catSpecies)[table(cwc$catSpecies) > 10])
bigCats <- bigCats[!bigCats %in% "GEOMXX", "MICROX", "RHEUPR", "CORYME"]

boxplot(ge ~ nat, data = cwc[cwc$catSpecies %in% "CROCNO",])
boxplot(ge ~ nat, data = cwc[cwc$catSpecies %in% "ORTHRU",])
boxplot(ge ~ nat, data = cwc[cwc$catSpecies %in% "MELACA",])
boxplot(ge ~ nat, data = cwc[cwc$catSpecies %in% "LOMOVE",])

boxplot(ge ~ treeSpecies, data = cwc[cwc$catSpecies %in% "MELACA",])

m1 <- lmer(ge ~ nat + (1|fam), data = cwc[cwc$catSpecies %in% "MELACA",])
summary(m1)

m2 <- lmer(ge ~ nat + (1|fam) + (1|catSpecies), data = cwc[cwc$catSpecies %in% bigCats,])
summary(m2)