# This script looks at and neatens up the caterpillar data sheet
# the df that it outputs will be compared to the branch sheet in a later script

# set up ####

rm(list = ls())
source("data/cleaning/cleaningFunctions.R")
file.remove("data/cleaning/makeClosetSearch.R")
file.remove("data/cleaning/fixIssuesCaterpillar.R")
cat("i <- 1\ntheseProbs <- data.frame(catNum = character(0),
                         catSpecies = character(0),
                         treeSpecies = character(0),
                         fate = character(0),
                         photograph = numeric(0),
                         issue = character(0))\n", file = "data/cleaning/makeClosetSearch.R")

# load data ####
cats <- read.csv("data/dirty/cat/cat.csv")
wt <- read.csv("data/dirty/cat/weights.csv")

# bulk edits ####
cats$changeID <- toupper(cats$changeID)
cats$fate <- toupper(cats$fate)
cats$frass <- toupper(cats$frass)
cats$emerged <- toupper(cats$emerged)

cats <- cats %>%
  rename(fieldNotes = X.notes)
cats$fate <- gsub(" ","", cats$fate)

# Find poorly named and already re-IDed caterpillars ####

cats$changeID[cats$changeID %in% " "] <- ""
cats[cats$changeID %in% "Y",]

problem("J2873", problemNote = "ReID was recorded as Y; what does it actually say?")
problem("K4532", problemNote = "ReID was recorded as Y; what does it actually say?")

cats <- cats %>%
  rename(ogCatSpecies = catSpecies) %>%
  mutate(catSpecies = ifelse(!changeID %in% "", changeID, ogCatSpecies)) %>%
  relocate(catSpecies, .before = ogCatSpecies) %>%
  relocate(ogCatSpecies, .after = recorded.in.first.wave)

# find "caterpillars" to be eliminated ####
# table(cats$catSpecies)

cats <- cats[!cats$catSpecies %in% "",]
cats <- cats[!cats$catSpecies %in% "BROWNX",]
cats <- cats[!cats$catSpecies %in% "SAWFLX",]
cats <- cats[!cats$catSpecies %in% "GRAPEX",]

# fix based on photos ####


photos <- cats[!cats$photograph %in% "",]
invisible(lapply(photos$catNum, function(x) cat("cats$photoID[cats$catNum %in% '", x, "'] <- \n", sep = "")))

cats$photoID <- ""
cats$photoID[cats$catNum %in% 'J2067'] <- "HYPAUN"
problem("J2099", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'J2102'] <- "ORTHHI"
cats$photoID[cats$catNum %in% 'J2103'] <- "EUPIXX"
cats$photoID[cats$catNum %in% 'J2139'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'J2164'] <- "ZALEXX"
cats$photoID[cats$catNum %in% 'J2165'] <- "ZALEXX"
cats$photoID[cats$catNum %in% 'J2171'] <- "ORTHHI"
cats$photoID[cats$catNum %in% 'J2252'] <- "ENNOSU"
cats$photoID[cats$catNum %in% 'J2269'] <- "PROCLI"
problem("J2327", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'J2343'] <- "EUPIMI"
cats$photoID[cats$catNum %in% 'J2509'] <- "HYPAUN"
problem("J2609", "search SD card for photo or see if cup actually says photo")
problem("J2616", "search SD card for photo or see if cup actually says photo")
problem("J2617", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'J2660'] <- "ACHADI"
cats$photoID[cats$catNum %in% 'J2676'] <- "DAVEXX NOCLUE"
cats$photoID[cats$catNum %in% 'J2682'] <- "EUPIMI"
problem("J2754", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'J2839'] <- "MESORU"
problem("J2843", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'J2844'] <- "MESORU"
cats$photoID[cats$catNum %in% 'J2848'] <- "ORTHHI"
cats$photoID[cats$catNum %in% 'J2875'] <- "PROCLI"
cats$photoID[cats$catNum %in% 'J2878'] <- "ACHADI"
cats$photoID[cats$catNum %in% 'J2901'] <- "LITHXX"
cats$photoID[cats$catNum %in% 'J2902'] <- "DAVEXX CLADAT"
cats$photoID[cats$catNum %in% 'J2941'] <- "IRIDLA"
cats$photoID[cats$catNum %in% 'J2993'] <- "ZALEXX"
cats$photoID[cats$catNum %in% 'J3037'] <- "PAPIGL"
cats$photoID[cats$catNum %in% 'J3057'] <- "EUPIXX"
cats$photoID[cats$catNum %in% 'J3124'] <- "PALTAN"
cats$photoID[cats$catNum %in% 'J3129'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'J3131'] <- "IRIDLA"
cats$photoID[cats$catNum %in% 'J3136'] <- "ORTHHI"
cats$photoID[cats$catNum %in% 'J3137'] <- "MICROX"
cats$photoID[cats$catNum %in% 'J3139'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'J3142'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'J3146'] <- "ACROSU"
cats$photoID[cats$catNum %in% 'J3164'] <- "ORTHHI"
cats$photoID[cats$catNum %in% 'J3168'] <- "ORTHHI"
cats$photoID[cats$catNum %in% 'J3170'] <- "ACHADI"
cats$photoID[cats$catNum %in% 'J3219'] <- "MICROX"
cats$photoID[cats$catNum %in% 'J3224'] <- "DAVEXX PROCLI"
cats$photoID[cats$catNum %in% 'J3231'] <- "DAVEXX HYPAUN"
cats$photoID[cats$catNum %in% 'J3235'] <- "DAVEXX NOT HYPAUN"
cats$photoID[cats$catNum %in% 'J3248'] <- "ZALEXX"
cats$photoID[cats$catNum %in% 'J3250'] <- "MICROX"
cats$photoID[cats$catNum %in% 'J3254'] <- "MICROX"
cats$photoID[cats$catNum %in% 'J3428'] <- "PYRRIS"
cats$photoID[cats$catNum %in% 'J3431'] <- "DAVEXX HERM"
cats$photoID[cats$catNum %in% 'J3433'] <- "DAVEXX HERM"
cats$photoID[cats$catNum %in% 'J3434'] <- "DAVEXX HERM"
cats$photoID[cats$catNum %in% 'J3435'] <- "MICROX"
cats$photoID[cats$catNum %in% 'J3440'] <- "MICROX"
problem("J3450", "search SD card for photo or see if cup actually says photo")
problem("J3451", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'J3468'] <- "ECTRCR"
cats$photoID[cats$catNum %in% 'J3469'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'J3477'] <- "EUPIXX"
cats$photoID[cats$catNum %in% 'J3485'] <- "DAVEXX HERM"
cats$photoID[cats$catNum %in% 'J3582'] <- "PROCLI"
cats$photoID[cats$catNum %in% 'J3588'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'J3590'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'J3591'] <- "PROCLI"
cats$photoID[cats$catNum %in% 'J3602'] <- "MORRLA"
cats$photoID[cats$catNum %in% 'J3607'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4003'] <- "CERAUN"
cats$photoID[cats$catNum %in% 'K4005'] <- "EUPIXX"
cats$photoID[cats$catNum %in% 'K4006'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4008'] <- "IRIDLA"
cats$photoID[cats$catNum %in% 'K4010'] <- "CAMPPE"
cats$photoID[cats$catNum %in% 'K4012'] <- "HYALCE"
cats$photoID[cats$catNum %in% 'K4014'] <- "BISTBE"
cats$photoID[cats$catNum %in% 'K4015'] <- "MORRCO"
cats$photoID[cats$catNum %in% 'K4016'] <- "MORRLA"
cats$photoID[cats$catNum %in% 'K4017'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4019'] <- "DAVEXX EUPIXX"
cats$photoID[cats$catNum %in% 'K4020'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4021'] <- "DAVEXX UNKNXX"
cats$photoID[cats$catNum %in% 'K4022'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4023'] <- "DAVEXX GEOMXX"
cats$photoID[cats$catNum %in% 'K4024'] <- "DAVEXX GEOMXX"
cats$photoID[cats$catNum %in% 'K4026'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4032'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4069'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4080'] <- "PSEUCY"
cats$photoID[cats$catNum %in% 'K4082'] <- "DAVEXX PROTPO"
cats$photoID[cats$catNum %in% 'K4084'] <- "PSEUCY"
cats$photoID[cats$catNum %in% 'K4085'] <- "CAMPPE"
cats$photoID[cats$catNum %in% 'K4104'] <- "ZALEXX"
cats$photoID[cats$catNum %in% 'K4106'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4107'] <- "PALTAN"
cats$photoID[cats$catNum %in% 'K4133'] <- "ENNOSU"
cats$photoID[cats$catNum %in% 'K4159'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4187'] <- "HYPAUN share w/dave"
cats$photoID[cats$catNum %in% 'K4189'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4190'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4220'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4222'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4223'] <- "PALTAN"
cats$photoID[cats$catNum %in% 'K4224'] <- "DAVEXX MICROX"
cats$photoID[cats$catNum %in% 'K4244'] <- "DAVEXX HERM"
cats$photoID[cats$catNum %in% 'K4245'] <- "DAVEXX PROTPO"
cats$photoID[cats$catNum %in% 'K4246'] <- "PROCLI"
cats$photoID[cats$catNum %in% 'K4249'] <- "EUPIXX"
cats$photoID[cats$catNum %in% 'K4280'] <- "MORRLA"
cats$photoID[cats$catNum %in% 'K4297'] <- "DAVEXX MICROX"
cats$photoID[cats$catNum %in% 'K4311'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4319'] <- "HETEGU"
problem("K4322", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'K4324'] <- "MELACA"
cats$photoID[cats$catNum %in% 'K4329'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4341'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4373'] <- "PYRRIS"
cats$photoID[cats$catNum %in% 'K4376'] <- "MICROX"
cats$photoID[cats$catNum %in% 'K4378'] <- "DAVEXX BUTTTX"
cats$photograph[cats$catNum %in% 'K4379'] <- NA
cats$photoID[cats$catNum %in% 'K4406'] <- "RHEUPR"
cats$photoID[cats$catNum %in% 'K4437'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4439'] <- "HYPAUN"
cats$photoID[cats$catNum %in% 'K4442'] <- "CAMPPE"
cats$photoID[cats$catNum %in% 'K4451'] <- "DAVEXX HERMXX"
cats$photoID[cats$catNum %in% 'K4455'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4457'] <- "LOMOVE"
cats$photoID[cats$catNum %in% 'K4460'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4537'] <- "DAVEXX PROCLI"
cats$photoID[cats$catNum %in% 'K4544'] <- "DAVEXX BUTTTX"
cats$photoID[cats$catNum %in% 'K4565'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'K4568'] <- "DAVEXX PROTPO"
cats$photoID[cats$catNum %in% 'K4570'] <- "MICROX"
cats$photoID[cats$catNum %in% 'K4572'] <- "DAVEXX PROTPO"
problem("K4624", "search SD card for photo or see if cup actually says photo")
problem("K4627", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'K4686'] <- "DAVEXX HERMXX"
cats$photoID[cats$catNum %in% 'K4694'] <- "PROCLI"
cats$photoID[cats$catNum %in% 'K4715'] <- "MICROX"
cats$photoID[cats$catNum %in% 'K4716'] <- "PROCLI"
cats$photoID[cats$catNum %in% 'K4719'] <- "EUPIXX"
cats$photoID[cats$catNum %in% 'K4721'] <- "PROCLI"
cats$photoID[cats$catNum %in% 'K4723'] <- "DAVEXX UNKXX"
cats$photoID[cats$catNum %in% 'K4725'] <- "PROCLI"
problem("K4726", "search SD card for photo or see if cup actually says photo")
cats$photoID[cats$catNum %in% 'K4727'] <- "MICROX"
cats$photoID[cats$catNum %in% 'K5002'] <- "PROCLI"
cats$photoID[cats$catNum %in% 'K5032'] <- "DAVEXX HERMXX"

cats <- cats %>%
  mutate(catSpecies = ifelse(nchar(photoID) == 6, photoID, catSpecies),
         showDave = ifelse(grepl("dave", photoID, ignore.case = T), "yes", ""))

table(cats$catSpecies[cats$showDave == ""])

cats$catSpecies[cats$catSpecies %in% "ALSOPO"] <- "CAMPPE"
cats$catSpecies[cats$catSpecies %in% "ACROXX"] <- "ACROSU"
cats$catSpecies[cats$catSpecies %in% "EPIMHO"] <- "CORYME"
cats$catSpecies[cats$catSpecies %in% "ERANTI"] <- "RHEUPR"
cats$catSpecies[cats$catSpecies %in% "FUZZYX"] <- "PYRRIS"
cats$catSpecies[cats$catSpecies %in% "LIKELY MORRLA SAYS CROCNO"]  <- "MORRLA"
cats$catSpecies[cats$catSpecies %in% "LIKELY WAS ACTUALLY MORRLA"]  <- "MORRLA"
cats$catSpecies[cats$catSpecies %in% "PTEROX"] <- "MICROX"

table(cats$catSpecies[cats$showDave == ""])
cats[cats$showDave == "yes",]

cats[cats$catSpecies %in% "ZALEXX" & cats$date == 714,]

# Fix obvious spelling errors and known reIDs ####
# table(cats$catSpecies)

cats$catSpecies[cats$catSpecies %in% "HYPAUN "] <- "HYPAUN"
cats$catSpecies[cats$catSpecies %in% "STRIPX"] <- "HYPAUN"
cats$catSpecies[cats$catSpecies %in% "BUTTX"] <- "BUTTTX"
cats$catSpecies[cats$catSpecies %in% "CRABXX"] <- "MICROX"
cats$catSpecies[cats$catSpecies %in% "SMICRO"] <- "MICROX"
cats <- cats[!cats$catSpecies %in% "MINEXX",]
cats$catSpecies[cats$catSpecies %in% "MIROX"] <- "MICROX"
cats$catSpecies[cats$catSpecies %in% "MICROCX"] <- "MICROX"
cats$catSpecies[cats$catSpecies %in% "ROUGEX"] <- "MICROX"
cats$catSpecies[cats$catSpecies %in% "LONMOVE"] <- "LOMOVE"
cats$catSpecies[cats$catSpecies %in% "LITHAN"] <- "LITHXX"
cats$catSpecies[cats$catSpecies %in% "TRICAL"] <- "MESORU"
cats$catSpecies[cats$catSpecies %in% "EUPISW"] <- "EUPIXX"
cats$catSpecies[cats$catSpecies %in% "UNKXX"] <- "UNKNXX"
cats$catSpecies[cats$catSpecies %in% "ZALELU"] <- "ZALEXX"
cats$catSpecies[cats$catSpecies %in% "XALEXX"] <- "ZALEXX"

table(cats$catSpecies[cats$showDave == ""])

cats$catSpecies[cats$catSpecies %in% "SCHIXX"] <- 'SCHIUN'

table(cats$treeSpecies)
id <- cats$catNum[cats$treeSpecies %in% "LITHXX"]
problem(id, "what is the host species?")
id <- cats$catNum[cats$treeSpecies %in% "MELACA"]
problem(id, "what is the host species?")
id <- cats$catNum[cats$treeSpecies %in% "MICROX"]
problem(id, "what is the host species?")

# fix obviously bad host IDs ####

cats$treeSpecies[cats$treeSpecies %in% "AMECA"] <- "AMEXX"
cats$treeSpecies[cats$treeSpecies %in% "AMELA"] <- "AMEXX"
cats$treeSpecies[cats$treeSpecies %in% "ASH 1"] <- "FRAAM"
cats$treeSpecies[cats$treeSpecies %in% "FRAXX"] <- "FRAAM"
cats$treeSpecies[cats$treeSpecies %in% "LIGVU"] <- "LIGOB"
cats$treeSpecies[cats$treeSpecies %in% "LONTA"] <- "LONMO"
cats$treeSpecies[cats$treeSpecies %in% "ROSNU"] <- "ROSMU"
cats$treeSpecies[cats$treeSpecies %in% "RUBAL"] <- "RUBXX"

# table(cats$treeSpecies)

# misc changes ####

cats$treeSpecies[cats$treeSpecies %in% "VIBPL" & cats$date < 621] <- "VIBSI"

# add plant information ####

invasives <- c("BERTH", 'EUOAL', 'LIGOB', 'LONMO', 'MALXX', 'ROSMU','VIBDI','VIBPL','VIBSI')
cats$hostNative <- "native"
cats$hostNative[cats$treeSpecies %in% invasives] <- "exotic"

cats$hostFamily <- "Roseaceae"
cats$hostFamily[cats$treeSpecies %in% c("BERTH", 'EUOAL')] <- 'InvasiveOutgroups'
cats$hostFamily[cats$treeSpecies %in% c("VIBLE","LONMO","VIBDE","VIBDI","VIBAC", "VIBPL","VIBSI","SAMCA")] <- 'Caprifoliaceae'
cats$hostFamily[cats$treeSpecies %in% c("FRAAM", "LIGOB")] <- 'Oleaceae'

# fix fates ####
# table(cats$fate)

fixFate <- function(bad, good){
  cats$fate[cats$fate %in% bad] <- good
  return(cats)
}

cats <- fixFate("AWOL", "MIA")
cats <- fixFate("DEAD", "D")

table(cats$fate)

# oh boy alright here are the problem children
noFate <- cats[cats$fate %in% "",]
nfn <- noFate$catNum
invisible(lapply(nfn, function(x) cat("problem('", x, "', 'What is the fate?')\n", sep = "")))

noFate %>% 
  select(date, catNum, catSpecies, fieldNotes)

problem('J2265', 'What is the fate?')
problem('J2276', 'What is the fate?')
problem('J2439', 'What is the fate?')
problem('J2612', 'What is the fate?')
problem('J2821', 'What is the fate?')
problem('J3130', 'What is the fate?')
cats$fate[cats$catNum %in% "J3228"] <- "MIA"
cats$fateDate[cats$catNum %in% "J3228"] <- 623
problem('J3554', 'What is the fate?')
problem('K4027', 'What is the fate?')
cats$fate[cats$catNum %in% "K4228"] <- "MIA"
cats$fateDate[cats$catNum %in% "K4228"] <- 706
problem('K4233', 'What is the fate?')
problem('K4234', 'What is the fate?')
problem('K4235', 'What is the fate?')
problem('K4346', 'What is the fate?')
cats$fate[cats$catNum %in% "K4438"] <- "MIA"
cats$fateDate[cats$catNum %in% "K4438"] <- 715
problem('K4571', 'What is the fate?')
problem('K4631', 'What is the fate?')
problem('K4632', 'What is the fate?')
cats$fate[cats$catNum %in% "K4993"] <- "MIA"
cats$fateDate[cats$catNum %in% "K4993"] <- 723
cats$fate[cats$catNum %in% "K5005"] <- "MIA"
cats$fateDate[cats$catNum %in% "K5005"] <- 723
problem('K5007', 'What is the fate?')


# other misc columns check ####

table(cats$frass)
table(cats$emerged)

# notes check (rough) ####
rearingIssues <- cats[!cats$rearingNotes %in% "",] %>%
  select(catNum, catSpecies, treeSpecies, fate, frass, photograph, rearingNotes)

cats <- cats[!cats$catNum %in% "J2070",]

# final species check (its still a lot)

cats[cats$catSpecies %in% "FOURLX", ]
cats[cats$catSpecies %in% "GEOMXX" & cats$showDave == "" & !cats$fate %in% c("K", "MIA"), ]
cats$catSpecies[cats$catNum %in% 'K4529'] <- "EUTRCL"
cats$catSpecies[cats$catNum %in% 'K4718'] <- "EUTRCL"
problem("K5006", "Any chance we can figure out what species this was?")

# caterpillars that have photos that were not otherwise noted

cats$photoID[cats$catNum %in% 'J2565'] <- "DAVEXX GEOMXX"
cats$photoID[cats$catNum %in% 'J3130'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'J3243'] <- "PROTPO"
cats$photoID[cats$catNum %in% 'J4030'] <- "PROCLI"

cats$showDave[cats$catNum %in% 'J2565'] <- "yes"
cats$catSpecies[cats$catNum %in% 'J3130'] <- "PROTPO"
cats$catSpecies[cats$catNum %in% 'J3243'] <- "PROTPO"
cats$catSpecies[cats$catNum %in% 'J4030'] <- "PROCLI"

table(cats$catSpecies[cats$showDave == ""])

cats$catSpecies[cats$catSpecies %in% "ORTHXX"] <- "ORTHRU"
cats$catSpecies[cats$catSpecies %in% "UNKNXX" & cats$showDave == "" & cats$date == cats$fateDate] <- "UNIDXX"
cats[cats$catSpecies %in% "UNKNXX" & cats$showDave == "", ]

cats <- cats[!cats$catNum %in% "J2532",] # fly larva
cats$catSpecies[cats$catNum %in% "J2592"] <- "UNIDXX"
cats$catSpecies[cats$catNum %in% "J3232"] <- "UNIDXX"

# K4459 is IDable after K4451 which is being shown to dave
# same with K5007 

table(cats$catSpecies[cats$showDave == "" & !cats$fate %in% c("K", "MIA")])

cats[cats$catSpecies %in% "NOCTXX" & cats$showDave == "" & !cats$fate %in% c("K", "MIA"), ]
cats[cats$catSpecies %in% "NOT ORTHRU" & cats$showDave == "" & !cats$fate %in% c("K", "MIA"), ]




