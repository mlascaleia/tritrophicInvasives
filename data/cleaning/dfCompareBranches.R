# This function sets up the two branch data sheets to be compared
# Then is where I ran the function to actually compare them
# the reconciling the branch data frames are made in another script

# set up ####

rm(list = ls())
library(tidyverse)

# load sheets ####

b1 <- read.csv("data/dirty/branch/Branch Data - Sheet1.csv")
b2 <- read.csv("data/dirty/branch/Branch Data - Sheet2.csv")

b1$lfCount <- as.numeric(b1$lfCount)
b2$lfCount <- as.numeric(b2$lfCount)

# get set up to be compared ####
# give initial lines. These will need to be overwritten in the future

b1$line <- 1:nrow(b1)
b2$line <- 1:nrow(b2)


# fix columns ####
# oof okay right off the bat one has more rows and more columns

colnames(b2)
colnames(b1)

sum(!is.na(b2$data.entry.notes)) # there are no data entry notes in sheet 2

b1$data.entry.notes[!b1$data.entry.notes %in% ""] # nothing useful here

b1$data.entry.notes <- NULL
b2$data.entry.notes <- NULL

sum(is.na(b2$X)) # nothing here either

b2$X <- NULL

# fix rows ####
# b2 has one more row than b1. The best way to figure this out will be rows per page
# unfortunately the page numbers in b1 were not dragged down
# (robi dont look its a for loop)

for(i in 1:nrow(b1)){
  if(!is.na(b1$sheet[i])){
    ns <- b1$sheet[i]
  } else {
    b1$sheet[i] <- ns
  }
}

# which ones are different?

uDifs <- function(){
  dif <- table(b1$sheet) - table(b2$sheet)
  (difs <- dif[dif != 0])
}

# oh no. oh no no no. That's A LOT.
# I hope I messed up somewhere?
# they come in pairs, so I think it's just the placement of the sheet name that was messed up
# gonna make a quick tool that invesitgates these

lookSheet <- function(latterSheet, rows = 3){
  row1 <- min(which(b1$sheet == latterSheet))
  print(b1[(row1 - rows):(row1 + rows - 1),])
  row2 <- min(which(b2$sheet == latterSheet))
  print(b2[(row2 - rows):(row2 + rows - 1),])
}

uDifs()
lookSheet(2)  
lookSheet(3)  # these both look fine, so I think 2 is literally just missing one in b1
lookSheet(4) # again, this looks fine, so I think 3 also has an extra one thrown in 

# I looked at the data sheets and this is, in fact, the case

b1 <- b1[!b1$line %in% 68,]
b2 <- b2[!b2$line %in% 44,]

uDifs() #finally one I can actually fix!
lookSheet(6, 25)
b1$sheet[b1$line %in% 143:160] <- 5

uDifs()
lookSheet(15) 
b2$sheet[b2$line %in% 443] <- 14

uDifs()
lookSheet(22)
b2$sheet[b2$line %in% 598] <- 21

uDifs()
lookSheet(30, 10)
b1$sheet[b1$line %in% 855:860] <- 29

uDifs()
# gonna pause on 33 and 48 as they are unpaired, I'll address them in the next section

lookSheet(52, 15)
b1$sheet[b1$line %in% 1544:1552] <- 52

uDifs()
lookSheet(64)
b2$sheet[b2$line %in% 1916] <- 64

uDifs()
lookSheet(70, 18)
b1$sheet[b1$line %in% 2101:2112] <- 69

uDifs()
lookSheet(83)
b1$sheet[b1$line %in% 2541] <- 82

uDifs() # alright just the two left. Going to side by side compare them to the data sheet

# 33 #
# 33 has 33 lines on the data sheet. 
length(b1$line[b1$sheet %in% 33]) # b1 checks out
length(b2$line[b2$sheet %in% 33]) # b2 does not

b2[b2$sheet %in% 33,] # simply a skipped line. These are always so hard to add...
dput(colnames(b2))
add33 <- data.frame("sheet" = 33, "date" = 617, "recorder" = "KD", "collector" ="RP" , 
                    "transect" = 133, "treeSpecies" = "ROSMU", 
                    "branchNum" = 1, "catCount" = 1, "catNumMin" = "J3184", "catNumMax" = "J3184",
                    "lfCount" = 400, "notes" = "", "line" = 967.1)
b2 <- rbind(b2, add33)

# 48
# 48 has 33 lines on the data sheet. 
length(b1$line[b1$sheet %in% 48]) # b1 does not check out
length(b2$line[b2$sheet %in% 48]) # b2 checks out

b1[b1$sheet %in% 48,]

add48 <- data.frame("sheet" = c(48,48), "date" = c(628,628), "recorder" = c("ML","ML"),
                    "collector" =c("PDHW","PDHW") , "transect" = c(153,153), "treeSpecies" = c("AMEXX","CRAXX"), 
                    "branchNum" = c(1,1), "catCount" = c(1,1), "catNumMin" = c("J3446","J3447"),
                    "catNumMax" = c("J3446","J3447"),"lfCount" = c(150,170), "notes" = c("",""), "line" = c(1442.1, 1442.2))
b1 <- rbind(b1, add48)
rm(add33, add48)

uDifs() # hooray!

# now it's time for row order

b1 <- arrange(b1, sheet, line)
b2 <- arrange(b2, sheet, line)

b1$line <- 1:nrow(b1)
b2$line <- 1:nrow(b2)

# hopefully it was as easy as that, but let's check

rando <- sample(1:nrow(b1), 10)
b1$lfCount[rando] - b2$lfCount[rando] # this wont always give 10 0's, but it almost always should!
# it gave me 69/70 0's, so I think it's all set!

rm(i, ns, rando, uDifs, lookSheet)

# grammar and syntax
# before we get into the meat and potatoes, their are many small things that can be corrected
# for instance changing the whole thing to uppercase will save us a lot of time

b1[,sapply(b1, class) == "character"] <- sapply(b1[,sapply(b1, class) == "character"], FUN = toupper)
b2[,sapply(b2, class) == "character"] <- sapply(b2[,sapply(b2, class) == "character"], FUN = toupper)

# then the other big thing will be cleaning up the collector column
# I think I'll be lazy and just correct for all commas, semicolons, colons, or slashes

b1$collector <- gsub("(;|/|:|,| )", "", b1$collector)
b2$collector <- gsub("(;|/|:|,| )", "", b2$collector)

table(b1$collector)
table(b2$collector)

# okay, now that the things I know about are out of the way, I'm just going to run through each column
# and see if there's anything egregious that can be fixed now

colnames(b1)
# skipping sheet

table(b1$date)
table(b2$date)

table(b1$recorder)
table(b2$recorder)

table(b1$transect)
table(b2$transect) # let's just nix those T's right here

b1$transect <- gsub("T", "", b1$transect)
b2$transect <- gsub("T", "", b2$transect)

# there's also that one "7129" in b2

b2$transect <- gsub("^7", "", b2$transect)

table(b1$treeSpecies)
table(b2$treeSpecies)

table(b1$branchNum)
table(b2$branchNum) # lol why are there so many 0's

b1$catNumMin[b1$catNumMin %in% "-"] <- ""
b2$catNumMin[b1$catNumMin %in% "-"] <- ""
b1$catNumMin[b1$catNumMax %in% "-"] <- ""
b2$catNumMin[b1$catNumMax %in% "-"] <- ""

# the rest are not going to have patterned errors, so I'm calling it there

# comparing ####

# the moment we've all been waiting for!

source("data/cleaning/cleaningFunctions.R")

# starting first compare 9/14/21 11:43 AM

dfCompare(b1, b2, discrepancy = "1;1" ,outputFile = "data/cleaning/combineDFs.R")









