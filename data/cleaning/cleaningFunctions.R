# this script contains usefull functions for cleaning data #

# set up ####

library(tidyverse)

# creating lines to test things
# these will ultimately be commented out

b11 <- b1[1:10,]
b22 <- b2[1:10,]


# df compare function ####

dfCompare <- function(b11, b22, discrepancy = "1;1", outputFile){
  for(i in 1:nrow(b11)){
    for(j in which(!b11[i,] == b22[i,])){
      g <- T
      while(g){
        correct <- readline(prompt = cat(b11[!b11 == b22][j], " or ", b22[!b11 == b22][j],"\n", sep = ""))
        if(correct == "q")
          stop(paste0("You should start the program at discrepancy ", i, ";", j))
        else if (correct == 1){
          write(paste0("sameB[", i,",", j, "] <- b1[", i,",", j, "]  #", i,";",j), file = outputFile, append = T)
          g <- F
        }
        else if (correct == 2){
          write(paste0("sameB[", i,",", j, "] <- b2[", i,",", j, "]  #", i,";",j), file = outputFile, append = T)
          g <- F
        }
        else
          cat("you're not making sense try again\n")
      }
    }
  }
}

sameB <- b11
dfCompare(b11, b22, outputFile = "data/cleaning/cleaningLines.R")









