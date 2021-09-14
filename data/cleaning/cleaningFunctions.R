# this script contains usefull functions for cleaning data #

# set up ####

library(tidyverse)

# df compare function ####

dfCompare <- function(b11, b22, discrepancy = "1;1", outputFile){
  si <- str_extract(discrepancy, "^[[:digit:]]*(?=;)")
  sj <- str_extract(discrepancy, "(?<=;)[[:digit:]]*$")
  first <- T
  sheetlines <- lapply(1:max(b1$sheet), FUN = function(x){
    return(b1$line[b1$sheet %in% x])
    })
  for(i in si:nrow(b11)){
    for(j in which(!b11[i,] == b22[i,])){
      if(first & j < sj & i == si){
        break()
      }
      else if(first & j == sj & i == si){
        first <- F
      }
      g <- T
      while(g){
        cat("sheet ", b1$sheet[i], )
        correct <- readline(prompt = cat(b11[i][!b11[i] == b22[i]][j], " or ", b22[i][!b11[i] == b22[i]][j],"\n", sep = ""))
        if(correct == "q"){
          write(paste0("You should restart the program at discrepancy ", i, ";", j), file = outputFile, append = T)
          stop(paste0("You should restart the program at discrepancy ", i, ";", j))
        }
        else if (correct == 1){
          write(paste0("sameB[", i,",", j, "] <- b1[", i,",", j, "]  #", i,";",j, b11[i][!b11[i] == b22[i]][j], " or ", b22[i][!b11[i] == b22[i]][j]), file = outputFile, append = T)
          g <- F
        }
        else if (correct == 2){
          write(paste0("sameB[", i,",", j, "] <- b2[", i,",", j, "]  #", i,";",j, b11[i][!b11[i] == b22[i]][j], " or ", b22[i][!b11[i] == b22[i]][j]), file = outputFile, append = T)
          g <- F
        }
        else
          cat("you're not making sense try again\n")
      }
    }
  }
}

dfCompare(b1, b2, discrepancy = "6;4" ,outputFile = "data/cleaning/testing.R")








