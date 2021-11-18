# this script contains usefull functions for cleaning data #

# set up ####

library(tidyverse)

# df compare function ####

dfCompare <- function(b11, b22, discrepancy = "1;1", outputFile){
  si <- str_extract(discrepancy, "^[[:digit:]]*(?=;)")
  sj <- str_extract(discrepancy, "(?<=;)[[:digit:]]*$")
  first <- T
  sheetlines <- lapply(1:max(b11$sheet), FUN = function(x){
    return(b11$line[b11$sheet %in% x])
    })
  for(i in si:nrow(b11)){
    for(j in which(!b11[i,] == b22[i,])){
      if(first & j < sj & i == si){
        next
      }
      if(first & j == sj & i == si){
        first <- F
      }
      g <- T
      while(g){
        correct <- readline(prompt = cat("sheet: ", b11$sheet[i], " line: ", i - min(sheetlines[[b11$sheet[i]]]) + 1, " col: '", colnames(b11)[j], "'\n", 
                                         b11[i,j], " or ", b22[i,j], "\n", sep = ""))
        if(correct == "q"){
          write(paste0("# You should restart the program at discrepancy ", i, ";", j), file = outputFile, append = T)
          stop(paste0("You should restart the program at discrepancy ", i, ";", j))
        }
        else if (correct == 1){
          write(paste0("sameB[", i,",", j, "] <- b1[", i,",", j, "]  # ", i,";",j, " ", b11[i,j], " not ", b22[i,j]), file = outputFile, append = T)
          g <- F
        }
        else if (correct == 2){
          write(paste0("sameB[", i,",", j, "] <- b2[", i,",", j, "]  # ", i,";",j, " ", b22[i,j], " not ", b11[i,j]), file = outputFile, append = T)
          g <- F
        }
        else
          cat("you're not making sense try again\n")
      }
    }
  }
}

# add to closet search function

problem <- function(catNumb, problemNote, 
                    outputFile = "data/cleaning/makeClosetSearch.R", 
                    fixFile = "data/cleaning/fixIssuesCaterpillar.R"){
  cat("\ntheseProbs[i,] <- cats %>%
      filter(catNum %in% '", catNumb, "') %>%
      dplyr::select(catNum, catSpecies, treeSpecies, fate, photograph) %>%
      mutate(issue = '", problemNote, "')
      i <- i + 1\n", append = T, sep = "", file = outputFile
  )
  cat("# ", catNumb, "\n\n", append =T, file = fixFile, sep = "")
}








