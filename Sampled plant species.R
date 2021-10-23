
library(data.table)
library(tidyverse)
library(writexl)

setwd("C:/Users/Fever/OneDrive/Desktop/R files/Cat data/tritrophicInvasives-main/tritrophicInvasives/data/dirty/branch")
temp = list.files(pattern="*.csv")

myfiles = lapply(temp, read.csv)


com <- rbindlist(myfiles, use.names= TRUE, fill = TRUE)

uniquespec <- com %>%
  select(treeSpecies)%>%
  distinct()

setwd("C:\\Users\\Fever\\OneDrive\\Desktop\\UCONN\\fall 2021\\Caterpillar paper")
write_xlsx(uniquespec, "sampled.xlsx" )
