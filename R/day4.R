## day 4

## load libraries
library(data.table)
library(tidyverse)

## read in data 
pairings <- fread("/Users/saracarioscia/grad-school/2022_advent_of_code/data/day4.txt", header = FALSE)


# PART ONE 
## for how many pairings is the range of one fully contained in the other? 


enclosed <- 0
for (i in 1:nrow(pairings)) {
  # first elf 
  elf1 <- unlist(strsplit(pairings$V1[i], "-")) %>% as.numeric()
  # second elf 
  elf2 <- unlist(strsplit(pairings$V2[i], "-")) %>% as.numeric()

  # check for a perfect match 
  if (elf1[1] == elf2[1] && elf1[2] == elf2[2]) {
    enclosed <- enclosed + 1
  } else if (elf1[1] <= elf2[1] && elf1[2] >= elf2[2]) {
    enclosed <- enclosed + 1
  } else if (elf2[1] <= elf1[1] && elf2[2] >= elf1[2]) {
    enclosed <- enclosed + 1
  }
}
enclosed # 536


# PART TWO 
## how many pairs overlap at all (even if not completely)? Aka, how many have no overlap 
partial <- 0
for (i in 1:nrow(pairings)) {
  elf1 <- unlist(strsplit(pairings$V1[i], "-")) %>% as.numeric()
  elf2 <- unlist(strsplit(pairings$V2[i], "-")) %>% as.numeric()
  
  if (elf1[1] >= elf2[1] && elf1[1] <= elf2[2]) {
    partial <- partial + 1
  } else if (elf2[1] >= elf1[1] && elf2[1] <= elf1[2]) {
    partial <- partial + 1
  } else if (elf1[2] <= elf2[2] && elf1[2] >= elf2[1]) {
    partial <- partial + 1
  } else if (elf2[2] <= elf1[2] && elf2[2] >= elf1[1]) {
    partial <- partial + 1
  }
}
partial # 845