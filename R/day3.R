# load packages
library(data.table)
library(vecsets)
library(tidyverse)


# read in data 
rucksacks <- fread("/Users/saracarioscia/grad-school/2022_advent_of_code/data/day3.txt", header = FALSE)

## PART ONE

## Rucksack challenge: find letters that are present in both compartments in the rucksack. 
## Convert these to numbers. 
## Sum the numbers attributed to the items that are duplicated 


# make empty list for the compartments of each rucksack
comp_1 <- list()
comp_2 <- list()

# for each rucksack, get the packages in each
for (i in 1:nrow(rucksacks)) {
  packages <- rucksacks$V1[[i]]
  # define the length of character chunks (Count the number of packages that should be in each compartment (half of each rucksack))
  n <- nchar(packages) / 2
  # define the first letter to go in the second rucksack
  m <- n + 1
  # use that number to separate the packages into two substrings 
  compartment_1 <- substring(packages, 1, n)
  compartment_2 <- substring(packages, m, nchar(packages))
  # make list of each 
  comp_1 <- append(comp_1, compartment_1)
  comp_2 <- append(comp_2, compartment_2)
}

# make lists into data tables
compartments <- cbind(comp_1, comp_2) %>% as.data.table()

# make empty list for letters present in two comparisons
duplicates <- list()
# for each row of 
for (i in 1:length(comp_1)) {
  dup <- vintersect(strsplit(as.character(comp_1[i]), "")[[1]], strsplit(as.character(comp_2[i]), "")[[1]])
  # append to list
  # if the same item is in one compartment more than once (i.e., "s" is in the rucksack 3 times), it causes a problem... we want just the item, so keep only the first thing of dup 
  duplicates <- append(duplicates, dup[1])
}

# save duplicates as data table and switch columns to rows 
duplicates <- as.data.frame(duplicates)
duplicates <- as.data.table(t(duplicates))

# lowercase letters have priority 1 through 26
low_convert <- setNames(seq_along(letters), letters)
lowercase <- low_convert[duplicates$V1]
# uppercase letters have priority 27 through 52
cap_convert <- setNames(seq_along(LETTERS), LETTERS)
capitals <- cap_convert[duplicates$V1]
# add 26 to each entry in capitals 
capitals <- capitals + 26

# get sum from all lowercase
lowercase_sum <- sum(lowercase, na.rm = TRUE)
# get sum from all capitals
capital_sum <- sum(capitals, na.rm = TRUE)
# get total sum 
total_sum <- sum(lowercase_sum, capital_sum) # 7821


## PART TWO 
## badge is the only item type (letter) carried by all three elves. find out the only item that is common between each group of three elves 
## each group is three lines (so first 3 lines is group 1, second 3 lines is group 2, etc.)
## find the letter for each group, then convert to numeric (same process as above), then report that sum 

# make empty list for badges 
badges <- list()


# set initial row 
i <- 1
# continue until end of rucksacks is reached 
while (i < nrow(rucksacks)) {
  # use iterator to grab row of rucksack for each elf 
  elf1_row <- i
  elf2_row <- i + 1
  elf3_row <- i + 2
  elf1 <- rucksacks$V1[[elf1_row]]
  elf2 <- rucksacks$V1[[elf2_row]]
  elf3 <- rucksacks$V1[[elf3_row]]
  # compare first two elves (vintersect syntax takes two)
  first_two <- vintersect(strsplit(as.character(elf1), "")[[1]], strsplit(as.character(elf2), "")[[1]])
  # compare first two with the third elf (no need for strsplit b/c of output from vintersect)
  badge <- vintersect(first_two, strsplit(as.character(elf3), "")[[1]]) 
  # take just the first match since some will be duplicated if the letter appeared twice in all three elves
  badges <- append(badges, badge[1])
  # move to the next group of elves
  i <- i + 3
}


# save badges as data table and switch columns to rows 
badges <- as.data.frame(badges)
badges <- as.data.table(t(badges))

# convert lower and uppercase to priority numbers as above 
lowercase_badge <- low_convert[badges$V1]
capital_badge <- cap_convert[badges$V1]
# add 26 to the capitals 
capital_badge <- capital_badge + 26

# sum lower, upper, and total badges
low_badge_sum <- sum(lowercase_badge, na.rm = TRUE)
cap_badge_sum <- sum(capital_badge, na.rm = TRUE)
total_badge_sum <- sum(low_badge_sum, cap_badge_sum) #2752
