## 

# load packages
library(tidyverse)
library(data.table)

# read in data 
datastream <- fread("/Users/saracarioscia/grad-school/2022_advent_of_code/data/day6.txt", header = FALSE)

# find index of the 4th consecutive unique character 
# check for pattern of four unique characters. If not unique, move to the next start location; if all four unique, save the index of the 4th letter 
# substring(data, start character, end character)

# get a separate substring for each letter
char_list <- list()
for (i in 1:nchar(datastream$V1[1])) {
  char <- substring(datastream$V1[1], i, i)
  char_list <- append(char_list, char)
}
characters <- unlist(char_list) %>% as.data.table()

# check if any letters are the same in each group of four
matches <- 0
answer <- 0
i <- 0
while (i >= answer) {
  i <- i + 1
  # does the first match the next 3? second match the next 2? third match the 4th? 
  if (characters$.[i] == characters$.[i+1] | characters$.[i] == characters$.[i+2] | characters$.[i] == characters$.[i+3]) {
    matches <- matches + 1
  } else if (characters$.[i+1] == characters$.[i+2] | characters$.[i+1] == characters$.[i+3]) {
    matches <- matches + 1
  } else if (characters$.[i+2] == characters$.[i+3]) {
    matches <- matches + 1
  } else {
    answer <- i + 3
  }
}
answer # 1175

## PART TWO 
# character after the start-of-message marker - 14 consecutive distinct characters 

# can use stringr::str_count to find the number of each letter that is present in my test string
# so `str_count(test, letters)` returns 26 values. The number output shows the number of instances of that letter in test
# for it to have 14 unique letters in a 14-letter string, the output of `length(which(str_count(test, letters) == 0))` needs to be 12 
