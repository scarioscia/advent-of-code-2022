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
# character after the start-of-message marker - 14 consecutive distinct characters (first instance of this) 

# try just length unique 
## start at the first character and loop until marker is found
## but instead of checking every string, we just need the first one... (checking all was too long). Now, stop if it gets to a sequence of 14 unique characters 
# to use length(unique), need the input to be a string subsettable by index 
characters_2 <- unlist(strsplit(datastream$V1, ""))

# could also make this a function and have the length of sequence (4 or 14) be a passed argument 
i <- 0
answer <- 0
while(i >= answer){
  if(length(unique(characters_2[i:(i+13)]))==14){
    answer <- i + 13
  }
  i <- i + 1 
}

answer # 3217