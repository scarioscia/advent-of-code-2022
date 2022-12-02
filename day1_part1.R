## Which elf is carrying the most Calories? How many calories is that elf carrying? 

library(data.table)
library(dplyr)

input <- fread("/Users/saracarioscia/grad-school/2022_advent_of_code/day1_input.txt")

## PART 1

# get second column to form user IDs
input$V2 <- input$V1

# make ID string for each length of NA 
num_NA <- 0
for (i in 1:nrow(input)) {
  if (!is.na(input[i,1])) {
    input[i,2] <- num_NA
  } else if (is.na(input[i,1])) {
    num_NA <- num_NA +1
  }
}

# group to sum by second column 
cal_per_elf <- input %>%
  group_by(V2) %>% 
  transmute(Total=sum(V1))

# find which value of calories per elf is the highest 
max(cal_per_elf, na.rm = TRUE) # 69310

# find which elf had that max
cal_per_elf[which(cal_per_elf$Total == max(cal_per_elf, na.rm = TRUE)),]
# elf 177 has 69310 calories


# PART 2

# find the top three elves and how many calories the three of them have

# sort order decreasing
cal_per_elf_sorted <- cal_per_elf[order(cal_per_elf$Total, decreasing = TRUE), ] 

# get only unique values 
cal_per_elf_sorted_unique <- unique(cal_per_elf_sorted)

# pick the top 3 
cal_per_elf_sorted_unique_3 <- head(cal_per_elf_sorted_unique,3)$Total

# sum those top three elves 
total_calories <- sum(cal_per_elf_sorted_unique_3) #206104



