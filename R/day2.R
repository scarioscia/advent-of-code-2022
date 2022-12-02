## Rock paper scissors
# Score for each round is based on what you choose (1 for Rock, 2 for Paper, and 3 for Scissors) 
# plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won)

# first col is your opponent, second col is you. 
# opponent: A for Rock, B for Paper, and C for Scissors
# you: X for Rock, Y for Paper, and Z for Scissors

# load libraries
library(data.table)

# read in data 
input <- fread("/Users/saracarioscia/grad-school/2022_advent_of_code/data/day2.txt", header = FALSE)
colnames(input) <- c("opponent", "you")


## PART 1: Your score based on the strategy guide

# calculate your scores based on shape 
scores <- input 
scores$shape <- scores$you

# one point for rock (X), two for paper (Y), three for scissors (Z)
scores[scores$shape == "X"]$shape <- 1
scores[scores$shape == "Y"]$shape <- 2
scores[scores$shape == "Z"]$shape <- 3
# convert shape col to numeric 
scores$shape <- scores$shape %>% as.numeric()
scores_points <- sum(scores$shape)

# calculate your points based on winning or losing 
scores$combined <- paste0(scores$opponent, scores$you)
# you win if it's AY, BZ, or CX
# you draw if it's AX, BY, CZ
# you lose if it's AZ, BX, CY
# 6 for win, 3 for draw, 0 for draw 
# win
scores[(scores$combined == "AY") | (scores$combined == "BZ") | (scores$combined == "CX")]$combined <- 6
# draw
scores[(scores$combined == "AX") | (scores$combined == "BY") | (scores$combined == "CZ")]$combined <- 3
# lose
scores[(scores$combined == "AZ") | (scores$combined == "BX") | (scores$combined == "CY")]$combined <- 0
# convert combined col to numeric 
scores$combined <- scores$combined %>% as.numeric()
scores_combined <- sum(scores$combined)

# total score is your shapes plus whether you beat your opponent
total_scores <- scores_points + scores_combined #13009


## PART 2: 
