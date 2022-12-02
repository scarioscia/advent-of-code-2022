## Rock paper scissors
# Score for each round is based on what you choose (1 for Rock, 2 for Paper, and 3 for Scissors) 
# plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won)


# load libraries
library(data.table)

# read in data 
input <- fread("/Users/saracarioscia/grad-school/2022_advent_of_code/data/day2.txt", header = FALSE)
colnames(input) <- c("opponent", "you")


## PART 1: Your score based on the strategy guide

# first col is your opponent, second col is you. 
# opponent: A for Rock, B for Paper, and C for Scissors
# you: X for Rock, Y for Paper, and Z for Scissors


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


## PART 2: Score under "correct" strategy guide

# X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.

new_guide <- input
new_guide$outcome <- new_guide$you
new_guide$opponent_outcome <- paste0(new_guide$opponent, new_guide$outcome)

# if outcome is a draw (Y), you need to do the same as your opponent 
new_guide[new_guide$opponent_outcome == "AY"]$you <- "A"
new_guide[new_guide$opponent_outcome == "BY"]$you <- "B"
new_guide[new_guide$opponent_outcome == "CY"]$you <- "C"

# if outcome is you lose, need to lost to your opponent 
new_guide[new_guide$opponent_outcome == "AX"]$you <- "C" #your opponent did Rock (A), you need to lose (X), so you have to do scissor (C)
new_guide[new_guide$opponent_outcome == "BX"]$you <- "A" 
new_guide[new_guide$opponent_outcome == "CX"]$you <- "B" 

# if outcome is you win, need to beat your opponent
new_guide[new_guide$opponent_outcome == "AZ"]$you <- "B" #your opponent did Rock (A), you need to win (Z), so you have to do paper (B)
new_guide[new_guide$opponent_outcome == "BZ"]$you <- "C" 
new_guide[new_guide$opponent_outcome == "CZ"]$you <- "A" 

# calculate your score based on shape 
new_guide$shape_score <- new_guide$you
new_guide[new_guide$shape_score == "A"]$shape_score <- 1
new_guide[new_guide$shape_score == "B"]$shape_score <- 2
new_guide[new_guide$shape_score == "C"]$shape_score <- 3
# get score from just the shapes 
new_guide$shape_score <- new_guide$shape_score %>% as.numeric()
new_shape_score <- sum(new_guide$shape_score)

# calculate your score based on win, lose, or draw 
new_guide$win_score <- new_guide$outcome
new_guide[new_guide$outcome == "X"]$win_score <- 0
new_guide[new_guide$outcome == "Y"]$win_score <- 3
new_guide[new_guide$outcome == "Z"]$win_score <- 6
# get score from the whether you win 
new_guide$outcome <- new_guide$win_score %>% as.numeric()
new_outcome_score <- sum(new_guide$outcome)

# get score from shape and outcome 
new_guide_overall_score <- new_shape_score + new_outcome_score #10398


