## How many trees are visible from the outside? 
## The outside of the grid (exterior rows and columns) are automatically visible. Interior trees are visible if they are not shielded by a taller tree 

# libraries
library(data.table)
library(tidyverse)

# read in data 
input <- fread("/Users/saracarioscia/grad-school/2022_advent_of_code/data/day8.txt", header = FALSE)

## PART ONE 
## How many trees are visible from the outside of the grid? 

# count how many characters per line so I can split input data into that many columns 
width = length(input$V1) 
# get one column for each character in each row; save as dataframe to allow indexing with [] subset of numeric row/column 
tree_grid <- input %>% separate(V1, into = paste0('col', 1:width), sep = 1:(width - 1)) %>% as.data.frame()

# get count of trees that are visible - definitely the entire exterior 
# all trees in the first and last rows: 2*ncol(tree_grid). all trees in the first and last columns, except the top and bottom one: 2*(nrow(tree_grid)-2)
visible_trees <- 2 * ncol(tree_grid) + 2 * (nrow(tree_grid)-2)



# check from each direction if the tree is visible 
# can ignore the exterior since we've already counted those in `visible_trees` and they'll be added at the end to the total tally 
# this loop makes the assumption that the tree is visible until proven otherwise (got this idea from the Reddit thread); I tried it a few ways with the assumption that 
# it was NOT visible until proven otherwise, but that never worked and I couldn't figure out why. So I inverted it and it worked here [shrug]

# from the second row until the last row 
for (i in 2:(nrow(tree_grid)-1)) {
  # from the second col until the last col
  for (j in 2:(ncol(tree_grid)-1)) {
    # get tree height 
    tree_height <- tree_grid[i,j]
    # assume it is visible
    visible <- TRUE 
    
    # check if it's visible from above
    # starting from the first row until the row above my tree (i.e., top of the grid down) 
    for (t in 1:(i-1)) {
      # if any tree in those rows (but the same column) is taller, my tree is hidden 
      if (tree_grid[t,j] >= tree_height) {
        visible <- FALSE
        # end this as soon as my tree is hidden 
        break
      }
    }
    # if it was still hidden from above, check if it's visible from below... look from the row below it until the end of the grid 
    if (visible == FALSE) {
      # assume visible 
      visible <- TRUE
      # check if any below it are taller and can hide it 
      for (t in (i+1):nrow(tree_grid)) {
        if (tree_grid[t,j] >= tree_height) {
          visible <- FALSE
          # if one hides it, it's not visible and we're done with this direction
          break
        }
      }
    }
    # if it was still hidden from above and below, we check left
    if (visible == FALSE) {
      visible <- TRUE
      # start from the start of the grid until you get to the column right before my tree 
      # (maybe I should also try to invert this and go from the one directly left until the first col, though it doesn't matter for the answer; just more logical sense)
      for (t in 1:(j-1)) {
        # if any of those to the left are taller and end this as soon as we get one 
        if (tree_grid[i,t] >= tree_height) {
          visible <- FALSE
          break
        }
      }
    }
    # if it's still hidden after all that, check the column to the right until the end 
    if (visible == FALSE) {
      visible <- TRUE
      # for columns to the right of my tree, until the end of the grid 
      for (t in (j+1):ncol(tree_grid)) {
        # if any of those to the right are taller and end this as soon as we get one 
        if (tree_grid[i,t] >= tree_height) {
          visible <- FALSE
          break
        }
      }
    }
    # if at this point the tree is visible, increase the count of visible trees 
    if (visible == TRUE) {
      visible_trees <- visible_trees + 1
    }
  }
} # 1693


## PART TWO 
## calculate view score - count number of trees in each direction that you can see from the given tree 

# set initial best view score
high_view_score <- 0
# set initial score for current tree
view_score <- 0
for (i in 1:nrow(tree_grid)) {
  for (j in 1:ncol(tree_grid)) {
    # get height of this tree 
    tree_height <- tree_grid[i,j]
    # initialize scores for this tree 
    sc1 <- 0 
    sc2 <- 0
    sc3 <- 0
    sc4 <- 0
    
    # first row sees no trees 
    if (i == 1) {
      sc1 <- sc1 + 0
    } else {
      # from the row above to the top row, count how many trees it can see 
      for (t in (i-1):1) {
        # how many trees can you see? 
        sc1 <- sc1 + 1
        # if you get to one taller, end this
        if (tree_grid[t,j] >= tree_height) {
          break
        }
      }
    }
    # first col sees no trees
    if (j == 1) {
      sc2 <- sc2 + 0
    } else {
      # from the col to the left until the first col, count how many trees it can see
      for (t in (j-1):1) {
        sc2 <- sc2 + 1
        # if you get to one taller, end this 
        if (tree_grid[i,t] >= tree_height) {
          break
        }
      }
    }
    # last row sees no trees 
    if (i == nrow(tree_grid)) {
      sc3 <- sc3 + 0
    } else {
      # from the row below until the bottom of the grid, count how many trees it can see 
      for (t in (i+1):nrow(tree_grid)) {
        sc3 <- sc3 + 1
        # if you get to one taller, end this 
        if (tree_grid[t,j] >= tree_height) {
          break
        }
      }
    }
    # last col sees no trees
    if (j == ncol(tree_grid)) {
      sc4 <- sc4 + 0
    } else {
      # from the col to the right until the far end of the grid, count how many trees it can see
      for (t in (j+1):ncol(tree_grid)) {
        sc4 <- sc4 + 1
        # if you get to one taller, end hti s
        if (tree_grid[i,t] >= tree_height) {
          break
        }
      }
    }
    # calculate tree score 
    view_score <- sc1 * sc2 * sc3 * sc4
    if (view_score > high_view_score) {
      high_view_score <- view_score
    }
  }
} # 422059


