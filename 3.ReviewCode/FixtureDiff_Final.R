# Fixtire Difficulties
#-----------------------

# This script serves as the main script
# for designing a function(s) that give 
# us a rolling fixture difficulty
# for the English Premier League

#=====================================================
# STEP 1: CREATE FUNCTION THAT PICKS OUR ROLL PERIOD
#=====================================================

# So lets create this function that takes the gameweek as an input and then
# subsets our dataframe by that amount of gameweeks

# Why? 

# When doing a rolling average, we want to subset our datafram by the required
# length, say GWs 10:17, or a smaller range of GWs 10:14. Thus we want
# this function to be flexible.

# Once we have this range we can begin to subset the data.

GW_Sub <- function(GW,GW_range){
  
  gameweek_number <- substr(GW,3,4)   # Take the number out of the gameweek
  gameweek_number <- as.numeric(gameweek_number)
  GW_range <- as.numeric(GW_range)
  gameweek_vector <- (gameweek_number:(gameweek_number + GW_range))

  # Now we have our vector, lets add the "GW" back to each
  # We will use lappy so it returns a list
  
  a <- vector()
  for (i in gameweek_vector) {
    a[i] <- paste("GW",i)
    a <-  a %>% na.omit(a)
  }
  
  new_range2 <- gsub(" ", "", a, fixed = TRUE)
  
}

# Test function

gw_filter <- GW_Sub("GW8", GW_range = 4)
View(gw_filter)

gw_filter <- GW_Sub("GW10", GW_range = 10)
View(gw_filter)

gw_filter <- GW_Sub("GW4", GW_range = 6)
View(gw_filter)


#=========================================================
# STEP 2: CREATE FUNCTION THAT CALCULATES TEAM DIFFICULTY
#=========================================================


Get_Team_Diff <- function(Data, GW_start, GW_length) {
  
  # First create function that can specify the gameweeks
  
  GW_Sub <- function(GW,GW_range){
    
    gameweek_number <- substr(GW,3,4)   # Take the number out of the gameweek
    gameweek_number <- as.numeric(gameweek_number)
    GW_range <- as.numeric(GW_range)
    gameweek_vector <- (gameweek_number:(gameweek_number + GW_range - 1))
    
    # Now we have our vector, lets add the "GW" back to each
    # We will use lappy so it returns a list
    
    a <- vector()
    for (i in gameweek_vector) {
      a[i] <- paste("GW",i)
      a <-  a %>% na.omit(a)
    }
    
    new_range2 <- gsub(" ", "", a, fixed = TRUE)
    
  }
  
  # Now we can focus on the data itself 
  
  # First filter the dataset by the set of gameweeks that you want.  
  # Choose gameweeks
  GW_Filter <- GW_Sub(GW_start, GW_range = GW_length)
  
  # Create new dataset
  new_df <- Data %>% select(Team, GW_Filter)
  
  # Sum the relevant gameweeks
  sum_df <-  new_df %>% mutate(SummedCol = select(., GW_start:names(rev(new_df)[1])) %>% 
                                 rowSums(na.rm = TRUE))
  
  # calculate the average difficulty
  
  ave_diff <- sum_df %>% mutate(AveDiff = SummedCol/(GW_length))
  
  # remove columns that won't be used
  final_set <- ave_diff %>% select(Team, AveDiff)
  
  final_set$Run_Diff <- ifelse(final_set$AveDiff > 8, "Tough", "Easy")
  
  # Return final set as output
  return(final_set)
}

# We can test it out again on a dataset.

# First load libraries and import the datasets
#---------------------------------------------

library(readr)
library(readxl)
library(tidyverse)

FixDiff_Attack <- read_excel("1.Data/FixDiff2020.xlsx", sheet = "MID_FWD")
FixDiff_Defense <- read_excel("1.Data/FixDiff2020.xlsx", sheet = "GK_DEF")

# First 5 gameweeks attack and defense

Get_Team_Diff(FixDiff_Attack, "GW2", 3) %>% arrange(AveDiff)
Get_Team_Diff(FixDiff_Defense, "GW1", 4) %>% arrange(AveDiff)
