# Fixtire Difficulties
#-----------------------

DiffCalc <- function(team,gameweek,lengthofroll,...){
  
  # Assign variables
  assignedTeam <- team
  Cur_GW <- gameweek
  roll <- lengthofroll
  
  assignedTeam[]
  
  print
}

# Import Fixture Difficulties dataset

library(readxl)
library(tidyverse)

FixtureDifficulties <- read_excel("FixtureDifficulties.xlsx")
View(FixtureDifficulties)

# Lets make sure we have the next few gameweeks
# If the next gameweek is GW9, then we want GW9 - GW(9+5)


#=====================================================
# STEP 1: CREATE FUNCTION THAT PICKS OUR ROLL PERIOD
#=====================================================

# So lets create this function that takes the gameweek as an input and then
# subsets our dataframe by that amount of gameweeks

# Why? 

# When doing a rolling average, we want to subset our datafram by the required
# length, say GWs 10:17, or a smaller range of GWs 10:14. Thus we want
# this function to be flexible.

# Once we have this range we can move forward.

GW_Sub <- function(GW,GW_range){
  
  gameweek_number <- substr(GW,3,3)   # Take the number out of the gameweek
  gameweek_number <- as.numeric(gameweek_number)
  GW_range <- as.numeric(GW_range)
  a <- vector()           # We are creating this to store the final
                                  # string we want
  
  gameweek_vector <- (gameweek_number:(gameweek_number + GW_range))
  
  
  # Now we have our vector, lets add the "GW" back to each
  # We will use lappy so it returns a list
  
  a <- vector()
  for (i in gameweek_vector) {
    a[i] <- paste("GW",i)
    a <-  a %>% na.omit(a)
  }
  
new_range <- gsub(" ", "", a, fixed = TRUE)

}


# Test function

gw_filter <- GW_Sub("GW10", GW_range = 6)


new_range2 <- gsub(" ", "", new_range, fixed = TRUE)

# Test Area
#---------------
#---------------

# STEP 1: DEFINE YOUR INPUT VARIABLES

GW <- "GW9"
GW_range <- 5

# Transform variables into desired class

gameweek_number <- substr(GW,3,3)   # Take the number out of the gameweek
gameweek_number <- as.numeric(gameweek_number)

# STEP 2: Define final variable for your function to populate based 
#         on your starting position and how many weeks you want to project 
#         forward

a <- gameweek_number:(gameweek_number + GW_range)

# Assign gameweek as vector and turn to list

gameweek_number2 <- gameweek_number  + 1
gameweek_number3 <- gameweek_number2 + 1
gameweek_number4 <- gameweek_number3 + 1
gameweek_number5 <- gameweek_number4 + 1

gameweek_vector <- c(gameweek_number 
                     ,gameweek_number2
                     ,gameweek_number3
                     ,gameweek_number4
                     ,gameweek_number5)    # Vector of numeric values

View(gameweek_vector)                     # Look at results

a <- vector()

for (i in gameweek_vector) {
  a[i] <- paste("GW",i)
  a <-  a %>% na.omit(a)
}

GW <- "GW8"
GW_range <- 4

gameweek_number <- substr(GW,3,3)   # Take the number out of the gameweek
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

#=====================================================
# STEP 2: Create function, that creates an additional column
#         that calculates the rolling average
#=====================================================

FixtureDifficulties <- read_excel("FixtureDifficulties.xlsx")
View(FixtureDifficulties)

x <- FixtureDifficulties %>% select(c(Team,gw_filter))
View(x)


rm(list=ls())







