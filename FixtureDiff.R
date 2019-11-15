# Fixtire Difficulties
#-----------------------

# install.packages() <- c("readr", "tidyverse")

library(readr)
library(tidyverse)


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

# Once we have this range we can move forward.

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

# Our function works perfectly.

# Import our fixture difficulty list

Fix_Diff <- read.csv("1.Data/FixtureDifficultiesv_01.csv")
View(FixtureDifficultiesv_01)


# So now the goal is to take the curent data set and apply a function to it that 
# does the following:

# 1) Takes the dataset and filters by the gameweeks we are looking at.
# 2) Calculates the sum of the records in for each team
# 3) ABle to vary this calculation depending on the dataset

# Inputs required

Rolling_Averages <- function(Data, GW_start, GW_length, team) {
  
  df <- Data
  GW_Filter <- GW_Sub(GW_start, GW_range = GW_length)
  
  teams <- df[,1]
  sub_df <- df %>% select(df[,1], GW_Filter)
  
}

df <- Fix_Diff
View(df)

#--------------------------------
# Set inputs you want to use 

GW_start <- "GW12"
GW_length <- 6
team = 'Arsenal'
# -------------------------------

GW_Filter <- GW_Sub(GW_start, GW_range = GW_length)
View(GW_Filter)


# Create a data set to test on Arsenal 

ArsData <- df %>% select(Team, GW_Filter) %>% filter(Team == team)
View(ArsData)


x <- ArsData %>% select(Team, GW_Filter)  %>% 
  #filter(Team == team) %>% 
  mutate(SummedCol = select(., GW_start:names(rev(ArsData)[1])) %>% 
           rowSums(na.rm = TRUE))  

View(x) 

z <- x %>% mutate(AveDiff = SummedCol/GW_length)

View(z)               

y <- z %>% select(-SummedCol) 
View(y) 

#------------------------------

GW_start <- "GW12"
GW_length <- 6
team = 'Arsenal'

Fix_Diff %>% select(Team, GW_Filter)  %>% 
  filter(Team == team) %>% 
  mutate(SummedCol = select(., GW_start:names(rev(Fix_Diff)[1])) %>% 
           rowSums(na.rm = TRUE))





# Test it
#----------
new <- Rolling_Averages(Fix_Diff, "GW12")


# ===================
### OTHER ROUGH WORK
# ===================

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







