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



