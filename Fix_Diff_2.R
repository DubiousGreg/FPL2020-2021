#--------------------------------------------
# CALCULATE ROLLING DIFFICULTIES ON FPL DATA
# ===========================================

# --------------------------------------------------------------------------
# Dataset must contain a column name "Team" in the first column of the data.
# Column names are thereafter the GAMEWEEKS
# ----


Fix_Diff <- read.csv("1.Data/FixtureDifficultiesv_01.csv")

# --------
# Set up gameweek information that you want to look at.
#---------

GW_start <- "GW13"
GW_length <- 5        # This is how many GWs on top of the starting week
# -------------------------------

Get_Team_Diff <- function(Data, GW_start, GW_length) {

# First create function that can specify the gameweeks
  
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
  
# Now we can focus on the data itself 
  
# First filter the dataset by the set of gameweeks that you want.  
  # Choose gameweeks
  GW_Filter <- GW_Sub(GW_start, GW_range = GW_length)
  
  # Create new dataset
  new_df <- Fix_Diff %>% select(Team, GW_Filter)

  # Sum the relevant gameweeks
   sum_df <-  new_df %>% mutate(SummedCol = select(., GW_start:names(rev(new_df)[1])) %>% 
             rowSums(na.rm = TRUE))
  
   # calculate the average difficulty
  ave_diff <- sum_df %>% mutate(AveDiff = SummedCol/GW_length)
   
   # remove columns that won't be used
   final_set <- ave_diff %>% select(Team, AveDiff)
  
   final_set$Run_Diff <- ifelse(final_set$AveDiff >= 3.5, "Tough", "Easy")
   
   # Return final set as output
   return(final_set)
}

# Test it out

Get_Team_Diff(Fix_Diff, "GW13", 5)


View(Get_Team_Diff(Fix_Diff, "GW13", 5))

# Store in a variable

gw_13_18 <- Get_Team_Diff(Fix_Diff, "GW13", 5) 
View(gw_12_17)  

# Call the data and enter the team name as an input to the pipe function

gw_13_18 %>% filter(Team == "Arsenal")
  
###------------------------------------------------------------------------------

#=====================
# SCENARIO BUILDER
#=====================

# Let us see who has the best run in for the rest of the year:

gw_13_38 <- Get_Team_Diff(Fix_Diff, "GW13", 25) %>% order_by(AveDiff)

View(gw_13_38)

# And what about the best ru over the next 5 gameweeks:

gw_13_17 <- Get_Team_Diff(Fix_Diff, "GW13", 4) 
View(gw_13_17)

# This shows us that City have the worst run of fixtures whereas Leicester has the best
# run of fixtures.

# Can we add another column to give an indication that the team has an easy run or not?

# Let's define "easy" as anyaverage score below 3.5 and any score higher than 3.5 will
# be defined as a difficult run.

gw_13_17

gw_13_17$Run_Diff <- ifelse(gw_13_17$AveDiff >= 3.5, "Tough", "Easy")
View(gw_13_17)

# Now we will go back to the function and add this transformation to the function.
# Run the function again on a new dataset.

gw_13_15 <- Get_Team_Diff(Fix_Diff, "GW13", 2)
View(gw_13_15)


gw_13_16 <- Get_Team_Diff(Fix_Diff, "GW13", 3)
View(gw_13_16 %>% filter(Run_Diff == "Easy"))

View(Fix_Diff)







    
  
