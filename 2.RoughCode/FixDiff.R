#--------------------------------------------
# CALCULATE ROLLING DIFFICULTIES ON FPL DATA
# ===========================================

# --------------------------------------------------------------------------
# Dataset must contain a column name "Team" in the first column of the data.
# Column names are thereafter the GAMEWEEKS
# ----

library(tidyverse)

FixDiff_Attack <- read_excel("1.Data/FixDiff2020.xlsx", sheet = "MID_FWD")
FixDiff_Defense <- read_excel("1.Data/FixDiff2020.xlsx", sheet = "GK_DEF")

# --------
# Set up gameweek information that you want to look at.
#---------

GW_start <- "GW1"
GW_length <- 5        # This is how many GWs on top of the starting week
# -------------------------------

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

# Test it out
# First 5 gameweeks attack and defense

Get_Team_Diff(FixDiff_Attack, "GW2", 3) %>% arrange(AveDiff)
Get_Team_Diff(FixDiff_Defense, "GW1", 4) %>% arrange(AveDiff)

# Sub out Tierney in GW3

Get_Team_Diff(FixDiff_Defense, "GW3", 3) %>% arrange(AveDiff)
# Maybe bring in someone from Man City?

# Sub out Aubameyang in GW3
Get_Team_Diff(FixDiff_Attack, "GW2", 6) %>% arrange(AveDiff)




Get_Team_Diff(FixDiff_Attack, "GW1", 5) %>% filter(Run_Diff == 'Easy')%>% arrange(AveDiff) %>% filter(Team != 'MUN')
Get_Team_Diff(FixDiff_Attack, "GW2", 9) %>% filter(Run_Diff == 'Easy') %>% arrange(AveDiff)
Get_Team_Diff(FixDiff_Attack, "GW2", 9) %>% arrange(AveDiff)

Get_Team_Diff(FixDiff_Defense, "GW1", 6) # %>% filter(Run_Diff == 'Easy')
Get_Team_Diff(FixDiff_Defense, "GW2", 9) %>% arrange(AveDiff)

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

gw_14_38 <- Get_Team_Diff(Fix_Diff, "GW14", 24) %>% order_by(AveDiff)

View(gw_14_38)

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

gw_14_17 <- Get_Team_Diff(Fix_Diff, "GW14", 3)
View(gw_14_17 %>% filter(Run_Diff == 'Easy'))

# Works well! Ok, now let's graph the stuff quickly:

qplot(x = Team, 
      y = AveDiff,
      data = gw_13_15,
      geom = "point",
      colour = Run_Diff)

# Let's make this look better. 

b <- ggplot(gw_13_15, aes(x = Team, y = AveDiff)) + geom_point(aes(color = Run_Diff)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) +
  scale_color_manual(values = c("#999999", "#E69F00")) + 
  theme_minimal() + 
  geom_hline(yintercept = 3.5, linetype="dashed", color = "black")

b






b + geom_point()

b + geom_point(aes(color = Run_Diff)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) +
  scale_color_manual(values = c("#999999", "#E69F00")) + 
  theme_minimal() + 
  geom_hline(yintercept = 3.5, linetype="dashed", color = "black")
  # geom_text(aes(label = Team))
  


##################################
# Looking at your curent Gameweek
##################################

GW_start <- "GW16"
GW_length <-  3 
#------------------

# Next 2 weeks

gw_18_19 <- Get_Team_Diff(Fix_Diff, GW_start, 1)

View(gw_18_19)
View(gw_14_15 %>% filter(Run_Diff == 'Easy'))

# 3 Weeks ahead

gw_18_20 <- Get_Team_Diff(Fix_Diff, GW_start, 2)

View(gw_18_20)
View(gw_14_16 %>% filter(Run_Diff == 'Easy'))

# 4 Weeks ahead

gw_18_21 <- Get_Team_Diff(Fix_Diff, GW_start, 3)

View(gw_18_21 %>% filter(Run_Diff == 'Easy'))

# 5 weeks ahead

gw_14_18 <- Get_Team_Diff(Fix_Diff, GW_start, 4)

View(gw_14_18 %>% filter(Run_Diff == 'Easy'))





    
  
