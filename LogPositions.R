#=========================
# Log Position Over Time
#=========================

library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)

# Current Log

GW8 <- c("LIV", "MCI", "ARS", "LEI", "CHE", "CRY", "BUR", "WHU", "TOT",
        "BOU", "WOL", "MUN", "SHU", "BHA", "AVL", "NEW", "SOU", "EVE",
        "NOR", "WAT")

GW1 <- c("MCI", "MUN", "LIV", "BHA", "BUR", "TOT", "ARS", "BOU", "SHU", "CRY",
         "EVE", "LEI", "WOL", "NEW", "AVL", "NOR", "SOU", "WAT", "CHE", "WHU")

GW2 <- c("LIV", "ARS", "MCI", "MUN", "BHA", "TOT", "BOU", "SHU","EVE", "BUR",
         "NOR", "LEI", "WOL", "CRY", "CHE", "WHU", "AVL", "NEW", "SOU", "WAT")

View(GW)

GW_ts <- as.data.frame(cbind(GW_num, GW1, GW2))
class(GW_ts)
View(GW_ts[3])


# Want to plot the league position over time.

GW_ts[1]        # 1st column
GW_ts[2]         # 2nd column
View(GW_ts[,3]) # The third column

### Lets try this different type of log

LogPositions <- read_excel("1.Data/LogPositions.xlsx")

LogPositions <- read_excel("C:/Users/AngusMacDonald/OneDrive - MONOCLE SOLUTIONS (PTY) LTD/Desktop/FPL/1.Data/LogPositions.xlsx")

head(LogPositions)
LogPositions[1:5,1:5]

dates <- as.Date(c("2019-08-10", "2019-08-17", "2019-08-24", "2019-08-31", "2019-09-07", "2019-09-14", "2019-09-21", "2019-09-28", "2019-10-06", "2019-10-13"))

LogP_test <- rbind(dates, LogPositions)

plot(LogP_test[1,], LogP_test[2,])

View(LogP_test)
# Transpose the matrix
#-----------------------

t_LogPositions <- t(LogPositions)

class(t_LogPositions)
View(t_LogPositions)

colnames(t_LogPositions) <- t_LogPositions[1,]

t_LogPositions <- t_LogPositions[-1,]
View(t_LogPositions)

GW <- c(1:8)

x <- cbind(GW, t_LogPositions)
View(x)

class(x)

x <- as.data.frame(x)

qplot(x$GW, x$ARS)
# Selecting a specific team
#-----------------------------




# Add GW

View(LogPositions)

LogPositions$GW <-  c(1:8)
GW <- c(1:8)

x <- cbind(GW, LogPositions)

