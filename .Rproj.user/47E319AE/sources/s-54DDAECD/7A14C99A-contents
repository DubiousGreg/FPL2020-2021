

library(readxl)
FantasyPremierLeague <- read_excel("FantasyPremierLeague.xlsx", sheet = 'xG')
View(FantasyPremierLeague)

# Define variables

shots <- FantasyPremierLeague$Shots
shot_on_target <- FantasyPremierLeague$Shots_On_Target
xg_raw <- FantasyPremierLeague$xG_Raw
goals <- FantasyPremierLeague$Goals_Scored


test <- lm(goals ~ shots + shot_on_target + xg_raw, FantasyPremierLeague, method = "poisson")

summary(test)


