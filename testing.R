

# Lab ----
#
# TODO: Develop combined method - predict points for first gameweek using odds
# Proceeding gameweeks using index method (odds not available).
# Combine both methods - select players for team based on average points scored between the two

# TODO: Experiment with strength index - maybe find a way for influence to be stronger for higher scoring players
# This would give a better indication of whom to captain.
# Include days to next match in the metric
#
########################################

# TODO:
# tidymodels package
# play around with random forests




#######################################

# Start ----

devtools::document()
#load_all()
#check()
source("packages.R")



##############################################################

# Odds data ----

# Setup
# Goals, assists and clean sheets
# Save latest data - Update these lines as appropriate

# https://www.fantasyfootballpundit.com/premier-league-goalscorer-assist-odds/

url <- "https://www.fantasyfootballreports.com/premier-league-clean-sheet-odds/"
xpath <- '//*[@id="tablepress-166"]'

odds_gs_gw21 <- read.csv("FFP Points Predictor gw21.csv")
odds_cs_gw21 <- fpl_odds_generator_cs()
use_data(odds_gs_gw21, overwrite = TRUE)
use_data(odds_cs_gw21, overwrite = TRUE)


#############################################################

# fpl_gameweek_predictions serves as a master function.
# Concerned about the duplicated names. Might try to join on player id in future. Done

# Initiate
# Use function from the github package "fplscrapR" to get up-to-date player details
player_details <- fplscrapR::get_player_details()


# Extract current gameweek
gameweek <- fpl_get_gameweek_next()$id

# sample table for difficulty - Not mission critical
# Visualisation
fplBuddy::fpl_fixtures_difficulty_rating(gameweek, gameweek)

# Find one week at a time.
# Next - develop to have multiple weeks as we had before - need to focus on index metric not odds to do this though.

# Test - Different from before because of id matching
rm(list = Filter(exists, c("odds_gs", "odds_cs")))
test <- fpl_gameweek_predictions(players = player_details, gameweek = 21)
# Bug here related to while loop
# Will not revert to existing data if current gameweek odds are missing


rm(list = Filter(exists, c("odds_gs", "odds_cs")))
fpl_transfer_replacements(player_name = "Bukayo Saka", gameweek = 22)


# Export finalised predictions each week.
write.csv(test, "data/Previous week predictions/gw21_players_xP.csv")




# / write a ReadMe
# TODO: Refine index method - currently not really accounting for injuries
# Possibly train on this year data as well as last years.
# Figure out what to do with master function when predicting more than one week at a time.
# I feel like the odds change midweek...
# Also, minutes adjustment not working for players like ISAK



# Matching by ID means there will be a disparity for predictions collected in previous weeks
# Why? Matching on Index now -
# More players means KDB minutes standardisation weighs heavier in his favour for example

##################################################

#### Optimisation ----

# Dream Team - unlimited budget
sol <- fpl_optimise(test, test$xP_index, budget = 1000)

# User specific
# user unique id
user <- "6238967"

# Budget
budget <- fpl_my_budget(key = "ja11g14@soton.ac.uk",
                        secret = "vonfoj-tyhby9-vyrrUf",
                        user = user)


# Find my team here...
sol <- fpl_optimise_my_team(input = test, obj_var = test$xP_index,
                            budget = 105, transfers = 2)


# TODO: Write a function to offer list of 3 transfer replacement suggestions -
# Include predicted points for next 3 matches (index method)
# TODO: tidymodels package - regression tree experimentation


# My Team
# Tell me which players should be subbed out
for (i in unlist(fpl_my_team(user, squad = 11, gw = 12))) {
  if (!(i %in% sol$name)) {
    print(i)
  }
}

# Scope to combine both methods into one here..............
# Fixture / strength index still not influential enough for predicting hauls for me.
# Refine how predictions are produced for more than one week at a time.


#####################################################

#### Performance Review ----

# Check the accuracy of the predictions in hindsight
# Use most recent predictions to track (monitor) recent performance
# Use fpl_performance_comparison() function

# gw4
gw4_players_xp <- fpl_performance_comparison(players = player_details, gw = 4)

# gw5
gw5_players_xp <- fpl_performance_comparison(players = player_details, gw = 5)

# gw6
gw6_players_xp <- fpl_performance_comparison(players = player_details, gw = 6)

# gw8
gw8_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 8) %>%
  select(name, xP_odds, xP_index, total_points)

# gw9
gw9_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 9) %>%
  select(name, xP_odds, xP_index, total_points)

# gw10
gw10_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 10) %>%
  select(name, xP_odds, xP_index, total_points)

# gw11
gw11_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 11) %>%
  select(name, xP_odds, xP_index, total_points)

# gw12
gw12_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 12) %>%
  select(name, xP_odds, xP_index, total_points)

# gw13
gw13_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 13) %>%
  select(name, xP_odds, xP_index, total_points)

# gw14
gw14_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 14) %>%
  select(name, xP_odds, xP_index, total_points)

# gw16
gw16_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 16) %>%
  select(name, xP_odds, xP_index, total_points)

# gw20
gw20_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 20) %>%
  select(name, xP_odds, xP_index, total_points)

# gw21
gw21_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 21) %>%
  select(name, xP_odds, xP_index, total_points)

# Data missing for the following gameweeks.
# This is skewing results
# Ideally we like to model our results based on form from latest gameweeks
# Need to Adjust weight argument to control this
# gw1, gw2, gw3, gw15, gw17, gw18, gw19



test <- NULL
for (i in 1:21) {

  if (exists(paste0("gw", i, "_players_xp"))) {

    temp <- eval(parse(text = (paste0("gw", i, "_players_xp"))))
    #print(temp)
    test <- rbind(test, temp)

  } else {
    next
  }
}

print(paste("test", "Correlation - Total Points Vs. odds:",
            round(cor(test$xP_odds, test$total_points), 3)))

# Correlations - Index prediction method
print(paste("test", "Correlation - Total Points Vs. index:",
            round(cor(test$xP_index, test$total_points), 3)))


install.packages("ggplot2")
library(ggplot2)
library(tidyr)

gw21_players_xp_long <- pivot_longer(gw21_players_xp,
                                    cols = c(total_points, xP_odds, xP_index),
                                    names_to = "type",
                                    values_to = "total_points")

p <- ggplot(gw21_players_xp_long, aes(x = total_points, fill = type)) +
  geom_density(alpha = 0.2)



