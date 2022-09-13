


# Don't forget to specify type argument when installing devtools

# install.packages("devtools", type = "win.binary")
library(dplyr)
library(devtools)
library(fuzzyjoin)
library(Rglpk)
library(rvest)
library(roxygen2)
library(data.table)
library(stringr)
library(reticulate)
library(jsonlite)

#devtools::install_github("ewenme/fplr", force = T)
#devtools::install_github("wiscostret/fplscrapR", force = T)
library(fplr)
library(fplscrapR)
#usethis::use_package("fplr")
#usethis::use_package("fplscrapR")


#devtools::install_github("j-abrams/fplBuddy", force = T)
library(fplBuddy)


####################################################################

# TODO: Warning: Supporting functions each need to be in an individual script
devtools::document()
#load_all()
#check()



#
# TODO: Gameweek Functions to compare model performance vs actual scores at some point
#
# TODO: Think about matching on id rather than name -
# This would avoid name clashes we are seeing
# Some manual computation to correct IDs fo missing players.

# TODO: Develop combined method - predict points for first gameweek using odds
# Proceeding gameweeks using index method (odds not available).
# Combine both methods - select players for team based on average points scored between the two

# TODO: Experiment with strength index - maybe find a way for influence to be stronger for higher scoring players
# This would give a better indication of whom to captain.
# Include days to next match in the metric
#
# TODO: Automate gameweek argument?
#
########################################
#
# TODO: change name of data sets including where they appear in functions.
# - Move data to behind-the-scenes location
# DONE - Moved fpl historic data
# TODO: Budget function - Include selling price somehow - DONE.
#
# TODO: Compare gw6 predictions vs actuals - DONE.
#
# TODO: Get odds data for gw7
# Clean sheets - DONE.
# goal scorers - not updated on the website

# TODO:
# tidymodels package
# play around with random forests




#######################################

library(fplBuddy)
# Odds data ----

# Goals, assists and clean sheets
odds_gs_gw8 <- read.csv("data/FFP Points Predictor gw6.csv")
odds_cs_gw8 <- fplBuddy::fpl_odds_generator_cs()
# use_data() - Update each gameweek iteration

use_data(odds_cs_gw8 , overwrite = T)




# Some useful code to overwrite sys_data if you ever need to - for later reference.
#https://github.com/r-lib/usethis/issues/1512

# my_new_env <- new.env(hash = FALSE)
#
# # load current internal data into this new environment
# load("R/sysdata.rda", envir = my_new_env)
#
# # add or replace some objects
# my_new_env$odds_gs_gw6 <- odds_gs_gw6
#
# # save the environment as internal package data
# save(list = names(my_new_env),
#      file = "R/sysdata.rda",
#      envir = my_new_env)


#########################
#
# Out of date now.

# gameweek
gameweek <- fpl_get_gameweek_next()$id - 1

gameweek <- 6

# period
period <- fpl_fixtures(gameweek, gameweek)
# sample table for difficulty
fplBuddy::fpl_fixtures_difficulty_rating(gameweek, gameweek)

# This returns ict index and points totals as of most recent week
players <- fpl_get_player_all()
players <- gw_joined
# TODO: Replace players with something else - gw_joined hits the spot. Why? to consider previous gameweek data

#weight = 0.5, strength_index = 1
players_index <- fpl_calculate_predictors(
                   players, period, gw = gameweek, weight = 0.5, strength_index = 1,
                   odds_gs = odds_gs_gw6_test,
                   odds_cs = odds_cs_gw6)


# user unique id
user <- "6238967"

#odds
players_xP_odds <- fpl_calculate_xP(players_index, predictors_odds, user = user, gw = gameweek)
sum(players_xP_odds$xP)

players_xP_index <- fpl_calculate_xP(players_index, predictors_indexes, user = user, gw = gameweek)
sum(players_xP_index$xP)

# Export finalised predictions each week.
# write.csv(players_xP_odds, "data/Previous week predictions/gw6_players_xP_odds.csv")
# write.csv(players_xP_index, "data/Previous week predictions/gw6_players_xP_index.csv")



#########################################

# Optimisation ----

# Budget
# 5 + 5 + 7.5 + 6 + 5.5 + 7.8 + 7.7 + 13 + 8.1 + 6.3 + 8.1
budget <- fpl_my_budget(key = "ja11g14@soton.ac.uk",
                        secret = "vonfoj-tyhby9-vyrrUf",
                        user = user)

# Dream Team
sol <- fplBuddy::fpl_optimise(players_xP_odds, players_xP_odds$xP, budget = budget)
sol <- fplBuddy::fpl_optimise(players_xP_index, players_xP_index$xP, budget = budget)

# Optimal transfers
sol <- fpl_optimise_my_team(players_xP_index, players_xP_index$xP, budget = budget, transfers = 2)


# My Team
# Tell me which players should be subbed out
for (i in unlist(fpl_my_team(user, squad = 11))) {
  if (!(i %in% sol$name)) {
    print(i)
  }
}



##############################################################

# Further testing
# fpl_gameweek_predictions serves as a master function.
# Concerned about the duplicated names. Might try to join on player id in future. Done here

# Initiate
player_details <- fplscrapR::get_player_details()


# Find one week at a time.
# Next - develop to have multiple weeks as we had before - need to focus on index metric not odds to do this though.

# Test - Different from before because of id matching
test  <- fpl_gameweek_predictions(players = player_details, 8) %>%
  filter(!(is.na(xP_odds)))

#TODO: Figure out how to deal with NAs... Team not being picked up is breaking the optimisation clearly.
# Look at this tomorrow please Jimmy


test2 <- read.csv("data/Previous week predictions/gw6_players_xP_odds.csv")

sol <- fplBuddy::fpl_optimise(test, test$xP_odds, budget = 83)
sol <- fpl_optimise_my_team(test, test$xP_odds, budget = 83, transfers = 5)

# Scope to combine both methods into one here..............
# Fixture / strength index still not influential enough for predicting hauls for me.


# Players missing - duplicated names or NAs - unable to join these guys
# missing <- players_id %>%
#   filter(name %!in% test2$name)



#####################################################

# Use most recent predictions to track (monitor) recent performance
# Use fpl_performance_comparison() function



# gw4
gw4_players_xp <- fpl_performance_comparison(players = player_details, gw = "gw4")

# gw5
gw5_players_xp <- fpl_performance_comparison(players = player_details, gw = "gw5")

# gw6
gw6_players_xp <- fpl_performance_comparison(players = player_details, gw = "gw6")





