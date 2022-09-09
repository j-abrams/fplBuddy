


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



# TODO: change name of data sets including where they appear in functions.
# - Move data to behind-the-scenes location
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

# TODO: Budget function - Include selling price somehow - DONE.
#
# TODO: Compare gw6 predictions vs actuals - DONE.
#
# TODO: Get odds data for gw7
# Clean sheets - DONE.
# goal scorers - not updated on the website

# TODO: Automate gameweek argument?



# Goals, assists and clean sheets
odds_gs_gw6 <- fplBuddy::fpl_odds_generator_gs(read.csv("data/FFP Points Predictor gw6.csv"))
odds_cs_gw7 <- fplBuddy::fpl_odds_generator_cs()
# use_data() - Update each gameweek iteration



# period
period <- fpl_fixtures(7, 7)
# sample table for difficulty
fplBuddy::fpl_fixtures_difficulty_rating(7, 7)

# This returns ict index and points totals as of most recent week
players <- fpl_get_player_all()

#weight = 0.5, strength_index = 1
players_index <- fpl_calculate_predictors(
                   players, period, gw = 7, weight = 0.5, strength_index = 1,
                   odds_gs = odds_gs_gw6,
                   odds_cs = odds_cs_gw6
                   )


# user unique id
user <- "6238967"

#odds
players_xP_odds <- fpl_calculate_xP(players_index, predictors_odds, user = user, gw = 6)
sum(players_xP_odds$xP)

players_xP_index <- fpl_calculate_xP(players_index, predictors_indexes, user = user, gw = 7)
sum(players_xP_index$xP)

# Export finalised predictions each week.
# write.csv(players_xP_odds, "data/Previous week predictions/gw6_players_xP_odds.csv")
# write.csv(players_xP_index, "data/Previous week predictions/gw6_players_xP_index.csv")



# Budget
# 5 + 5 + 7.5 + 6 + 5.5 + 7.8 + 7.7 + 13 + 8.1 + 6.3 + 8.1
budget <- fpl_my_budget(key = "ja11g14@soton.ac.uk", secret = "vonfoj-tyhby9-vyrrUf", user = "6238967")


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







