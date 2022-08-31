


# Don't forget to specify type argument when installing devtools

# install.packages("devtools", type = "win.binary")
library(dplyr)
library(devtools)
library(fuzzyjoin)
library(Rglpk)
library(rvest)
library(roxygen2)
#devtools::install_github("ewenme/fplr", force = T)
#devtools::install_github("wiscostret/fplscrapR", force = T)
library(fplr)
library(fplscrapR)
#usethis::use_package("fplr")
#usethis::use_package("fplscrapR")


#devtools::install_github("j-abrams/fplBuddy", force = T)
library(fplBuddy)


####################################################################

devtools::document()
#load_all()
#library(fplBuddy)
#check()



# TODO: further documentation using roxygen
# TODO: change name of data sets including where they appear in functions.
# TODO: Functions to compare model performance vs actual scores at some point



# Goals, assists and clean sheets
odds_gs_gw5 <- fplBuddy::fpl_odds_generator_gs(odds_gs_gw5)
odds_cs_gw5 <- fplBuddy::fpl_odds_generator_cs()

#fpl_load_historic_data()
#fpl_fixtures_difficulty_rating()

# period
period <- fplBuddy::fpl_fixtures(5, 5)
# sample table for difficulty
fplBuddy::fpl_fixtures_difficulty_rating(5, 5)


#weight = 0.5, strength_index = 1
players_index <-
  fplBuddy::fpl_calculate_predictors(period, odds_gs = odds_gs_gw5, odds_cs = odds_cs_gw5)


# user unique id
user <- "6238967"

#odds
players_xP <- fplBuddy::fpl_calculate_xP(players_index, predictors_odds, user)
sum(players_xP$xP)

#indexes
players_xP2 <- fplBuddy::fpl_calculate_xP(players_index, predictors_indexes, user)
sum(players_xP2$xP)


sol <- fpl_optimise(players_xP, players_xP$xP, budget = 83.6)


# Budget
4 + 7.5 + 5 + 7 + 13 + 5.3 + 6.2 + 4.5 + 11.4 + 8.1 + 11.6

# Dream Team
sol <- fplBuddy::fpl_optimise(players_xP, players_xP$xP, budget = 83.6)
# Optimal transfers
sol <- fplBuddy::fpl_optimise_my_team(players_xP, players_xP$xP, budget = 83.6)

# My Team
my_team <- fpl_my_team(user)

# Tell me which players should be subbed out
for (i in unlist(my_team)) {
  if (!(i %in% sol$name)) {
    print(i)
  }
}

