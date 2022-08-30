


# Don't forget to specify type argument when installing devtools
# install.packages("devtools", type = "win.binary")



library(dplyr)
library(devtools)
library(fuzzyjoin)
library(Rglpk)
library(rvest)
library(roxygen2)


devtools::install_github("ewenme/fplr", force = T)
devtools::install_github("wiscostret/fplscrapR", force = T)
library(fplr)
library(fplscrapR)
#usethis::use_package("fplr")
#usethis::use_package("fplscrapR")



####################################################################

devtools::document()
load_all()
library(fplBuddy)


use_mit_license()
check()






#path <- "data/FFP Points Predictor gw5.csv"
odds_gs_gw5 <- fpl_odds_generator_gs(odds_gs_gw5)

url <- "https://www.fantasyfootballreports.com/premier-league-clean-sheet-odds/"
xpath <- '//*[@id="tablepress-166"]'
odds_cs_gw5 <- fpl_odds_generator_cs(url, xpath)



#usethis::use_data(fpl_historical_data, overwrite = T)

all_gameweeks_21_22 <- fpl_load_historic_data()

period <- fpl_fixtures(5, 5)
#fpl_fixtures_difficulty_rating(5, 5)




players_index <- fplBuddy::fpl_calculate_predictors(period, weight = 0.5, strength_index = 1, gw = 5,
                                                    odds_gs_gw5, odds_cs_gw5)




user <- "6238967"

#odds
players_xP <- fpl_calculate_xP(players_index, predictors_odds, user)
sum(players_xP$xP)
#indexes
players_xP2 <- fpl_calculate_xP(players_index, predictors_indexes, user)
sum(players_xP2$xP)


sol <- fpl_optimise(players_xP, players_xP$xP, budget = 83.6)


# Budget
4 + 7.5 + 5 + 7 + 13 + 5.3 + 6.2 + 4.5 + 11.4 + 8.1 + 11.6

sol <- fpl_optimise(players_xP2, players_xP2$xP, budget = 83.6)
sol <- fpl_optimise_my_team(players_xP, players_xP2$xP, budget = 83.6)


my_team <- fpl_my_team(user)

# Tell me which players should be subbed out
for (i in unlist(my_team)) {
  if (!(i %in% sol$name)) {
    print(i)
  }
}


fpl_fixtures_difficulty_rating(5,7)
