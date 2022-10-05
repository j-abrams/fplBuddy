



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

# TODO:
# tidymodels package
# play around with random forests




#######################################

devtools::document()
#load_all()
#check()
source("packages.R")


#########################
#
# Old methods

# gameweek
gameweek <- fpl_get_gameweek_next()$id




##############################################################

# Further testing
# fpl_gameweek_predictions serves as a master function.
# Concerned about the duplicated names. Might try to join on player id in future. Done here

devtools::document()


# Odds data ----

# Goals, assists and clean sheets
# Save latest data - Update these lines as appropriate

# https://www.fantasyfootballpundit.com/premier-league-goalscorer-assist-odds/

odds_gs_gw10 <- read.csv("FFP Points Predictor gw10.csv")
odds_cs_gw10 <- fplBuddy::fpl_odds_generator_cs()
use_data(odds_gs_gw10, overwrite = TRUE)
use_data(odds_cs_gw10)

# Initiate
player_details <- fplscrapR::get_player_details()

# sample table for difficulty
fplBuddy::fpl_fixtures_difficulty_rating(gameweek, gameweek)

# Find one week at a time.
# Next - develop to have multiple weeks as we had before - need to focus on index metric not odds to do this though.

# Test - Different from before because of id matching
rm(list = Filter(exists, c("odds_gs", "odds_cs")))
test <- fpl_gameweek_predictions(players = player_details, 9)


# Export finalised predictions each week.
write.csv(test, "data/Previous week predictions/gw9_players_xP.csv")

test2 <- read.csv("data/Previous week predictions/gw8_players_xP.csv")


# TODO: Find all documentation some how
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

# user unique id
user <- "6238967"

budget <- fpl_my_budget(key = "ja11g14@soton.ac.uk",
                        secret = "vonfoj-tyhby9-vyrrUf",
                        user = user)

# Find my team here...

sol <- fpl_optimise(test, test$xP_index, budget = budget)

sol <- fpl_optimise_my_team(input = test, obj_var = test$xP_odds,
                            budget = budget, transfers = 2)


# My Team
# Tell me which players should be subbed out
for (i in unlist(fpl_my_team(user, squad = 11))) {
  if (!(i %in% sol$name)) {
    print(i)
  }
}

# Scope to combine both methods into one here..............
# Fixture / strength index still not influential enough for predicting hauls for me.


#####################################################

#### Performance Review ----

# Use most recent predictions to track (monitor) recent performance
# Use fpl_performance_comparison() function

# gw4
gw4_players_xp <- fpl_performance_comparison(players = player_details, gw = 4)

# gw5
gw5_players_xp <- fpl_performance_comparison(players = player_details, gw = 5)

# gw6
gw6_players_xp <- fpl_performance_comparison(players = player_details, gw = 6)

# gw8
gw8_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 8)

# gw9
gw9_players_xp <- fpl_performance_comparison(players = player_details, gw_x = 9)


