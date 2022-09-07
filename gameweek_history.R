


# Tomorrow ----

# Sell price seems to be available from fpl api data

# Concerned about the duplicated names. Might try to join on player id in future.

# Use most recent predictions to track (monitor) recent performance




# Enter these vals for GAMEWEEK 6
# Items specified in "gameweeks" are weeks for which we already have the data
gameweeks <- list("gw1", "gw2", "gw3", "gw4", "gw5", "gw6")
player_details <- fplscrapR::get_player_details()

fpl_gameweek_points <- function(player_details = player_details, gameweeks = gameweeks) {

  gw_collated <- NULL

  for (i in 1:length(gameweeks)) {
    temp <- player_details %>%
      group_by(playername) %>%
      filter(fixture <= i * 10 & fixture > (i * 10) - 10) %>%
      mutate(was_home = as.numeric(was_home)) %>%
      select(name = playername, total_points, ict_index) %>%
      ungroup()

    # Assignment - Might need this later
    assign(gameweeks[[i]], temp)

    # Form gw_collated using rbind()
    gw_collated <- rbind(gw_collated, temp)
  }

  # 1. Collate
  gw_collated <- gw_collated %>%
    group_by(name) %>%
    summarise(total_points = sum(total_points),
              ict_index = sum(as.numeric(ict_index)))

  # 2. Join
  gw_joined <- fpl_get_player_all() %>%
    mutate(name = paste(first_name, second_name)) %>%
    left_join(gw_collated, by = "name") %>%
    mutate(total_points = total_points.y,
           ict_index = ict_index.y) %>%
    select(-c(name, ict_index.x, total_points.x, ict_index.y, total_points.y)) %>%
    filter(news != "Transferred to Rangers - Unknown return date")

  most_recent_gw <- as.numeric(str_sub(gameweeks[[length(gameweeks)]], -1)) + 1

  period <- fplBuddy::fpl_fixtures(most_recent_gw, most_recent_gw)

  players_index <- fpl_calculate_predictors(
    players = gw_joined, period = period, gw = most_recent_gw,
    odds_gs = eval(parse(text = paste0("odds_gs_", gameweeks[[length(gameweeks)]]))),
    odds_cs = eval(parse(text = paste0("odds_cs_", gameweeks[[length(gameweeks)]]))))

  gw_players_xP_index <- fpl_calculate_xP(
    players_index, predictors_indexes, user = "6238967", gw = most_recent_gw)

  return(gw_players_xP_index)

}




test <- fpl_gameweek_points(player_details = player_details, gameweeks = gameweeks)



#######################################

# Save Past predictions - Opportunity to do some back casting here
# Odds data only available from gameweek 4 onwards

# 1. Collate

# 2. Join

period <- fplBuddy::fpl_fixtures(4, 4)

# 3. Get index and xP
players_index <- fpl_calculate_predictors(players = gw4_test, period = period, strength_index = 1, gw = 4,
                                          odds_gs = odds_gs_gw4, odds_cs = odds_cs_gw4)

gw4_players_xP_index <- fpl_calculate_xP(players_index, predictors_indexes, user = "6238967", gw = 4)
gw4_players_xP_odds <- fpl_calculate_xP(players_index, predictors_odds, user = "6238967", gw = 4)


write.csv(gw4_players_xP_index, "data/Previous week predictions/gw4_players_xP_index.csv")
write.csv(gw4_players_xP_odds,  "data/Previous week predictions/gw4_players_xP_odds.csv")

write.csv(gw5_players_xp, "gw5_predictions_actuals.csv")







#####################################################

# Comparisons ----

# For monitoring purposes
# Take a look at how odds and index predictions shape up against actual recored points.
# Odds is approx 60% accurate, index approx 50%

fpl_performance_comparison <- function(gw = "gw4") {

  # Prep for join
  gameweek <- eval(parse(text = gw)) %>%
    mutate(name = str_convert(name))

  # Define filenames to be read in using paste0 function and gw argument
  filename  <- paste0("data/Previous week predictions/", gw, "_players_xP_odds.csv")
  filename2 <- paste0("data/Previous week predictions/", gw, "_players_xP_index.csv")

  # Employ dplyr full_joins to pull predictions and actuals together
  gameweek_players_xp <- read.csv(filename) %>%
    full_join(read.csv(filename2), by = "name") %>%
    mutate(name = str_convert(name)) %>%
    full_join(gameweek, by = "name") %>%
    select(-ict_index) %>%
    select(name, "xP_odds" = "xP.x", "xP_index" = "xP.y", total_points) %>%
    filter(!is.na(total_points) & !is.na(xP_index))

  # Correlations - Odds prediction method
  print(paste(gw, "Correlation - Total Points Vs. odds:",
              round(cor(gameweek_players_xp$xP_odds, gameweek_players_xp$total_points), 3)))

  # Correlations - Index prediction method
  print(paste(gw, "Correlation - Total Points Vs. index:",
              round(cor(gameweek_players_xp$xP_index, gameweek_players_xp$total_points), 3)))

  return(gameweek_players_xp)
}


# gw4
gw4_players_xp <- fpl_performance_comparison(gw = "gw4")

# gw5
gw5_players_xp <- fpl_performance_comparison(gw = "gw5")

# gw6
gw6_players_xp <- fpl_performance_comparison(gw = "gw6")


