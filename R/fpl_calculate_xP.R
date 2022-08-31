




#' Calculate expected points for each player
#'
#' Two separate methods - use odds data, or use other indexes (average points, ict index etc.)
#'
#' @param data Output from "fpl_calculate_predictors" function used as the initial starting point
#'
#' @param predictors select which variables to use as predicting variables - either odds or indexes
#'
#' @return Dataframe.
#'
#' @export


# fpl_calculate_xP()
#
# Use players_index as a starting point to build our model.
# Select predictors based on contents of "predictors" argument

# Test:
# train = all_gameweeks

fpl_calculate_xP <- function(data = players_index, predictors = predictors_odds, user) {

  # Calculate expected points for the period (xP) - Using historical model
  model <- lm(total_points ~ ., data = fpl_historical_data_final %>% select(predictors))
  p <- predict(model, data)

  # Take a look at residuals and r squared
  print(summary(model))

  # save p as a new coloumn in players_collated_final
  players_collated <- data %>%
    mutate(p = round(p, 2))  %>%
    group_by(name) %>%
    mutate(xP = sum(p)) %>%
    select(name, team, position, now_cost, xP) %>%
    distinct() %>%
    ungroup()

  gw <- fpl_get_gameweek_current()[[1]] - 1

  my_team <- fpl_my_team(user, squad = 11)

  # Join with "my_team" to identify which players are in my team.
  players_xP <- players_collated %>%
    mutate(in_my_team = ifelse(name %in% my_team, 1, 0))

  return(players_xP)
}

