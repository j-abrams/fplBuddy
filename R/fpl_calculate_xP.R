




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


# TODO: investigate whether we can get better predictions by ignoring lower scoring players
# fpl_historical_data_final2 <- fplBuddy::fpl_historical_data_final %>%
#   group_by(name) %>%
#   arrange(desc(total_points)) %>%
#   head(16000)


fpl_calculate_xP <- function(data = players_index, predictors = predictors_indexes,
                             user, gw = 20) {

  # Calculate expected points for the period (xP) - Using historical model
  model <- lm(total_points ~ ., data = fplBuddy::fpl_historical_data_final %>% select(predictors))
  p <- predict(model, data)

  # Take a look at residuals and r squared
  print(summary(model))

  # View Correlation if desired
  #cor(model$model$total_points, model$model$james_odds_index)
  #cor(model$model$total_points, model$model$james_cs_index)

  test <- data %>%
    select(gameweek) %>%
    distinct()


  if (nrow(test) == 1) {

    # save p as a new coloumn in players_collated_final
    players_collated <- data %>%
      mutate(p = round(p, 2))  %>%
      group_by(name) %>%
      mutate(xP = sum(p)) %>%
      select(name, team, gameweek, position, now_cost, xP) %>%
      distinct() %>%
      ungroup()

  } else {

    players_collated <- data %>%
      mutate(xP = round(p, 2)) %>%
      select(name, team, gameweek, position, now_cost, xP)

  }


  my_team <- fpl_my_team(user, squad = 11, gw = gw)

  # Join with "my_team" to identify which players are in my team.
  players_xP <- players_collated %>%
    mutate(in_my_team = ifelse(name %in% my_team, 1, 0)) %>%
    filter(!is.na(xP))

  return(players_xP)
}

