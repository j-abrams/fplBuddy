

#' Return expected points predictions for a specified gameweek
#'
#' Master function compiling outputs from various other functions.
#'
#' @param player_details The result of fplscrapR::get_player_details()
#'
#' @param gameweek Gameweek for which to return predictions for
#'
#' @param predictions "odds" or "index" - Decide which method to call
#'
#' @return Dataframe.
#'
#' @export


fpl_gameweek_predictions <- function(players = player_details, gameweek = 8) {


  # Testing contents of function arguments to generate helpful error messages to the user

  if ("total_points" %!in% names(players) &
      "ict_index" %!in% names(players)) {
    stop("Ensure players argument is a dataframe containing fpl players
  which includes the columns total_points and ict_index.")
  }

  if (!(is.numeric(gameweek))) {
    stop("Gameweek argument will only accept a numeric input.")
  }

  if (gameweek < 4) {
    stop("Odds data not available prior to gameweek 4.")
  }
  if (gameweek == 7) {
    stop("Gameweek 7 is cancelled. Please enter another gameweek.")
  }



  last_gameweek <- gameweek - 1

  # Populate list of past gameweeks
  gameweeks <- list()
  for (i in 1:last_gameweek) {
    #print(paste0("gw", i))
    gameweeks <- append(gameweeks, list(paste0("gw", i)))
  }


  gw_collated <- NULL

  for (i in 1:length(gameweeks)) {
    temp <- players %>%
      group_by(playername) %>%
      filter(fixture <= i * 10 & fixture > (i * 10) - 10) %>%
      mutate(was_home = as.numeric(was_home)) %>%
      select(name = playername, minutes, element, total_points, ict_index) %>%
      ungroup()

    # Assignment - Might need this later
    assign(gameweeks[[i]], temp)

    # Form gw_collated using rbind()
    gw_collated <- rbind(gw_collated, temp)
  }

  # Save Past predictions - Opportunity to do some back casting here
  # Odds data only available from gameweek 4 onwards

  # 1. Collate
  gw_collated <- gw_collated %>%
    group_by(name) %>%
    summarise(element = min(element),
              minutes = sum(minutes),
              total_points = sum(total_points),
              ict_index = sum(as.numeric(ict_index)),
              matches_played = n())

  # 2. Join
  gw_joined <- fpl_get_player_all() %>%
    #mutate(name = paste(first_name, second_name)) %>%
    left_join(gw_collated, by = c("id" = "element") ) %>%
    mutate(total_points = total_points.y,
           ict_index = ict_index.y,
           minutes = minutes.y) %>%
    select(-c(name, minutes.x, minutes.y, ict_index.x, total_points.x, ict_index.y, total_points.y))


  # period
  period <- fpl_fixtures(gameweek, gameweek)


  # odds
  odds_gs <- fpl_odds_generator_gs(
    eval(parse(text = paste0("fplBuddy::odds_gs_gw", gameweek)))
  ) %>%
    mutate(gameweek = gameweek)

  odds_cs <- eval(parse(text = paste0("fplBuddy::odds_cs_gw", gameweek))) %>%
    mutate(gameweek = gameweek)


  # players_index
  players_index <- fpl_calculate_predictors(
    players = gw_joined, period = period,
    odds_gs = odds_gs,
    odds_cs = odds_cs)

  gw_players_xP_odds <- fpl_calculate_xP(
    players_index, predictors_odds, user = "6238967", gw = gameweek)

  gw_players_xP_index <- fpl_calculate_xP(
    players_index, predictors_indexes, user = "6238967", gw = gameweek)


  # Call everything together
  gw_players_xP <- gw_players_xP_odds %>%
    rename("xP_odds" = "xP") %>%
    left_join(gw_players_xP_index, by = c("name", "team", "gameweek", "position", "now_cost", "in_my_team")) %>%
    rename("xP_index" = "xP") %>%
    select(name, team, position, gameweek, now_cost, xP_odds, xP_index, in_my_team) %>%
    mutate(xP_odds = ifelse(is.na(xP_index), NA, xP_odds))

  # return
  return(gw_players_xP)

}


