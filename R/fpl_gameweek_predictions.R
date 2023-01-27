

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


fpl_gameweek_predictions <- function(players = player_details, gameweek = 20) {

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



  # Get gw_joined
  gw_joined <- fpl_get_gw_joined(players, gameweek)

  # Get period
  period <- fpl_fixtures(gameweek, gameweek)

  # Get odds
  # Ensuring odds data populates.
  # "while" loop Will back track to find most recent odds in absence of the latest entries.


  # Weird warning.
  #
  # Warning message:
  # In rm(list = Filter(exists, c("odds_gs", "odds_cs"))) :
  #   object 'odds_gs' not found
  # rm(list = Filter(exists, c("odds_gs", "odds_cs")))

  i <- 0
  while (!exists("odds_gs")) {
    print(i)
    skip_to_next <- FALSE
    tryCatch(
      odds_gs <- eval(parse(text = paste0("fplBuddy::odds_gs_gw", gameweek - i))),
      error = function(e) {
        skip_to_next <<- TRUE
      }
    )
    if(skip_to_next) {
      i <- i + 1
    }
  }

  # Inform user which odds data is being used by default
  print(paste("Defaulting to use odds data for gameweek",  gameweek - i))


  # Acquiring odds_gs and odds_cs - saved as data files within the package
  odds_gs <- fpl_odds_generator_gs(odds_gs) %>%
    mutate(gameweek = gameweek)

  odds_cs <- eval(parse(text = paste0("fplBuddy::odds_cs_gw", gameweek - i))) %>%
    mutate(gameweek = gameweek)


  # players_index
  players_index <- fpl_calculate_predictors(
    players = gw_joined, period = period, weight = 1,
    odds_gs = odds_gs,
    odds_cs = odds_cs)


  # Expected points (xP) for both odds method and index method
  gw_players_xP_odds <- fpl_calculate_xP(
    players_index, predictors_odds, user = "6238967", gw = gameweek - i - 1)

  gw_players_xP_index <- fpl_calculate_xP(
    players_index, predictors_indexes, user = "6238967", gw = gameweek - i - 1)


  # Call everything together
  gw_players_xP <- gw_players_xP_odds %>%
    rename("xP_odds" = "xP") %>%
    left_join(gw_players_xP_index, by = c("name", "team", "gameweek", "position", "now_cost", "in_my_team")) %>%
    rename("xP_index" = "xP") %>%
    select(name, team, position, gameweek, now_cost, xP_odds, xP_index, in_my_team) #%>%
    #mutate(xP_odds = ifelse(is.na(xP_index), NA, xP_odds))


  gw_players_xP <- gw_players_xP %>%
    filter(!(is.na(gameweek)))

  # return
  return(gw_players_xP)

}


