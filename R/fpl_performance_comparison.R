

#' Compare how predictions shape up against observed actuals, one gameweek at a time.
#'
#' Print correlation for both methods for a side by side comparison
#'
#' @param gw Gameweek we are comparing
#'
#' @return Dataframe.
#'
#' @export


# Comparisons

# For monitoring purposes
# Take a look at how odds and index predictions shape up against actual recored points.
# Odds is approx 60% accurate, index approx 50%

fpl_performance_comparison <- function(players, gw = "gw4") {

  # Prep
  gameweek <- fpl_get_gameweek_next()$id

  # Populate list of past gameweeks
  gameweeks <- list()
  for (i in 1:gameweek) {
    #print(paste0("gw", i))
    gameweeks <- append(gameweeks, list(paste0("gw", i)))
  }



  for (i in 1:length(gameweeks)) {
    temp <- players %>%
      group_by(playername) %>%
      filter(fixture <= i * 10 & fixture > (i * 10) - 10) %>%
      mutate(was_home = as.numeric(was_home)) %>%
      select(name = playername, minutes, element, total_points, ict_index) %>%
      ungroup()

    # Assignment - Might need this later
    assign(gameweeks[[i]], temp)
  }



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

