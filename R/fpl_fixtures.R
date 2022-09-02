

#' Return a dataframe containing all fixtures coming up between a start and end date.
#'
#' Denotes home and away team strength for each team in each fixture.
#'
#' @param gameweek1 Period start date.
#'
#' @param gameweek2 Period end date.
#'
#' @return Dataframe
#'
#' @export


# Which team has the easiest fixtures for the next 5 weeks? Gooners
# This function will tell you which team has the easiest fixtures
# For selected period between 2 game weeks (inclusive bounds)
#
# When are the double gameweeks?
# Not decided yet

# fpl_fixtures_difficulty_rating()
#
# Rate fixtures difficulty between two selected gameweeks.

# gameweek1 <- 6
# gameweek2 <- 7

fpl_fixtures <- function(gameweek1, gameweek2) {

  # Team data acquired via fplr package
  teams <- fplr::fpl_get_teams() %>%
    select(id, name, strength, strength_overall_home, strength_overall_away)
  # Can we learn anything from strength_home, strength_away


  # Specifying fixtures and fixture difficulty by joining with teams
  fixtures <- fplr::fpl_get_fixtures() %>%
    left_join(teams, by = c("team_h" = "id")) %>%
    left_join(teams, by = c("team_a" = "id")) %>%
    select("gameweek" = "event",
           kickoff_time,
           team_h,
           team_a,
           "team_h_name" = "name.x",
           "team_a_name" = "name.y",
           team_h_difficulty,
           team_a_difficulty,
           strength_overall_home = strength_overall_home.x,
           strength_overall_away = strength_overall_away.y)

  # Filter subject to user selected gameweek arguments
  fixtures_x_weeks <- fixtures %>%
    filter(gameweek %in% c((gameweek1 - 1):gameweek2))

  # Initialisation before commencing loop operation
  fixtures_all <- NULL

  # for loop iterate through teams one by one to find fixtures
  # contained within specified gameweek period.
  for (i in teams$name) {
    fixtures_temp <- fixtures_x_weeks %>%
      filter(team_h_name == i | team_a_name == i) %>%
      mutate(team = i,
             opponent = case_when(
               team_h_name == i ~ team_a_name,
               team_a_name == i ~ team_h_name),
             was_home = case_when(
               team_h_name == i ~ 1,
               team_a_name == i ~ 0),
             opponent_difficulty = case_when(
               team_h_name == i ~ team_h_difficulty,
               team_a_name == i ~ team_a_difficulty)) %>%
      select(gameweek, team, opponent, opponent_difficulty,
             was_home, strength_overall_home, strength_overall_away, kickoff_time)

    fixtures_all <- rbind(fixtures_all, fixtures_temp)
  }

  # Data wrangling - fixtures_all_2 to be returned
  fixtures_all_2 <- fixtures_all %>%
    left_join(teams, by = c("team" = "name")) %>%
    select(-opponent_difficulty, -strength, -strength_overall_home.y, -strength_overall_away.y) %>%
    dplyr::rename(strength_overall_home = strength_overall_home.x,
                  strength_overall_away = strength_overall_away.x) %>%

    group_by(team) %>%
    mutate(next_match =
             as.numeric(as.Date(max(kickoff_time)) - as.Date(shift(kickoff_time)))) %>%
    mutate(next_match = ifelse(is.na(next_match), 0, next_match)) %>%
    filter(next_match != 0) %>%
    mutate(next_match = sum(next_match)) %>%
    select(-kickoff_time)

  return(fixtures_all_2)

}



