

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


  # Fix for strength index.
  # Had not been updated in a while -
  # strength index now computes relative to teams current position in the table
  table <-
    read_html("https://www.bbc.co.uk/sport/football/tables") %>%
    html_table() %>%
    as.data.frame() %>%
    select(Team, Pts) %>%
    head(-1) %>%
    arrange(Team) %>%
    mutate(id = 1:20) %>%
    select(-Team)

  # Team data acquired via fplr package
  teams <- fplr::fpl_get_teams() %>%
    select(id, name, strength, strength_overall_home, strength_overall_away) %>%
  # Can we learn anything from strength_home, strength_away
    left_join(table, by = "id")

  max_strength_home <- max(teams$strength_overall_home)
  min_strength_home <- min(teams$strength_overall_home)

  max_strength_away <- max(teams$strength_overall_away)
  min_strength_away <- min(teams$strength_overall_away)

  teams <- teams %>%
    mutate(strength_overall_home = denormalize(range01(as.numeric(Pts)),
                                               min_strength_home, max_strength_home)) %>%
    mutate(strength_overall_away = denormalize(range01(as.numeric(Pts)),
                                               min_strength_away, max_strength_away)) %>%
    select(-Pts)



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

  fixtures_test <- fixtures %>%
    select(gameweek) %>%
    distinct() %>%
    mutate(gameweek2 = lag(gameweek)) %>%
    filter(gameweek == gameweek1)

  gameweek_last <- fixtures_test$gameweek2

  # Filter subject to user selected gameweek arguments
  fixtures_x_weeks <- fixtures %>%
    group_by(gameweek) %>%
    filter(!(is.na(gameweek))) %>%
    filter(gameweek %in% c(gameweek_last:gameweek2))

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
    filter(gameweek %in% c(gameweek1:gameweek2)) %>%
    mutate(next_match = sum(next_match)) %>%
    select(-kickoff_time)
  fixtures_all_2 <- fixtures_all_2 %>%
    mutate(next_match = ifelse(next_match == 0, max(fixtures_all_2$next_match), next_match))

  return(fixtures_all_2)

}



