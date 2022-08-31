

#' Produce a table detailing fixture difficulty for each team over future gameweeks.
#'
#' Investigate which teams have the easiest and hardest run of fixtures coming up.
#'
#' @param gameweek1 Period start date.
#'
#' @param gameweek2 Period end date.
#'
#' @return Dataframe.
#'
#' @export



# fpl_fixtures_difficulty_rating()
#
# Variation on the same function here

fpl_fixtures_difficulty_rating <- function(gameweek1, gameweek2) {

  # Team data acquired via fplr package
  teams <- fplr::fpl_get_teams() %>%
    select(id, name, strength, strength_overall_home, strength_overall_away)

  # Specifying fixtures and fixture difficulty by joining with teams
  fixtures <- fplr::fpl_get_fixtures() %>%
    left_join(teams, by = c("team_h" = "id")) %>%
    left_join(teams, by = c("team_a" = "id")) %>%
    select("gameweek" = "event",
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
    filter(gameweek %in% c(gameweek1:gameweek2))

  # Initialisation
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
      select(gameweek, team, opponent, opponent_difficulty, was_home, strength_overall_home, strength_overall_away)

    fixtures_all <- rbind(fixtures_all, fixtures_temp)
  }


  # This could also be useful info at some point - might wish to return this
  # summary of who has the easiest fixtures, calculaed via opponent difficulty metric.
  fixtures_all_final <- fixtures_all %>%
    group_by(team) %>%
    summarise(fixtures_difficulty = sum(opponent_difficulty)) %>%
    arrange(fixtures_difficulty)

  return(fixtures_all_final)
}



