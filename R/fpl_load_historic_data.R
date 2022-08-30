



#' Load historic gameweek data from the 2021/22 season
#'
#' This data offers a gameweek by gameweek breakdown of fields such as total_points, ICT score, etc.
#'
#' @export


# fpl_load_historic_data


# This function is used in regression script i believe? Investigate on monday.
fpl_load_historic_data <- function()  {

  # Adjust teams to get team strength home and away etc.
  teams <- fpl_get_teams() %>%
    dplyr::select(name, strength_overall_home, strength_overall_away)

  # Messy data - need to include extra rows for relegated teams 21/22 season
  # Assumption - dummy data to denote home and away strength ratings for missing teams
  teams_adjusted <- rbind(teams,
               c("Norwich", min(teams$strength_overall_home) * 0.9, min(teams$strength_overall_away) * 0.9),

               c("Watford", min(teams$strength_overall_home) * 0.95, min(teams$strength_overall_away) * 0.95),

               c("Burnley", min(teams$strength_overall_home), min(teams$strength_overall_away))) %>%

    # Strength indicators seem to be wrong way round strangely
    dplyr::rename(strength_overall_home = strength_overall_away,
                  strength_overall_away = strength_overall_home)


  # Stick it all together
  # Read from csv in data repo
  all_gameweeks <- fpl_historical_data %>%
    dplyr::select("GW", "name", "position", "team",
                  "opponent_name", "was_home", "minutes",
                  "total_points", "james_points_index", "james_ict_index",
                  "goals_scored", "assists", "clean_sheets") %>%

    # james strength index
    # takes home / away into account as well as strength metric for each team
    left_join(teams_adjusted, by = c("team" = "name")) %>%
    mutate(team_strength_overall =
             ifelse(was_home == 1, strength_overall_home, strength_overall_away)
    ) %>%
    dplyr::select(-c(strength_overall_home, strength_overall_away)) %>%

    left_join(teams_adjusted, by = c("opponent_name" = "name")) %>%
    mutate(opponent_strength_overall =
             ifelse(was_home == 1, strength_overall_home, strength_overall_away)
    ) %>%
    dplyr::select(-c(strength_overall_home, strength_overall_away)) %>%
    mutate(james_strength_index =
             # Add was_home to weight home performance
             was_home +
             (as.numeric(team_strength_overall) -
             as.numeric(opponent_strength_overall)) / 100 ) %>%
    dplyr::select(-c(team_strength_overall, opponent_strength_overall, was_home)) %>%

    # Messy data - fix posiotion type.
    mutate(position = ifelse(position == "GKP", "GK", position)) %>%
    filter(GW != 1)


  # Derived metric for processing goal, assist and clean sheet odds
  # Includes standardisation and scaling
  all_gameweeks <- all_gameweeks %>%
    mutate(james_odds_index =
             range01(minutes) * 2 +
             assists * 3 +
             case_when(
               position == "FWD" ~ goals_scored * 4,
               position == "MID" ~ goals_scored * 5,
               position == "DEF" ~ goals_scored * 6,
               position == "GK"  ~ goals_scored * 6
             )
    ) %>%
    mutate(james_cs_index =
             case_when(
               position == "FWD" ~ clean_sheets * 0,
               position == "MID" ~ clean_sheets * 1,
               position == "DEF" ~ clean_sheets * 4,
               position == "GK"  ~ clean_sheets * 4
             )
    ) %>%
    select(-minutes, -goals_scored, - assists, -clean_sheets)

  # Return
  return(all_gameweeks)

}



