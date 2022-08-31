



#' Calculate each of the unique indexes for modelling
#'
#' Converts fields such as average points, ict rating etc. into usable indexes for generating predicted points
#'
#' @param period Fixtures taking place in the specified period.
#'
#' @param weight Adjustment to weight predictions based more heavily on either current form or historical performance.
#'
#' @param strength_index "Low", "Med" or "High" to define influence fixtures have on our expected points.
#'
#' @param odds_gs Goalscoring and Assists odds for current week.
#'
#' @param odds_cs Odds for each team to keep a clean sheet this week.
#'
#' @return Dataframe
#'
#' @export




# fpl_calculate_predictors()

# Display all indexes side by side and carry out regression modelling
# Subject to which predictor variables are to be considered in this analysis

# Create players_collated here please.

# Testing
# weight = 0.5
# strength_index = 1
# gw = 5
# odds_gs = odds_gs_gw5
# odds_cs = odds_cs_gw5

fpl_calculate_predictors <- function(period, weight = 0.5, strength_index = 1, odds_gs = odds_gs_gw4, odds_cs = odds_cs_gw4) {

  # gw is used as a conversion for total points scored so far - to average points per gw
  # Therefore for gw 4, we want to divide total points by number of gameweeks completed so far: 3
  gw <- fplr::fpl_get_gameweek_current()$id - 1

  # Data preparation
  last_season_index <- fpl_historical_performance_data
  # TODO: include Ilkay Gundogan - visit archives - long.

  # Coefficient Used later for scaling in accordance with round01 standardisation function
  odds_cs_max <- max(odds_cs$clean_sheet_odds)

  # function from fplr package - return data on all 593 current players - Now 606 as of latest count
  players <- fpl_get_player_all()

  # Data wrangling
  players_collated <- players %>%
    mutate(name = paste(first_name, second_name)) %>%
    select(name, second_name, team, minutes, element_type, now_cost,
           total_points, ict_index) %>%

    # Define position from element_type variable here.
    mutate(position =
             case_when(
               element_type == 1 ~ "GK",
               element_type == 2 ~ "DEF",
               element_type == 3 ~ "MID",
               element_type == 4 ~ "FWD"
             )
           ) %>%
    # join with period - will be used to find strength index
    left_join(period, by = c("team" = "id")) %>%
    select(-team, -gameweek) %>%
    dplyr::rename("team" = "team.y") %>%

    # Define average_points, used to develop the final points index
    # This is points scored by each player so far this season.
    # Limited as we have only had two gameweeks at the time of writing!
    select(name, team, opponent, everything()) %>%
    group_by(name) %>%
    mutate(average_points = round((total_points / gw), 2)) %>%
    ungroup() %>%

    # james_points_index & james_ict_index
    # fuzzyjoin - the first step towards computing finalpoints and ict index scores
    fuzzyjoin::stringdist_left_join(last_season_index, by = "name", max_dist = 2) %>%
    # Soooo messy. Still so many players names don't match. See archives
    # This is a mess because names do not match in the two datasets

    # Adjust points index with "weight" - whether to prioritise this season form or last season average
    # As time moves on weight should adjust closer to 1 - as more gw data becomes available
    dplyr::rename("name" = "name.x") %>%
    mutate(james_points_index = ifelse(is.na(average_points.y),
                                       average_points.x,
                                       (weight * average_points.x) +
                                       ((1 - weight) * average_points.y))) %>%

    # Adjust ICT index with weight factor in the same manner as above
    mutate(ict_index_lag = ict_index / gw) %>%
    mutate(james_ict_index = ifelse(is.na(ict_index_avg),
                                    ict_index_lag,
                                    (weight * ict_index_lag) +
                                    ((1 - weight) * ict_index_avg))) %>%


    # james_strength_index
    # Calculate james_strength_index here - differential between strengths of home and away teams
    # On average home / away doesnt necessarily effect individual performances too highly
    mutate(james_strength_index =
             # Add "was_home"
             was_home +
             ifelse(was_home == 1,
                    (as.numeric(strength_overall_home) -
                       as.numeric(strength_overall_away)) / 100,
                    (as.numeric(strength_overall_away) -
                       as.numeric(strength_overall_home)) / 100)
           ) %>%

    # Determine which factors influence proceedings most heavily - points and ict (0.30 and 0.24 correlation)
    # TODO: Rename strength_index to be fixture_index.
    mutate(james_strength_index = james_strength_index * strength_index) %>%


  # james_odds_index and james_cs_index

    # Fix mitrovic player name
    mutate(second_name = ifelse(team == "Fulham" & startsWith(name, "Aleksandar"), "Mitrovic", second_name)) %>%

    # Combine with latest odds data here
    # solution using str_convert to get a better match
    mutate(second_name = str_convert(second_name)) %>%
    #mutate(name = str_convert(name)) %>%
    left_join(odds_gs, by = c("second_name" = "Name")) %>%
    left_join(odds_cs, by = c("team" = "Team")) %>%

    # Standardise
    # Fix goals
    mutate(AnytimeGoal = as.numeric(minutes) * as.numeric(AnytimeGoal)) %>%
    mutate(AnytimeGoal = range01(ifelse(is.na(AnytimeGoal), 0, AnytimeGoal))) %>%
    # Fix assists
    mutate(AnytimeAssist = as.numeric(minutes) * as.numeric(AnytimeAssist)) %>%
    mutate(AnytimeAssist = range01(ifelse(is.na(AnytimeAssist), 0, AnytimeAssist))) %>%
    # Fix clean sheets
    mutate(clean_sheet_odds = as.numeric(minutes) * as.numeric(clean_sheet_odds)) %>%
    mutate(clean_sheet_odds = range01(ifelse(is.na(clean_sheet_odds), 0, clean_sheet_odds))) %>%

    # james_odds_index
    # Include points gathered for mins played in the following manner
    # Include assist points
    # Points for a goal scored vary on position - hence employing a case when statement
    mutate(james_odds_index =
             # Modelling Assumption - Include extra points scored for minutes played
             range01(minutes) * 2 +
             # 3 points for registering an assist
             AnytimeAssist * 3 +
             # Points for scoring a goal
             case_when(
               position == "FWD" ~ AnytimeGoal * 4,
               position == "MID" ~ AnytimeGoal * 5,
               position == "DEF" ~ AnytimeGoal * 6,
               position == "GK"  ~ AnytimeGoal * 6
             )
           ) %>%
    # remove NAs
    mutate(james_odds_index = ifelse(is.na(james_odds_index), 0, james_odds_index)) %>%

    # Standardise and scale cs_index relative to minutes played
    # Reason for doing this - only have totals for teams at the moment
    mutate(james_cs_index =
             odds_cs_max *
             case_when(
               position == "MID" ~ clean_sheet_odds * 1,
               position == "DEF" ~ clean_sheet_odds * 4,
               position == "GK"  ~ clean_sheet_odds * 4,
               # Striker will score 0 extra points for clean sheet
               TRUE ~ 0
             )
           ) %>%

    # HOT FIX - 2 ben davies in the listings. One plays for spurs, the other for liverpool
    # Fuck off ben davies
    #group_by(name) %>%
    #filter(minutes == max(minutes)) %>%
    #ungroup() %>%
    # group_by(second_name) %>%
    # filter(total_points == max(total_points)) %>%
    # ungroup() %>%

    # Problematic Players ...
    # Why are they problematic? Cos second name is matched More than once,
    # so these players have inflated odds indexes
    # "Taylor Richards"    "Abdoulaye Doucouré" "Tom Davies"         "Harry Wilson"       "Nampalys Mendy"
    # "Daniel James"       "Tyler Adams"        "Archie Gray"        "Jordan Henderson"   "Rhys Williams"
    # "Luke Chambers"      "Harvey Davies"      "Kalvin Phillips"    "Brandon Williams"   "Amad Diallo"
    # "Jamal Lewis"        "Steve Cook"         "Omar Richards"      "Stuart Armstrong"   "Davinson Sánchez"
    # "Harvey White"       "Ben Johnson"        "Jackson Smith"

    select("name", "second_name", "team", "position",
           "opponent", "was_home", "now_cost",
           "james_odds_index", "james_cs_index",
           "james_points_index", "james_ict_index", "james_strength_index")


  #, "minutes", "AnytimeGoal", "AnytimeAssist", "clean_sheet_odds")
  # Select these fields to check odds
  # Some of these are so dumb. Ziyech and gordon for example - why are their odds so high ffs
  # Fixed this by scaling odds index relative to minutes played


  return(players_collated)

}



