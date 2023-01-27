


# Transfer suggestion function.

# 1. Give the function the name of a player
# 2. Offer three suggested replacements
# - Based off predicted points next 3 weeks (index method)
# - same position as replaced player



fpl_transfer_replacements <- function(players = player_details, player_name, gameweek = 13) {

  gw_joined <- fpl_get_gw_joined(players, gameweek)

  period <- fpl_fixtures(gameweek, gameweek + 2)

  i <- 0
  while (!exists("odds_gs")) {
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
  rm(skip_to_next)

  # Acquiring odds_gs and odds_cs - saved as data files within the package
  odds_gs <- fpl_odds_generator_gs(odds_gs) %>%
    mutate(gameweek = gameweek)

  odds_cs <- eval(parse(text = paste0("fplBuddy::odds_cs_gw", gameweek - i))) %>%
    mutate(gameweek = gameweek)


  players_index <- fpl_calculate_predictors(players = gw_joined,
                                            period = period, weight = 1,
                                            odds_gs = odds_gs,
                                            odds_cs = odds_cs)

  pos <- head(filter(players_index, name %in% player_name), 1)$position
  cost <- head(filter(players_index, name %in% player_name), 1)$now_cost


  players_index <- players_index %>%
    filter(position == pos,
           now_cost <= cost + 1)

  gw_players_xP_index <- fpl_calculate_xP(
    players_index, predictors_indexes, user = "6238967", gw = gameweek - i - 1) %>%
    group_by(name) %>%
    mutate(total_xP = sum(xP)) %>%
    arrange(desc(total_xP)) %>%
    select(-in_my_team, -total_xP) %>%
    head(9) %>%

    left_join(period, by = c("gameweek", "team")) %>%
    mutate(fixture = ifelse(was_home == 1,
                            paste0(xP, " - ", opponent, " - H"),
                            paste0(xP, " - ", opponent, " - A"))) %>%

    select(-xP, -opponent, -was_home, -strength_overall_home, -strength_overall_away, -id, -next_match) %>%

    pivot_wider(names_prefix = "gameweek_", names_from = gameweek, values_from = fixture)

  return(gw_players_xP_index)
}



#rm(list = Filter(exists, c("odds_gs", "odds_cs")))
#fpl_transfer_replacements(player_name = "James Maddison", gameweek = 13)

# TODO: List fixtures in this period alongside predicted points for each week




