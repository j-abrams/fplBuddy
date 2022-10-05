



fpl_get_gw_joined <- function(players, gameweek) {

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


  return(gw_joined)

}

