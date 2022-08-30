


#' Return a list containing all players selected in your team
#'
#' Converts fields such as average points, ict rating etc. into usable indexes for generating predicted points
#'
#' @param user Unique identifier
#'
#' @param squad Either 11 or 15, depending on whether to select all team including or excluding subs
#'
#' @param gw Numeric value, should be set to the most recently passed gameweek (gw4 at time of writing)S
#'
#' @export


# fpl_get_my_team()

# Return list of players in your team - this function is used as an input elsewhere also
fpl_my_team <- function(user, squad = 11, gw = 4) {

  # Find team like this
  # Just need to find player id (visit "pick team", then "view gameweek history")
  my_team_temp <- fplscrapR::get_entry_player_picks(user, gw) %>%
    filter(position %in% 1:squad)
  my_team <- as.list(my_team_temp$playername)

  return(my_team)

}





# Function to Work out funds user has at their disposal right now
# Assume benched players are static - although in practice this may be false.
fpl_my_budget <- function(user, players_collated_final, gw = 4) {

  # Initial budget
  budget <- fplscrapR::get_entry(user)$last_deadline_value / 10

  # Isolate cost of bench players only to calculate funds available for squad players
  my_team_temp <- fplscrapR::get_entry_player_picks(user, gw) %>%
    filter(position %in% 12:15) %>%
    left_join(players_collated_final, by = c("playername" = "name")) %>%
    select(playername, now_cost)

  # Final amount
  budget_final <- budget - sum(my_team_temp$now_cost)

  return(budget_final)

}


