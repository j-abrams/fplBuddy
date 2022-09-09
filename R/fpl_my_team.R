


#' Return a list containing all players selected in your team
#'
#' Nested within "fpl_calculate_xP" to produce the variable "in_my_team"
#'
#' @param user Unique identifier
#'
#' @param squad Either 11 or 15, depending on whether to select all team including or excluding subs
#'
#' @return List.
#'
#' @export


# fpl_get_my_team()

# Return list of players in your team - this function is used as an input elsewhere also
fpl_my_team <- function(user, squad = 11) {

  gw <- fplr::fpl_get_gameweek_current()$id - 1

  # Find team like this
  # Just need to find player id (visit "pick team", then "view gameweek history")
  my_team_temp <- fplscrapR::get_entry_player_picks(user, gw) %>%
    filter(position %in% 1:squad)
  my_team <- as.list(my_team_temp$playername)

  return(my_team)

}






