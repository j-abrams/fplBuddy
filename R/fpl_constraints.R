




#' Constraint, Direction and RHS functions all for defining the boundaries and constraints for the optimisation
#'
#' Used largely behind the scenes within other denoted optimisation functions
#'
#' @export




# fpl_constraints_fn.R

# Individual functions for producing constraints, direction, and rhs one by one.
# Used for setting up our bounded optimisation.
# Constraints include position constraints, budget constraints, and team constraint
# (No more than 3 players from one team allowed - otherwise Liverpool players everywhere)



# Position constraints

# Total Budget = 100
# 2 Goal keepers
# 5 Defenders
# 5 Midfielders
# 3 Strikers
# 11 players total


# Define constraints for optimisation problem
constraint_function <- function(input) {

  # Position constraints - how many players of each position to select
  # 11 players total - non-negotiable
  # Maybe explore possibility of having 15 at some point
  position_constraint <- rbind(
    as.numeric(input$position == "GK"),
    as.numeric(input$position == "DEF"),
    as.numeric(input$position == "DEF"),
    as.numeric(input$position == "MID"),
    as.numeric(input$position == "MID"),
    as.numeric(input$position == "FWD"),
    as.numeric(input$position == "FWD"),
    as.numeric(input$position %in% c("GK", "DEF", "MID", "FWD"))
  )


  # Team constraints - No more than 3 players from any one team
  team_constraint <- rbind(
    as.numeric(input$team == "Arsenal"),
    as.numeric(input$team == "Aston Villa"),
    as.numeric(input$team == "Bournemouth"),
    as.numeric(input$team == "Brentford"),
    as.numeric(input$team == "Brighton"),
    as.numeric(input$team == "Chelsea"),
    as.numeric(input$team == "Crystal Palace"),
    as.numeric(input$team == "Everton"),
    as.numeric(input$team == "Fulham"),
    as.numeric(input$team == "Leeds"),
    as.numeric(input$team == "Leicester"),
    as.numeric(input$team == "Liverpool"),
    as.numeric(input$team == "Man City"),
    as.numeric(input$team == "Man Utd"),
    as.numeric(input$team == "Newcastle"),
    as.numeric(input$team == "Nott'm Forest"),
    as.numeric(input$team == "Southampton"),
    as.numeric(input$team == "Spurs"),
    as.numeric(input$team == "West Ham"),
    as.numeric(input$team == "Wolves")
  )


  # Pull cost, position and team constraints together
  constraint_matrix <- rbind(
    input$now_cost,
    position_constraint,
    team_constraint
  )

  return(constraint_matrix)

}

#######################################


# Define direction for optimisation problem - eg equal to, greater than, less than
direction_function <- function() {

  position_direction <- c(
    "==",    # GK
    "<=",    # Def
    ">=",    # Def
    "<=",    # Mid
    ">=",    # Mid
    "<=",    # Str
    ">=",    # Str
    "=="     # Total players
  )


  team_direction <- c(
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<=",
    "<="
  )


  direction <-
    c("<=",
      position_direction,
      team_direction
    )

  return(direction)

}


# Finally, rhs (right hand side) bounds for optimisation setup
rhs_function <- function(budget) {

  team_rhs <- c(
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3
  )


  position_rhs <- c(
    1,    # GK
    5,    # Def
    3,    # Def
    5,    # Mid
    3,    # Mid
    3,    # Str
    1,    # Str
    11    # Total Players
  )


  rhs <-
    c(budget,  #Cost,
      position_rhs,
      team_rhs # no more than 3 players per team
    )

  return(rhs)

}

