


#'
#' Decide which transfers should be made for the users' team with this function
#'
#' Transfers argument is flexible to denote how many changes can be made this week
#'
#' @param input Output from "fpl_calculate_xP()" function
#'
#' @param obj_var Variable to optimise based on (xP by default)
#'
#' @param transfers Number of free transfers available to the user
#'
#' @param budget total funds available to spend on first 11 players
#'
#' @export

# fpl_optimise_my_team()
#
# Same optimisation as before, but this time take team into account
# Extra constraint is coded in subject to transfers argument.
# Budget assumption - bench players x 4 cost 17, leaving 83 budget for starting 11...

fpl_optimise_my_team <- function(input, obj_var, budget = 83, transfers = 2) {

  num.players <- length(input[1])

  obj <- obj_var

  var.types <- rep("B", num.players)

  # Constraints


  constraint_matrix <- constraint_function(input)
  direction <- direction_function()
  rhs <- rhs_function(budget)


  # Offer Transfer advice
  # Possibly aim to move this to a similar but separate function
  constraint_matrix <- rbind(
    constraint_matrix,

    as.numeric(input$in_my_team == 1),
    as.numeric(input$position == "DEF"),
    as.numeric(input$position == "MID"),
    as.numeric(input$position == "FWD")
  )

  direction <- c(
    direction,

    ">=",
    "==",
    "==",
    "=="
  )

  rhs <- c(
    rhs,

    11 - transfers,
    nrow(filter(input, in_my_team == 1, position == "DEF")),
    nrow(filter(input, in_my_team == 1, position == "MID")),
    nrow(filter(input, in_my_team == 1, position == "FWD"))
  )



  # Rglpk package for linear programming
  sol <- Rglpk_solve_LP(obj = obj,
                        mat = constraint_matrix,
                        dir = direction,
                        rhs = rhs,
                        types = var.types,
                        max = TRUE)

  sol_final <-
    cbind(input,
          as.data.frame(sol$solution)) %>%
    filter(sol$solution == 1)


  return(sol_final)
}

