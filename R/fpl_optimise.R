




#' Select the top 11 players subject to expected points, and boundable constraints
#'
#' Constraints are applicable for positions, team, budget, etc.
#'
#' @param input Output from "fpl_calculate_xP()" function
#'
#' @param obj_var Variable to optimise based on (xP by default)
#'
#' @param budget total funds available to spend on first 11 players
#'
#' @return Solution to optimisation problem.
#'
#' @export


# fpl_optimise()
#
# Optimisation function
# Takes constraints, direction and rhs into account

# Testing
#input = players_xP
#obj_var = players_xP$xP

fpl_optimise <- function(input = players_xP, obj_var = players_xP$xP, budget = 83) {

  num.players <- length(input[1])

  obj <- obj_var

  var.types <- rep("B", num.players)

  # Constraints


  constraint_matrix <- constraint_function(input)
  direction <- direction_function()
  rhs <- rhs_function(budget)


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
    filter(sol$solution == 1) %>%
    select(-`sol$solution`)


  return(sol_final)
}








