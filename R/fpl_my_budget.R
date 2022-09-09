


#' Find the budegt available for transfers
#'
#' Calculaed via the selling price column returned from the "fpl_get_api_response()" function
#'
#' @param user Unique identifier
#'
#' @param key fpl account username
#'
#' @param secret fpl account password
#'
#' @return Integer.
#'
#' @export






# Function to Work out funds user has at their disposal right now
# Assume benched players are static - although in practice this may be false.
fpl_my_budget <- function(user, key, secret) {

  my_team_temp <- fpl_get_api_response(key = key, secret = secret, user = user) %>%
    filter(multiplier > 0)

  budget_final <- sum(my_team_temp$selling_price) / 10

  return(budget_final)

}
