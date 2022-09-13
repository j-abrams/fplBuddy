


#' Retrieve API data directly from the fpl website
#'
#' Requires username and password authentication to retrieve more granular information
#'
#' @param key Email for your fpl account
#'
#' @param secert Password for fpl account
#'
#' @param user Unique user id
#'
#' @export





# Make use of reticulate package to run python code
# Code returns the authenticated reponse required to scrape fpl data using the api
# https://medium.com/@bram.vanherle1/fantasy-premier-league-api-authentication-guide-2f7aeb2382e4

# Put this in a function that accepts three arguments
# user (user id), key (login), secret (password) for authentication.



# Finally got this to work using global assignment,
# and refernecing the r object "r.py_key" within the accompanying python script.
fpl_get_api_response <- function(key, secret, user) {

  py_key <<- reticulate::r_to_py(key)
  py_secret <<- reticulate::r_to_py(secret)
  py_user <<- reticulate::r_to_py(user)

  # Run this attached python script to get what we need
  reticulate::source_python("fpl.py")

  #res <- res
  res <- fromJSON(res$text)$picks


  return(res)
}


