



#' Return the odds for each team to keep a cleansheet for the next gameweek.
#'
#' @param url Web link where odds table is scraped from
#'
#' @param xpath Unique identifier for the clean sheet probability table
#'
#' @export


# fpl_odds_generator_cs()
#
# Odds for each team to keep a clean sheet for the current gameweek
# Two argument - url - website address, and
# xpath - unique identifier for clean sheet table

fpl_odds_generator_cs <- function(url, xpath) {

  odds <- read_html(url) %>%
    html_nodes(xpath = xpath) %>%
    html_table()

  clean_sheet_odds <- odds[[1]] %>%
    # Data processing
    mutate(clean_sheet_odds = as.numeric(sub("%", "", `Clean Sheet Odds`)) / 100 ) %>%
    select(Team, clean_sheet_odds) %>%
    mutate(Team = case_when(
      Team == "Manchester City" ~ "Man City",
      Team == "Manchester United" ~ "Man Utd",
      Team == "Tottenham" ~ "Spurs",
      Team == "Nottingham" ~ "Nott'm Forest",
      Team == "Brigton" ~ "Brighton",

      Team == "Manch.City" ~ "Man City",
      Team == "Cr.Palace" ~ "Crystal Palace",
      Team == "Wolverhampton" ~ "Wolves",
      Team == "Manch.Utd." ~ "Man Utd",


      TRUE ~ Team))

  return(clean_sheet_odds)
}



# Access google sheets doc programmatically
# test <- read_html(
#   "https://www.fantasyfootballpundit.com/premier-league-goalscorer-assist-odds/#Premier-League-Anytime-Goalscorer-Odds") %>%
#   html_nodes(xpath = '//*[@id="post-15823"]/div/script[9]')
#
# x <- as.character(test[1][1])
#
# paste0("https", sub('.*https', "", sub('csv.*', "", x)), "csv")
