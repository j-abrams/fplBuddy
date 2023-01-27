



#' Return the odds for each team to keep a cleansheet for the next gameweek.
#'
#' Table is updated each week when new clean sheet odds become available.
#'
#' @return Dataframe containing clean sheet odds.
#'
#' @export


# fpl_odds_generator_cs()
#
# Odds for each team to keep a clean sheet for the current gameweek
# Two argument - url - website address, and
# xpath - unique identifier for clean sheet table

fpl_odds_generator_cs <- function() {

  url <- "https://www.fantasyfootballreports.com/premier-league-clean-sheet-odds/"
  xpath <- '//*[@id="tablepress-166"]'


  odds <- read_html(url) %>%
    html_nodes(xpath = xpath) %>%
    html_table()


  clean_sheet_odds <- odds[[1]] %>%
    # Data processing
    mutate(clean_sheet_odds = ifelse(nchar(`Clean Sheet Odds`) < 7,
                                     as.numeric(sub("%", "", `Clean Sheet Odds`)) / 100,
                                     sapply((strsplit(
                                       (gsub("%", "", gsub(",", "+", gsub(" ", "", `Clean Sheet Odds`)))), "+", fixed = T)),
                                       function(x) mean(as.numeric(x))) / 100 )) %>%
    select(Team, clean_sheet_odds) %>%
    mutate(Team = case_when(
      Team == "Manchester City" ~ "Man City",
      Team == "Manchester United" ~ "Man Utd",
      Team == "Tottenham" ~ "Spurs",
      Team == "Nottingham" ~ "Nott'm Forest",
      Team == "Brigton" ~ "Brighton",

      # Teams displayed in different formats some weeks.
      # Annoying as this means we need to update the names manually
      Team == "Manch.City" ~ "Man City",
      Team == "Cr.Palace" ~ "Crystal Palace",
      Team == "Wolverhampton" ~ "Wolves",
      Team == "Manch.Utd." ~ "Man Utd",

      Team == "Manchester Utd." ~ "Man Utd",

      Team == "Manch. City" ~ "Man City",
      TRUE ~ Team))

  return(clean_sheet_odds)
}



# Extension
# Access google sheets doc programmatically

# test <- read_html(
#   "https://www.fantasyfootballpundit.com/premier-league-goalscorer-assist-odds/#Premier-League-Anytime-Goalscorer-Odds") %>%
#   html_nodes(xpath = '//*[@id="post-15823"]/div/script[9]')
#
# x <- as.character(test[1][1])
#
# paste0("https", sub('.*https', "", sub('csv.*', "", x)), "csv")

