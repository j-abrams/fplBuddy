



#' Return the odds for each player to score or register an assist for the next gameweek.
#'
#' @param data Latest odds data for likelihood of scoring and assisting - gw5 at the time of writing
#'
#' @export


# Two separate sources for goals/assists and clean sheets
# Following two functions deal with both, one by one

# fpl_odds_generator_gs
#
# Return odds for anytime goalscorer and anytime assists

fpl_odds_generator_gs <- function(data = odds_gs_gw5) {

  # Data cleansing - several names need updating. Warning - very manual task
  # Note - other names on this list have been excluded. Fix at some point

  odds_gw <- data %>%
    mutate(Name =
             case_when(
               Name == "Jesus" ~ "Fernando de Jesus",
               Name == "Son" ~ "Heung-min",
               Name == "Alvarez" ~ "Álvarez",
               Name == "Richarlison" ~ "de Andrade",
               Name == "Martinelli" ~ "Martinelli Silva",
               Name == "Luis Diaz" ~ "Díaz",
               Name == "Perez" ~ "Pérez",
               Name == "Odegaard" ~ "Ødegaard",
               Name == "Ã˜degaard" ~ "Ødegaard",
               Name == "Ronaldo" ~ "dos Santos Aveiro",
               Name == "Rondon" ~ "Rondón",
               Name == "Bernardo" ~ "Veiga de Carvalho e Silva",
               Name == "Lucas Moura" ~ "Rodrigues Moura da Silva",
               Name == "Coutinho" ~ "Coutinho Correia",
               Name == "Fernandes" ~ "Borges Fernandes",
               Name == "Perisic" ~ "Perišić",
               Name == "Perišic" ~ "Perišić",  # Perisic is broken
               Name == "A.Armstrong" ~ "Armstrong",
               Name == "R..Sessegnon" ~ "Sessegnon",
               Name == "Buendia" ~ "Buendía Stati",
               Name == "Jorginho" ~ "Frello Filho",
               Name == "Andreas" ~ "Hoelgebaum Pereira",
               Name == "Gundogan" ~ "Gündogan",
               Name == "Gross" ~ "Groß",
               Name == "Rodrigo" ~ "Moreno",
               Name == "Cucurella" ~ "Cucurella Saseta",
               Name == "Davies" ~ "Ben Davies",
               Name == "R.Sessegnon" ~ "Sessegnon",
               Name == "Henderson" ~ "J.Henderson",
               TRUE ~ Name
             )
           ) %>%
    # Still more names to fix. This is the problem with messy data, rip

    select(Name, Team, AnytimeAssist, AnytimeGoal) %>%
    mutate(AnytimeAssist = as.numeric(sub("%", "", AnytimeAssist)) / 100) %>%
    mutate(AnytimeGoal = as.numeric(sub("%", "", AnytimeGoal)) / 100) %>%

    # Daniel James and Reece James discrepency
    filter(Name != "James" | Team == "Leeds") %>%
    # Adams, Cook, Johnson,  Phillips - Remove duplicated names
    group_by(Name) %>%
    filter(AnytimeGoal == min(as.numeric(AnytimeGoal))) %>%
    ungroup() %>%

    # Hot Fix
    mutate(Name = str_convert(Name))

  return(odds_gw)
}



