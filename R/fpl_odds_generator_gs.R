



#' Return the odds for each player to score or register an assist for the next gameweek.
#'
#' Navigate to fantasyfootballpundit to find the latest odds for this week.
#'
#' @param data Latest odds data for likelihood of scoring and assisting - gw5 at the time of writing
#'
#' @return Dataframe.
#'
#' @export


# Two separate sources for goals/assists and clean sheets
# Following two functions deal with both, one by one

# fpl_odds_generator_gs
#
# Return odds for anytime goalscorer and anytime assists



fpl_odds_generator_gs <- function(data) {

  # Data cleansing - several names need updating. Warning - very manual task
  # Note - other names on this list have been excluded. Fix at some point


  # players_id - used for our joins.
  players_id <- fpl_get_player_all() %>%
    mutate(name = paste(first_name, second_name)) %>%
    select(name, second_name, id)



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

    select(Name, Team, AnytimeAssist, AnytimeGoal, Start) %>%
    mutate(AnytimeAssist = as.numeric(sub("%", "", AnytimeAssist)) / 100) %>%
    mutate(AnytimeGoal = as.numeric(sub("%", "", AnytimeGoal)) / 100) %>%

    # Daniel James and Reece James discrepency
    filter(Name != "James" | Team == "Leeds") %>%
    # Adams, Cook, Johnson,  Phillips - Remove duplicated names
    group_by(Name) %>%
    filter(AnytimeGoal == min(as.numeric(AnytimeGoal))) %>%
    ungroup() %>%

    # Hot Fix
    mutate(Name = str_convert(Name)) %>%


    # New Lookup via id
    mutate(second_name = str_convert(Name)) %>%
    left_join(players_id, by = "second_name") %>%
    select(-name, -second_name) %>%
    mutate(id = case_when(
      Name == "Darwin" ~ 297,
      Name == "Alvarez" ~ 319,
      Name == "Diaz" ~ 293,
      Name == "Carvalho" ~ 296,
      Name == "Jimenez" ~ 476,
      Name == "Antony" ~ 609,
      Name == "Gro?" ~ 104,
      Name == "Mitrovic" ~ 210,
      Name == "Podence" ~ 483,
      Name == "Guedes" ~ 579,
      Name == "Neto" ~ 486,
      Name == "Gundogan" ~ 300,
      Name == "Hwang" ~ 481,
      Name == "Odegaard" ~ 7,
      Name == "Peri?ic" ~ 448,
      Name == "Adama" ~ 491,
      Name == "Joelinton" ~ 371,
      Name == "Sinisterra" ~ 508,
      Name == "Fabio Vieira" ~ 25,
      Name == "Willian" ~ 614,
      Name == "Bruno Guimaraes" ~ 374,
      Name == "Buendia Stati" ~ 42,
      Name == "Sarmiento" ~ 119,
      Name == "Neves" ~ 480,
      Name == "Aribo" ~ 512,
      Name == "Rondon" ~ 177,
      Name == "Almiron" ~ 369,
      Name == "Hojbjerg" ~ 433,
      Name == "Matheus" ~ 589,
      Name == "Emerson Royal" ~ 445,
      Name == "S.Armstrong" ~ 405,
      Name == "N.Williams" ~ 295,
      Name == "J.Henderson" ~ 275,
      Name == "Fornals" ~ 469,
      Name == "Caicedo" ~ 120,
      Name == "Rodri" ~ 315,
      Name == "Moutinho" ~ 503,
      Name == "Fred" ~ 331,
      Name == "S.Longstaff" ~ 370,
      Name == "Fabinho" ~ 282,
      Name == "Casemiro" ~ 593,
      Name == "Dembele" ~ 74,
      Name == "Estupinan" ~ 586,
      Name == "Ait-Nouri" ~ 487,
      Name == "Renan Lodi" ~ 602,
      Name == "Ben Davies" ~ 432,
      Name == "Roca" ~ 245,
      Name == "Van Dijk" ~ 280,
      Name == "Roerslev" ~ 90,
      Name == "Cedric" ~ 1,
      Name == "Perez" ~ 260,
      Name == "Palhinha" ~ 220,
      Name == "Firpo" ~ 239,
      Name == "Vinagre" ~ 531,
      Name == "C.Doucoure" ~ 514,
      Name == "Dalot" ~ 342,
      Name == "Sambi" ~ 18,
      Name == "Ake" ~ 308,
      Name == "Dias" ~ 312,
      Name == "Semedo" ~ 482,
      Name == "Thiago Silva" ~ 128,
      Name == "Schar" ~ 366,
      Name == "Lerma" ~ 64,
      Name == "Gabriel" ~ 16,
      Name == "Kouyate" ~ 583,
      Name == "Douglas Luiz" ~ 46,
      Name == "Toti" ~ 489,
      Name == "Sanchez" ~ 435,
      Name == "Martinez" ~ 533,
      Name == "Sergio Gomez" ~ 587,
      Name == "Van Hecke" ~ 544,
      Name == "Lyanco" ~ 413,
      Name == "Emerson" ~ 545,
      Name == "Soumare" ~ 269,
      TRUE ~ as.numeric(id)
    )) %>%
    #mutate(id = as.character(id)) %>%
    filter(!is.na(id)) %>%
    #filter(!is.na(Name)) %>%
    group_by(Name) %>%
    filter(id == max(id)) %>%
    filter(AnytimeGoal == max(AnytimeGoal)) %>%
    filter(AnytimeAssist == max(AnytimeAssist)) %>%
    ungroup() %>%
    distinct() %>%
    mutate(AnytimeGoal = AnytimeGoal * as.numeric(sub("%", "", Start))) %>%
    mutate(AnytimeAssist = AnytimeAssist * as.numeric(sub("%", "", Start))) %>%
    select(-Start)
    #https://stackoverflow.com/questions/8329059/how-to-convert-character-of-percentage-into-numeric-in-r

  #filter(n() > 1)

  return(odds_gw)

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





# Some useful code to overwrite sys_data if you ever need to - for later reference.
#https://github.com/r-lib/usethis/issues/1512

# my_new_env <- new.env(hash = FALSE)
#
# # load current internal data into this new environment
# load("R/sysdata.rda", envir = my_new_env)
#
# # add or replace some objects
# my_new_env$odds_gs_gw6 <- odds_gs_gw6
#
# # save the environment as internal package data
# save(list = names(my_new_env),
#      file = "R/sysdata.rda",
#      envir = my_new_env)



