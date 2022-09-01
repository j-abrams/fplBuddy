


# Tomorrow ----

# TODO: Write a function to do this automatically tomorrow.
# TODO: Monitor predictions vs actual observed scores
# TODO: Pull in odds data for gw 6
# TODO: Try to move data around in the package to hide from user...
# TODO: Write function to retrieve archived odds and clean sheet data for gameweek, a user specified argument


test <- fplscrapR::get_player_details()

gameweeks <- list("gw1", "gw2", "gw3", "gw4", "gw5")

for (i in 1:length(gameweeks)) {
  temp <- test %>%
    group_by(playername) %>%
    filter(fixture <= i * 10 & fixture > (i * 10) - 10) %>%
    mutate(was_home = as.numeric(was_home)) %>%
    select(name = playername, total_points, ict_index) %>%
    ungroup()
  assign(gameweeks[[i]], temp)
}




#######################################

# Save Past predictions - Opportunity to do some back casting here
# Odds data only available from gameweek 4 onwards

# 1. Collate
gw_collated <- rbind(gw1, gw2, gw3) %>%
  group_by(name) %>%
  summarise(total_points = sum(total_points),
            ict_index = sum(as.numeric(ict_index)))

# 2. Join
gw4_test <- fpl_get_player_all() %>%
  mutate(name = paste(first_name, second_name)) %>%
  left_join(gw_collated, by = "name") %>%
  mutate(total_points = total_points.y,
         ict_index = ict_index.y) %>%
  select(-c(name, ict_index.x, total_points.x, ict_index.y, total_points.y))



period <- fplBuddy::fpl_fixtures(4, 4)

# 3. Get index and xP
players_index <- fpl_calculate_predictors(players = gw4_test, period = period, strength_index = 1, gw = 4,
                                          odds_gs = odds_gs_gw4, odds_cs = odds_cs_gw4)

gw4_players_xP_index <- fpl_calculate_xP(players_index, predictors_indexes, user = "6238967", gw = 4)
gw4_players_xP_odds <- fpl_calculate_xP(players_index, predictors_odds, user = "6238967", gw = 4)


write.csv(gw4_players_xP_index, "data/Previous week predictions/gw4_players_xP_index.csv")





#####################################################

# Comparisons ----

gw4 <- gw4 %>%
  mutate(name = str_convert(name))

gw4_players_xp <- read.csv("data/Previous week predictions/gw4_players_xP_odds.csv") %>%
  full_join(read.csv("data/Previous week predictions/gw4_players_xP_index.csv"), by = "name") %>%
  mutate(name = str_convert(name)) %>%
  full_join(gw4, by = "name") %>%
  select(-ict_index) %>%
  select(name, "xP_odds" = "xP.x", "xP_index" = "xP.y", total_points) %>%
  filter(!is.na(total_points) & !is.na(xP_index))


# Correlations
cor(gw4_players_xp$xP_odds, gw4_players_xp$total_points)
cor(gw4_players_xp$xP_index, gw4_players_xp$total_points)

# Odds metric vs Index metric correlation? Pretty high
cor(gw4_players_xp$xP_index, gw4_players_xp$xP_odds)





##########################################################################



gw5 <- gw5 %>%
  mutate(name = str_convert(name))

gw5_players_xp <- read.csv("data/Previous week predictions/gw5_players_xP_odds.csv") %>%
  full_join(read.csv("data/Previous week predictions/gw5_players_xP_index.csv"), by = "name") %>%
  mutate(name = str_convert(name)) %>%
  full_join(gw5, by = "name") %>%
  select(-ict_index) %>%
  select(name, "xP_odds" = "xP.x", "xP_index" = "xP.y", total_points) %>%
  filter(!is.na(total_points) & !is.na(xP_index))



# Correlations
cor(gw5_players_xp$xP_odds, gw5_players_xp$total_points)
cor(gw5_players_xp$xP_index, gw5_players_xp$total_points)

# Odds metric vs Index metric correlation? Pretty high
cor(gw5_players_xp$xP_index, gw5_players_xp$xP_odds)


