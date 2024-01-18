# Scrapes statcast pitch by pitch data for the 2017 and 2018 seasons.

require(readr)
require(dplyr)
require(xml2)
require(magrittr)
require(RSQLite)
source("scrape_statcast_savant_pitcher_date.r")

dates_reduced <- read_csv("C:\\Users\\Ishaan\\Documents\\R\\sabermetric-projects\\Project 1 Combos\\data_files
                          \\dates_statcast_build.csv", header=TRUE)
x2017season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2017)
x2018season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2018)

x2017data <- x2017season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2017 <- x2017data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2018data <- x2018season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2018 <- x2018data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

statcast_bind <- rbind(x2017data, x2018data)

statcast_bind$game_date <- as.character(statcast_bind$game_date)

statcast_bind <- statcast_bind %>%
  arrange(game_date)

statcast_bind <- statcast_bind %>%
  filter(!is.na(game_date))

conn <- dbConnect(SQLite(), "statcast.db")
dbWriteTable(conn, "statcast_data", statcast_bind)
dbDisconnect(conn)

