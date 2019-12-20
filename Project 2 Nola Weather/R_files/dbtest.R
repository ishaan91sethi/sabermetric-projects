# Test file for statcast database

library(RSQLite)
# Only have statcast date from 2017 and 2018
conn <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db")
query <- "SELECT game_date FROM statcast_data" 
dates <- dbGetQuery(conn, query)
dbDisconnect(conn)

dates %>%

typeof(dates[1,])

dates2 <- dates %>%
  mutate(year = substr((dates[[1]]), 1, 4)) %>%
  filter(year != 2017, year != 2018)
  # group_by(year) %>%
  # summarize(count = n()) %>%
  # arrange(desc(year))


dates3 <- dates2 %>%
  group_by(game_date) %>%
  summarize(count = n())