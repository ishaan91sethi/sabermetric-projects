library(ggplot2)
library(RSQLite)
library(dplyr)
library(readr)
library(riem)
library(stringr)
library(lubridate)

# I first retrieved the ballpark ID and day/night indicator for Nola's starts from the 2015-2018 retrosheet
# gamelogs.
conn <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\retrosheet_gamelogs.db")
query <- "SELECT Date, DayNight, ParkID FROM gamelogs WHERE HomeStartingPitcherID == 'nolaa001' OR 
  VisitorStartingPitcherID == 'nolaa001'" 
starts_location_time <- dbGetQuery(conn, query)
dbDisconnect(conn)

# Next, I read in the ballpark info and joined it with the above dataframe so I could get the city and state
# for each of Nola's starts.
park_info <- read_csv("parkinfo.csv")
park_info <- park_info %>%
  select(ParkID = PARKID, CITY, STATE)
starts_location_time <- left_join(starts_location_time, park_info, by = "ParkID")

# All the cities that Nola pitched in.
cities <- starts_location_time %>%
  group_by(CITY, STATE) %>%
  summarize(count = n()) %>%
  select(-count)

# I used this list of cities to find airport codes that will be used with the Riem package to acquire
# weather data for Nola's starts.

# Read in Fangraphs gamelog data for Nola
nola_data <- read_csv("Nola_gamelog.csv")

# Take out cumulative statistics row
nola_data <- nola_data[-1,]


# Finds the list of away opponents and the number of starts at these locations
opponent_list <- nola_data %>%
  filter(substr(Opp, 1, 1) == "@") %>%
  group_by(Opp) %>%
  summarize(count = n())

source("omit_at.R")

omit_at_vector <- Vectorize(omit_at)

# Creates a column denoting if away game or not and uses omit_at_vector to remove the "@" from the 
# Opp variable. Also creates a home team column with team codes and changes Date variable from a character
# to a date (using ymd() from lubridate package).

nola_data <- nola_data %>%
  mutate(away_game = ifelse(str_sub(Opp, 1, 1) == "@", "Y", "N"), Opp = omit_at_vector(Opp),
         home_team = ifelse(away_game == "Y", Opp, "PHI"), Date = ymd(Date)) %>%
  select(-Team) %>%
  select(Date, Opp, away_game, home_team, everything())

source("airport_codes.R")
# Function gets tibble to be inner joined with nola_data. The tibble adds airport codes for the riem package.
airport_code_table <- airport_codes()

# Adds airport_codes based on the location of the game using left_join().
nola_data <- nola_data %>%
  left_join(airport_code_table, by = c("home_team" = "Team"))

# Select the date and time of game columns from starts_location_time and store them in a seperate dataframe.
date_start_times <- starts_location_time %>%
  select(Date, DayNight) %>%
  mutate(Date = ymd(Date))

# Adds DayNight variable to nola_data.
nola_data <- nola_data %>%
  left_join(date_start_times, by = c("Date"))

# Filter out 2019 data until retrosheet 2019 gamelogs are released
nola_data2015_2018 <- nola_data %>%
  filter(year(Date) != 2019)

source("get_weather.R")

# Add weather data to nola_data2015_2018
nola_data_with_weather <- get_weather(nola_data2015_2018)


# Next, I will gather the average curveball spin rate from each of his starts.
# Curveball spin. CSV on baseball savant says release_spin is the column name, but it's actually release_spin_rate
conn2 <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db")
query2 <- "SELECT game_date, pitch_type, release_spin_rate FROM statcast_data WHERE pitcher == 605400"
pitch_spin_data <- dbGetQuery(conn2, query2)
dbDisconnect(conn2)

# Nola pitch types
pitch_spin_data %>%
  group_by(pitch_type) %>%
  summarize(count = n())

# Find average curveball spin in each start. I am grouping his knuckle curves and his regular curveballs together.
# I converted game_date into the Date data type from character for joining with the overall dataframe.
curveball_spin_data <- pitch_spin_data %>%
  filter(pitch_type == "KC" | pitch_type == "CU") %>%
  select(-pitch_type) %>%
  group_by(game_date) %>%
  summarize(avg_curve_spin_rate = mean(release_spin_rate, na.rm = TRUE), count = n()) %>%
  mutate(Date = as.Date(game_date)) %>%
  select(-game_date)

# Now, I will add my curveball spin data to my overall nola_data_with_weather, storing the result in a new variable
nola_data_with_weather_curve <- nola_data_with_weather %>%
  left_join(curveball_spin_data, by = c("Date"))

# I was curious to see how similar his curveball was to his knuckle curve (Is it just a classification quirk
# or are they two distinct pitches?)
conn2 <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db")
query2 <- "SELECT pitch_type, release_speed, release_spin_rate, pfx_x, pfx_z FROM statcast_data WHERE 
  pitcher == 605400 AND (pitch_type == 'CU' OR pitch_type == 'KC')"
curve_comparison_data <- dbGetQuery(conn2, query2)
dbDisconnect(conn2)

avg_curve_data <- curve_comparison_data %>%
  group_by(pitch_type) %>%
  summarize(avg_velo = mean(release_speed, na.rm = TRUE), avg_spin = mean(release_spin_rate, na.rm = TRUE),
            avg_vert_mvmt = mean(pfx_z, na.rm = TRUE), avg_hor_mvmt = mean(pfx_x, na.rm = TRUE), count = n())

# Sample graph
x <- c(1:5)
GSv2 <- seq(1, 150, by = 30)
df <- data.frame(x, y)
ggplot(data = df, mapping = aes(x = x, y = x, color = GSv2)) +
  geom_point(size = 5) +
  xlab("Temperature in Farenheight") +
  ylab("Relative Humidity") +
  ggtitle("Sample graph") +
  theme(plot.title = element_text(hjust = 0.5))

# Graphs visualizing the results.
ggplot(data = nola_data_with_weather, aes(x = tmpf, y = relh, color = GSv2)) +
  geom_point(size = 5) +
  xlab("Temperature in Fahrenheight") +
  ylab("Relative Humidity") +
  ggtitle("The Effect of Temperature and Humidity on Game Score") +
  theme(plot.title = element_text(hjust = 0.5)) # Centers title 
  
  
ggplot(data = nola_data_with_weather, aes(x = tmpf, y = GSv2)) +
  geom_point() +
  geom_smooth() +
  xlab("Temperature in Fahrenheight") +
  ylab("Game Score (Version 2)") +
  ggtitle("The Effect of Temperature on Game Score") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = nola_data_with_weather, aes(x = relh, y = GSv2)) +
  geom_point() +
  geom_smooth() +
  xlab("Relative humidity") +
  ylab("Game Score (Version 2)") +
  ggtitle("The Effect of Humidity on Game Score") +
  theme(plot.title = element_text(hjust = 0.5))


# Same graphs with curveball spin rate instead of game score
ggplot(data = nola_data_with_weather_curve, aes(x = tmpf, y = relh, color = avg_curve_spin_rate)) +
  geom_point(size = 5) +
  xlab("Temperature in Fahrenheight") +
  ylab("Relative Humidity") +
  ggtitle("The Effect of Temperature and Humidity on Curveball Spin Rate") +
  theme(plot.title = element_text(hjust = 0.5)) #+
  #scale_size_continuous(range = c(5, 5))

ggplot(data = nola_data_with_weather_curve, aes(x = tmpf, y = avg_curve_spin_rate)) +
  geom_point() +
  geom_smooth() +
  xlab("Temperature in Fahrenheight") +
  ylab("Curveball Spin Rate") +
  ggtitle("The Effect of Temperature on Curveball Spin Rate") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = nola_data_with_weather_curve, aes(x = relh, y = avg_curve_spin_rate)) +
  geom_point() +
  geom_smooth() +
  xlab("Relative humidity") +
  ylab("Curveball Spin Rate") +
  ggtitle("The Effect of Humidity on Curveball Spin Rate") +
  theme(plot.title = element_text(hjust = 0.5))


# For introduction: Analyzing monthly stretches
nola_data2 <- nola_data %>%
  mutate(month = month(Date))
nola_data_by_month <- nola_data2 %>%
  group_by(month) %>%
  summarize(avg_fip = mean(FIP))

ggplot(data = nola_data_by_month, aes(x = month, y = avg_fip)) +
  geom_col() +
  xlab("Month") +
  ylab("FIP") +
  ggtitle("Aaron Nola's Career FIP per month") +
  theme(plot.title = element_text(hjust = 0.5))







# Cross validation with wunderground
nola_data_with_weather %>%
  select(Date, tmpf, DayNight, home_team) -> temps
print(tbl_df(temps), n=93)
