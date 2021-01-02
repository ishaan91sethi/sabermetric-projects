# Things to correct/control for: pitch count, type of pitch, location, and batter and hitter side. 
# Instead of home runs, maybe barrels?
# Variables to consider: velocity, horizontal, and vertical movement. Only pitches in the strikezone.
# Different model for each pitch type, each count, and each location. May want to go one pitch at a time.
# launch_speed_angle variable for barrels.

# Note: Statcast pitch speeds only for 2017 onwards, according to csv info page
library(RSQLite)
library(dplyr)
library(ggplot2)


conn <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db") 
# Table name is statcast_data

pitch_data <- dbGetQuery(conn, "SELECT pitch_type, game_year, release_speed, zone, p_throws, stand, 
                         pfx_x AS hor_mov, pfx_z AS vert_mov, launch_speed_angle, balls, strikes,
                         description, release_pos_x AS x_pos, release_pos_z as z_pos, plate_x, plate_z
                         FROM statcast_data WHERE game_year NOT IN (2015, 2016)")

dbDisconnect(conn)
head(pitch_data)
nrow(pitch_data)
# x_pos and z_pos refer to release position of the ball, plate_x and plate_z are the position of the
# ball when it crosses home plate from the catcher's position.

table(pitch_data$description)
table(pitch_data$pitch_type)
table(pitch_data$game_year)
# Vector of valid pitch types, left out blank, PO (pitch out probably), UN, null
# Some quick abbrev. meanings: SC-Screwball, FS-Splitter, FC-Cutter, FT-Two Seam, CS-Slow Curve?,
# FO - Forkball
pitch_types <- c("CH", "CS", "CU", "EP", "FC", "FF", "FO", "FS", "FT", "KC", "KN", "SC",
                 "SI", "SL")

# Vector with pitch outcomes that involves swings
swing_outcomes <- c("foul", "foul_tip", "hit_into_play", "hit_into_play_no_out", "hit_into_play_score",
                    "swinging_strike", "swinging_strike_blocked")


pitch_data %>%
  filter(!(pitch_type %in% pitch_types)) %>%
  group_by(game_year) %>%
  summarize(count = n())
# Weird pitch types are fairly evenly distributed, except 2020 has far less

# Add barrel variable and turn some variables into factors for future. Also create a swing_t_f variable
# based on vector swing_outcomes above and eliminate 4 ball counts
pitch_data2 <- pitch_data %>%
  mutate(barrel = ifelse(launch_speed_angle == 6, 1, 0), pitch_type = as.factor(pitch_type),
         p_throws = as.factor(p_throws), stand = as.factor(stand), 
         swing_t_f = description %in% swing_outcomes, 
         count_factor = as.factor(paste(balls, strikes, sep = "-"))) %>%
  filter(pitch_type %in% pitch_types, balls < 4) %>%
  as_tibble()

# Substitue missing values for barrel variable as 0 (pitches could've been fouled, taken, etc. and
# not assigned a statcast batted ball classification)
pitch_data2[which(is.na(pitch_data2$barrel)), "barrel"] <- 0

# Barrels/swings by count
pitch_data2 %>%
  filter(swing_t_f) %>%
  group_by(balls, strikes) %>%
  summarize(barrel_rate = mean(barrel)) %>%
  arrange(desc(barrel_rate))

# Are the differences in certain counts due to the hitter swinging freer and better in hitter counts
# or the pitcher giving easier pitches in certain counts? Probably both. 



pitch_data_zone <- pitch_data2 %>%
  filter(zone %in% 1:9)

pitch_data_zone %>%
  group_by(pitch_type) %>%
  summarize(barrel_rate = mean(barrel), count = n()) %>%
  arrange(desc(barrel_rate))


pitch_data_zone %>%
  filter(pitch_type == "FF", !is.na(zone)) %>%
  group_by(zone, p_throws, stand) %>%
  summarize(barrel_rate = mean(barrel), count = n()) %>%
  arrange(count)

# Barrels/swings by count, except this time with only pitches in zone
pitch_data_zone %>%
  filter(swing_t_f) %>%
  group_by(balls, strikes) %>%
  summarize(barrel_rate = mean(barrel)) %>%
  arrange(desc(barrel_rate))

pitch_data_zone %>%
  filter(swing_t_f, zone == 5) %>%
  group_by(balls, strikes) %>%
  summarize(barrel_rate = mean(barrel)) %>%
  arrange(desc(barrel_rate))
# Even when controlling  for swing frequency (by only including pitches that were swung on) and location,
# which is the variable I think count most affects for pitchers (3-0 count, more pitches down the middle), 
# barrel_swing rate (barrels/swings) is still affected by count. It's possible that pitch quality is still
# worse (less movement / velocity perhaps), but it seems like hitters are just better at hitting barrels 
# in certain counts. In other words, the effects of the hitting count are likely due both to hitters giving
# up more hittable pitches and hitters swinging in a way that is freer. Hitters can afford to swing in a 
# more aggressive, harder way in certain counts as well, which is definitely a major factor.

pitch_data2 %>%
  mutate(middle = zone == 5) %>%
  group_by(balls, strikes) %>%
  summarize(middle_rate = mean(middle, na.rm = TRUE)) %>%
  arrange(desc(middle_rate))
