# Run before each session
library(RSQLite)
library(dplyr)
library(ggplot2)
conn <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db") 
# Table name is statcast_data

# Workspace
pitch_count_2018 <- dbGetQuery(conn, "SELECT COUNT(*) FROM statcast_data WHERE game_year == 2018")

# ORDER BY RANDOM() is SQLite specific. This query focuses only on batted balls and not swinging strikes/called strikes.
query <- paste0("SELECT pitcher, pitch_type, zone, estimated_woba_using_speedangle AS xwoba, p_throws AS p_side, ",
                "stand AS b_side, woba_value AS woba FROM statcast_data WHERE game_year == 2018 AND xwoba IS NOT NULL")
pitch_data <- dbGetQuery(conn, query)

# Filtered out any pitches outside the strike zone.
pitch_split_data <- pitch_data %>% filter(zone %in% c(1:9))

dbDisconnect(conn)

# Data splits by batter and hitter sides
lhp_on_left <- pitch_split_data %>%
  filter(p_side == "L", b_side == "L")
lhp_on_right <- pitch_split_data %>%
  filter(p_side == "L", b_side == "R")
rhp_on_left <- pitch_split_data %>%
  filter(p_side == "R", b_side == "L")
rhp_on_right <- pitch_split_data %>%
  filter(p_side == "R", b_side == "R")

# Format and group the data by pitch/location combinations. Also creates a column for number of pitches to filter
# combinations with low sample sizes. Then, the data is arranged in ascending order by xwoba (best combos at top).
source("format.r")
lhp_lb_combo <- format(lhp_on_left, 200)
lhp_rb_combo <- format(lhp_on_right, 300)
rhp_rb_combo <- format(rhp_on_right, 300)
rhp_lb_combo <- format(rhp_on_left, 300)

# Add columns for each table indicating the batter and pitcher hand for future table joining.

source("add_bat_pitch_sides.r")

lhp_lb_combo <- add_bat_pitch_sides(lhp_lb_combo, "L", "L")
lhp_rb_combo <- add_bat_pitch_sides(lhp_rb_combo, "L", "R")
rhp_lb_combo <- add_bat_pitch_sides(rhp_lb_combo, "R", "L")
rhp_rb_combo <- add_bat_pitch_sides(rhp_rb_combo, "R", "R")

# Best combos

lhp_lb_combo
lhp_rb_combo
rhp_lb_combo
rhp_rb_combo

# Worst combos

lhp_lb_combo %>% arrange(desc(avg_xwoba))
lhp_rb_combo %>% arrange(desc(avg_xwoba))
rhp_lb_combo %>% arrange(desc(avg_xwoba))
rhp_rb_combo %>% arrange(desc(avg_xwoba))



# Which pitchers pitch best in the zone amongst their arm side against a certain batter side?
# First, I combined the pitch/zone combination data for each batter/pitcher side instance with the original
# pitch by pitch data I pulled from my SQLite db that contains pitcher IDs.

source("join_combo_pitcherID.r")

lhp_lb_data <- join_combo_pitcherID(pitch_split_data, lhp_lb_combo)
lhp_rb_data <- join_combo_pitcherID(pitch_split_data, lhp_rb_combo)
rhp_lb_data <- join_combo_pitcherID(pitch_split_data, rhp_lb_combo)
rhp_rb_data <- join_combo_pitcherID(pitch_split_data, rhp_rb_combo)

# Next, I group by pitcher ID to find their average xwobas based on their pitch type and location combinations.

source("group_by_pitcherID.r")

lhp_lb_leaders <- group_by_pitcherID(lhp_lb_data, 150)
lhp_rb_leaders <- group_by_pitcherID(lhp_rb_data, 500)
rhp_lb_leaders <- group_by_pitcherID(rhp_lb_data, 500)
rhp_rb_leaders <- group_by_pitcherID(rhp_rb_data, 500)

# Best pitcher leaderboards

lhp_lb_leaders
lhp_rb_leaders
rhp_lb_leaders
rhp_rb_leaders

# Worst pitcher leaderboards

lhp_lb_leaders %>% arrange(desc(avg_xwoba))
lhp_rb_leaders %>% arrange(desc(avg_xwoba))
rhp_lb_leaders %>% arrange(desc(avg_xwoba))
rhp_rb_leaders %>% arrange(desc(avg_xwoba))

# Now, I will create an overall pitcher leaderboard, combining the splits tables into 1, match it to the
# original table, and average the overall xwobas for pitchers from facing both righties and lefties.

# First I combined the splits data vertically.
all_pitch_combos <- bind_rows(lhp_lb_combo, lhp_rb_combo, rhp_lb_combo, rhp_rb_combo)

# Next, I joined it with the original pitch data. Now, all_pitch_combos contains the same original pitch
# by pitch data, except with the average xwoba associated with the pitch combos
all_pitch_combos <- join_combo_pitcherID(pitch_split_data, all_pitch_combos)

# Finally, I group by pitcher IDs and arrange by best overall xwobas.

pitch_leaders <- group_by_pitcherID(all_pitch_combos, 100)

# Overall pitch leaderboard

pitch_leaders

# Overall worst leaderboard

pitch_leaders %>% arrange(desc(avg_xwoba))