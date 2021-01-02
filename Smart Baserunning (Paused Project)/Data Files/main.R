library(Lahman)
library(tidyverse)
library(RSQLite)
library(splines)

Master_handedness <- Master[, c("playerID", "nameFirst", "nameLast", "bats")]
head(Master_handedness$playerID)


# Read in csv files
setwd("Data Files")

id_map <- read_csv("id_map.csv")
id_map <- id_map[, c("IDPLAYER", "IDFANGRAPHS")]

# Add id_map to Master_handedness
Master_handedness <- left_join(Master_handedness, id_map, by = c("playerID" = "IDPLAYER"))
Master_handedness[, "IDFANGRAPHS"] <- as.numeric(Master_handedness[, "IDFANGRAPHS"])

league_opport <- read_csv("league_opportunity_data.csv")
head(league_opport)

sb_cs_weights <- read_csv("sb_cs_values.csv") %>%
  filter(Season >= 2015)
head(sb_cs_weights)

# Min. 250 PA
sprint_speed_data <- read_csv("sprint_speed_data.csv") %>%
  select(-X6)
head(sprint_speed_data)

# Helper functions to extract first and last names
get_first_name <- function(full_name) {
  if (str_count(full_name, " ") == 2 & !str_detect(full_name, "Jr.")) {
    return(str_extract(full_name, "\\S+ \\S+"))
  }
  else {
    return(sub(" ", "", str_extract(full_name, pattern = "\\S+ ")))
  }
}


get_last_name <- function(full_name) {
  if (str_detect(full_name, "Jr."))
    return(sub(" ", "", str_extract(full_name, pattern = " \\S+ \\S+")))
  else {
    return(sub(" ", "", str_extract(full_name, pattern = " \\S+$")))
  }
}

# Vectorized version
get_first_name_v <- Vectorize(get_first_name)
get_last_name_v <- Vectorize(get_last_name)

# Min. 400 PAs
steal_data <- read_csv("steal_data_15_19.csv") %>%
  mutate(first_name = get_first_name_v(Name),
         last_name = get_last_name_v(Name)) %>%
  rename(singles = `1B`)

head(steal_data)

# Combine dataframes together

# First add handedness to steal_data
steal_data <- left_join(steal_data, Master_handedness, by = c("playerid" = "IDFANGRAPHS"))

table(steal_data$bats)

# The two players missing their batting side are AJ Pierzynski and Anthony Santander
which(is.na(steal_data$bats))
steal_data[502, "bats"] <- "L" 
steal_data[661, "bats"] <- "B"

# Next, combine with sprint speed data. full_data will hold the final combination of 
# all the tables

# First, adjust some names in steal_data
old_first_names <- c("Giovanny", "Mike", "Nicholas", "Nori", "Peter")
last_names <- c("Urshela", "Yastrzemski", "Castellanos", "Aoki", "Alonso")
new_first_names <- c("Gio", "Michael", "Nick", "Norichika", "Pete")

for (i in 1:length(old_first_names)) {
  old_name <- old_first_names[i]; new_name <- new_first_names[i]; last_name <- last_names[i] 
  print(sum(steal_data$first_name == old_name & steal_data$last_name == last_name))
  steal_data[steal_data$first_name == old_name & steal_data$last_name == last_name, 
             "first_name"] <- new_name
}



# Special case for Melvin Upton Jr. / B.J. Upton
steal_data[which(steal_data$first_name == "Melvin"), "first_name"] <- "B.J."
steal_data[which(steal_data$last_name == "Upton Jr."), "last_name"] <- "Upton"

full_data <- left_join(steal_data, sprint_speed_data, by = c("first_name", "last_name",
                                                             "Season" = "year"))
sum(is.na(full_data$sprint_speed)) # All rows were matched
head(full_data)

# Next, calculate leaguewide stolen base runs (formula from FanGraphs)
# lgwSB = (SB * runSB + CS * runCS) / (1B + BB + HBP – IBB)
league_stats <- inner_join(league_opport, sb_cs_weights, by = "Season")
lgw_sb_df <- league_stats %>%
  mutate(lgwSB = (SB * runSB + CS * runCS) / (`1B` + BB + HBP - IBB)) %>%
  select(Season, runSB, runCS, lgwSB)

# Join lgw_sb_df with full_data and calculate wSB for each player
# wSB = (SB * runSB) + (CS * runCS) – (lgwSB * (1B + BB + HBP – IBB))
full_data <- inner_join(full_data, lgw_sb_df, by = "Season") %>%
  mutate(wSB = ((SB * runSB) + (CS * runCS) - (lgwSB * (singles + BB + HBP - IBB))),
         opportun = singles + BB + HBP - IBB, wSB_rate = wSB / opportun) %>%
  select(Season, Name, Team, G, PA, SB, CS, BsR, bats, hp_to_1b, sprint_speed, wSB, lgwSB,
         everything())

full_data %>%
  arrange(wSB)

run_time_splits <- full_data %>%
  group_by(bats) %>%
  summarize(avg_run = mean(hp_to_1b, na.rm = TRUE), avg_sprint = mean(sprint_speed))

# Find hp_to_1b summaries for each batter side
for (i in c("B", "L", "R")) {
  df <- subset(full_data, bats == i)
  print(summary(df$hp_to_1b))
}

# Calculate ratio of lefty vs righty pitcher PAs
conn <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db") 

query_data <- dbGetQuery(conn, "SELECT game_pk, at_bat_number, p_throws FROM statcast_data")
lefty_pitch_count <- dbGetQuery(conn, "SELECT COUNT(*) FROM statcast_data where p_throws == 'L'")
righty_pitch_count <- dbGetQuery(conn, "SELECT COUNT(*) FROM statcast_data where p_throws == 'R'")
dbDisconnect(conn)

pa_splits <- query_data %>%
  distinct() %>%
  group_by(p_throws) %>%
  summarize(count = n())
# Percentage of righty and lefty PAs 
1 / (pa_splits$count / pa_splits$count[2:1] + 1)

# Percentage of pitches is roughly the same
1 / (lefty_pitch_count / righty_pitch_count + 1)

# Expected home plate to first base times for switch hitters based on PA ratio
sum(1 / (pa_splits$count / pa_splits$count[2:1] + 1) * run_time_splits$avg_run[3:2])

# I decided to shift down hp_to_home for B and R to match the average of L. There is some evidence
# in terms of average sprint speed that righties as a group may be faster, but it isn't strong 
# enough for me to adjust my adjustments.
switch_index <- which(full_data$bats == "B"); right_index <- which(full_data$bats == "R")
# Value to adjust switch and right hand hitters hp_to_1b times by
switch_adjust <- run_time_splits$avg_run[1] - run_time_splits$avg_run[2]
right_adjust <- run_time_splits$avg_run[3] - run_time_splits$avg_run[2]

full_data[switch_index, "hp_to_1b"] <- full_data[switch_index, "hp_to_1b"] - switch_adjust
full_data[right_index, "hp_to_1b"] <- full_data[right_index, "hp_to_1b"] - right_adjust

# Average hp_to_1b are now the same
full_data %>%
  group_by(bats) %>%
  summarize(avg_run = mean(hp_to_1b, na.rm = TRUE), avg_sprint = mean(sprint_speed))


cor(full_data$sprint_speed, full_data$wSB_rate)
cor(full_data$hp_to_1b, full_data$wSB_rate, use = "na.or.complete")

under_27 <- subset(full_data, sprint_speed < 27)
over_27 <- subset(full_data, sprint_speed >= 27)

cor(under_27$sprint_speed, under_27$wSB_rate)
cor(over_27$sprint_speed, over_27$wSB_rate)

# Visualizations
ggplot(full_data, aes(x = sprint_speed)) +
  geom_density()

ggplot(filter(full_data, bats == "L"), aes(x = hp_to_1b)) +
  geom_density()

ggplot(data = full_data, aes(x = sprint_speed, y = wSB)) +
  geom_point() +
  geom_smooth()

ggplot(data = full_data, aes(x = hp_to_1b, y = wSB)) +
  geom_point() +
  geom_smooth()

ggplot(data = full_data, aes(x = sprint_speed, y = wSB_rate)) +
  geom_point() +
  geom_smooth()

ggplot(data = full_data, aes(x = hp_to_1b, y = wSB_rate)) +
  geom_point() +
  geom_smooth()

# Build some simple spline regression models

# First sprint speed model
full_data_no_2020 <- full_data %>%
  filter(Season != 2019)
(n <- nrow(full_data_no_2020))

set.seed(50)
train_index_sprint <- sample(1:n, 600)
train_sprint <- full_data_no_2020[train_index_sprint,]
cv_sprint <- full_data_no_2020[-train_index_sprint,]

train_sprint_over_27 <- subset(train_sprint, sprint_speed >= 27)
ggplot(train_sprint_over_27, aes(x = sprint_speed, y = wSB_rate)) +
  geom_point() +
  geom_smooth()

lm_fit_sprint <- lm(wSB_rate ~ sprint_speed + I(sprint_speed ^ 2), data = train_sprint) 
source("fit_on_cv.R")
fit_on_cv(lm_fit_sprint, cv_sprint)


spline_fit_sprint <- lm(wSB_rate ~ bs(sprint_speed, knots = c(27), degree = 1),
                        data = train_sprint)
fit_on_cv(spline_fit_sprint, cv_sprint)

test <- predict(spline_fit_sprint, data.frame(sprint_speed = seq(24, 31, by = 0.2)))
ggplot(NULL, aes(x = seq(24, 31, by = 0.2), y = test)) +
  geom_point()


# When I get back, try with just wSB too. First, figure out error on splome_fit_sprint and
# experiment with knots. Possibly edit or make a new function in order to fit on wSB instead
# of wSB_rate


# When I come back, start building simple models. need to start by making training, cv, and test sets.
# Thinking of using 2015-2018(half), 2018(half), and 2019 and 2017-2018(half), 2018(half), and 2019.
# Also I think this is a great opportunity to try some splines stuff (forget exact name, but I'm 
# thinking the stuff that splits the data into different ranges)



