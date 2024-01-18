# Things to correct/control for: pitch count, type of pitch, location, and batter and hitter side. 
# Instead of home runs, maybe barrels?
# Variables to consider: velocity, horizontal, and vertical movement. Only pitches in the strikezone.
# Different model for each pitch type, each count, and each location. May want to go one pitch at a time.
# launch_speed_angle variable for barrels.

# Note: Statcast pitch speeds only for 2017 onwards, according to csv info page
library(RSQLite)
library(dplyr)
library(ggplot2)
library(randomForest)

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



pitch_data_zone <- pitch_data2 %>%
  filter(zone %in% 1:9)



# Current model; different model for pitcher and batter handedness, and pitch type. Predict barrel or 
# not using 4 variables: count factor variable (12 levels), velocity, horizontal movement, pitch location
# and vertical movement. 
# All pitches in the strike zone, not just pitches swung on in the strike zone. By including
# count as a variable, I take into account the swing percentage and better contact that accompanies 
# certain counts. 
# 1. With these models, I can predict the chance of a barrel for that particular pitch.
# 2. Another potential application is evaluating a pitch solely based on its velocity and movement 
# by averaging (or weighted averaging based on pitch zone frequency) the coefficients for velocity 
# and movement across each zone. I could also just plug in all of a pitcher's specific pitch type
# that were in the zone and average the values, although this is count dependent. 
# 3. Can plug in a pitcher's strike zone pitches and average the results (or maybe show a distribution) to
# attempt to show how barrel prone a pitcher is. 

# Limitations: doesn't incorporate pitches outside of the zone, simplifies zone into 9 locations rather
# than the more precise actual location, no sequencing, doesn't consider spin variables (direction, 
# efficiency)

# Potential algorithms: logistic regression, knn, svms


# ------------------------- Four Seam Fastballs ---------------------------------

ff_data <- subset(pitch_data_zone, pitch_type == "FF") %>%
  mutate(barrel = as.factor(barrel), zone = as.factor(zone), 
         handedness_combo = as.factor(paste0(p_throws, stand))) %>%
  select(-p_throws, -stand, -launch_speed_angle, -balls, -strikes, -description,
         -swing_t_f)

# NOTE: For handedness combo, the first letter is the pitcher's throwing arm and the second is the
# batter's side
head(ff_data)

table(ff_data$handedness_combo); table(ff_data$barrel); table(ff_data$zone);
ff_data_train <- ff_data %>%
  filter(game_year != 2020)
ff_data_test <- ff_data %>%
  filter(game_year == 2020)
# Next, I will implement 10-fold CV
set.seed(192)
ff_data_train <- ff_data_train[sample(nrow(ff_data_train)),] # Randomize dataframe
fold_count <- nrow(ff_data_train) %/% 10  # Approximate number of observations in each fold 
# This will become the "fold" column in my training dataframe
fold_column <- unlist(sapply(1:10, rep, times = fold_count, simplify = FALSE))
# Just let the last two observations be in the last fold, as nrow(ff_data_train) is not divisible by 10
fold_column <- c(fold_column, rep(10, nrow(ff_data_train) - length(fold_column)))

# When I get back, functionalize the above. And continue creating the cross validation function



# My test data is just all pitches from 2019
test_ff <- subset(ff_data, game_year == 2019)

# Will use out of bag error for CV 
train_ff <- subset(ff_data, game_year != 2019)

# Tried two different approaches to randomForest(), dataframe vs matrix. There was not a significant
# difference in terms of times so I just used the dataframe approach. The matrix approach is in a 
# separate R file.
# Dataframe approach
set.seed(9)
rf_model_ff <- randomForest(barrel ~ release_speed + zone + hor_mov + vert_mov + count_factor + 
                              handedness_combo, ntree = 100, mtry = 4, data = train_ff)

# Manually tinkered with hyperparameters. Did this by looking at differences in importance values
# for each variable and average prediction for barrels and non barrels.

# For ntree, I chose 100. I experimented with 10, 50,
# 100, and 150, but even 10 stabilized quite quickly. I could go higher, but it is computationally
# expensive with this much data.

# For mtry, I tested mtry values from 1 through 6 (although the lower end was obviously not going
# to perform well). I settled for mtry = 4, as it had the largest separation in predicted values
# for barrels vs non-barrels (using OOB predictions)


# Model analysis

importance(rf_model_ff)
oob_pred_ff <- predict(rf_model_ff, type = "prob")
ff_pred_actual_df <- train_ff %>%
  mutate(out_of_bag_pred = oob_pred_ff[, 2]) %>%
  select(out_of_bag_pred, barrel, release_speed, zone, hor_mov, vert_mov, count_factor, 
         handedness_combo, description, everything())

# data.frame(out_of_bag_pred = oob_pred_ff[, 2], barrel = train_ff$barrel)

head(ff_pred_actual_df)

source("print_grouped_df.R")

print_grouped_df(ff_pred_actual_df, "barrel")
print_grouped_df(ff_pred_actual_df, "zone")
print_grouped_df(ff_pred_actual_df, "count_factor")
print_grouped_df(ff_pred_actual_df, "handedness_combo")


summary(subset(ff_pred_actual_df, barrel == 0)$out_of_bag_pred)
summary(subset(ff_pred_actual_df, barrel == 1)$out_of_bag_pred)

ff_pred_actual_df %>%
  arrange(desc(out_of_bag_pred))

rf_model_ff

# Graph
ggplot() +
  geom_density(data = subset(ff_pred_actual_df, barrel == 0), aes(x = out_of_bag_pred)) +
  geom_density(data = subset(ff_pred_actual_df, barrel == 1), aes(x = out_of_bag_pred), color = "blue") +
  xlim(c(0, 0.2))

# Look to see model performance using the above chunk
# (lines 175-177) and expand on this. Possibly look at distributions. Look into ways to speed up
# random forest model training

# To improve the model, can tune the hyperparameters

# When I get back, tinker with mtry hyperparameter, look at randomForestExplainer package, 
# use package to keep analyzing the random forest. Look into weighted classes, ml articles,
# consider ROC curve. Also add actual location to variables



# Logistic Regression (and other approaches needing a CV set)
ff_data_no_2019 <- subset(ff_data, game_year != 2019)
n <- nrow(ff_data_no_2019)
train_index <- sample(1:n, round(n * 0.8))

train_ff2 <- ff_data_no_2019[train_index,]
cv_ff2 <- ff_data_no_2019[-train_index,]

log_model <- glm(barrel ~ release_speed + hor_mov + vert_mov + balls + strikes,
                 family = "binomial", data = train_ff2)
# Try count instead of balls and strikes in model

# Account for balls in barrel statistic for individual pitchers by including strike percentage











pitch_data_zone %>%
  filter(pitch_type == "FF") %>%
  group_by(pitch_type, zone, p_throws, stand) %>%
  summarize(count = n()) %>%
  arrange(count)

ff_rr_data <- subset(pitch_data_zone, pitch_type == "FF" & p_throws == "R" & stand == "R")

ff_rr_data <- ff_rr_data %>%
  mutate(barrel = as.factor(barrel), zone = as.factor(zone))

# My test data is just all pitches from 2019
test_ff_rr <- subset(ff_rr_data, game_year == 2019)
# Will use out of bag error for CV 
train_ff_rr <- subset(ff_rr_data, game_year != 2019)

rf_model_ff_rr <- randomForest(barrel ~ release_speed + hor_mov + vert_mov + balls + strikes,
                               ntree = 50, mtry = 5, data = df_zone[index_train,])











# List of dataframes split up by zone variable
ff_rr_df_list <- split(ff_rr_data, ff_rr_data$zone)

ff_rr_df_list[[9]] %>%
  group_by(barrel) %>%
  summarize(avg_velo = mean(release_speed), avg_hor_mov = mean(hor_mov),
            avg_vert_mov = mean(vert_mov), count = n())


ff_rr_models <- list()

# Fits a log model for a certain location in the strike zone
fit_log_model <- function(df, zone_choice, seed) {
  df[, "index"] <- 1:nrow(df)
  df_zone <- subset(df, zone == zone_choice & game_year != 2019)
  n <- nrow(df_zone)
  
  set.seed(seed) 
  index_train <- sample(df_zone$index, round(n * 0.8))
  log_model <- glm(barrel ~ release_speed + hor_mov + vert_mov + balls + strikes,
                   family = "binomial", data = df_zone[index_train,])
  return(list(log_model, index_train))
}

x <- fit_log_model(ff_rr_data, 3, 15)
summary(x[[1]])

pred <- predict(x[[1]], ff_rr_data[x[[2]],], type = "response")
x2 <- ff_rr_data[x[[2]],]
x2[, "pred"] <- pred

# Outputs a 2 item list with a random forests model and indices of training set
fit_rf_model <- function(df, zone_choice, seed) {
  df[, "index"] <- 1:nrow(df)
  df_zone <- subset(df, zone == zone_choice & game_year != 2019)
  n <- nrow(df_zone)
  
  set.seed(seed) 
  index_train <- sample(df_zone$index, round(n * 0.8))
  rf_model <- randomForest(barrel ~ release_speed + hor_mov + vert_mov + balls + strikes,
                           ntree = 50, mtry = 5, data = df_zone[index_train,])
  return(list(rf_model, index_train))
}

df_zone3 <- subset(ff_rr_data, zone == 3 & game_year != 2019)
rf_model <- randomForest(barrel ~ release_speed + hor_mov + vert_mov + balls + strikes + zone,
                         strata = ff_rr_data$barrel, sampsize = c(6000, 3000),
                         ntree = 1000, mtry = 3, data = ff_rr_data)

rf_model
importance(rf_model)

x3 <- fit_rf_model(ff_rr_data, 3, 15)
summary(x3[[1]])

# When I get back, make and add in count factor variable in model

# Confusion matrix: rows are actual labels, columns are predicted labels. 

# For predict() with randomForest() model, use type = "prob"


# When I get back, start modeling, first by making training, test, and cv sets. 
