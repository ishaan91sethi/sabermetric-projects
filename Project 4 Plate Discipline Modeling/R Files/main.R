# My main R script for modeling hitter strikeout and walk rate.

library(tidyverse)
library(car) # For vif() function
library(lmtest) # For heteroscedasticity test (bptest())
# -------------------------Introduction-----------------------------------------------


# At least 250 PA's for first half and 200 PA's for second half
# Read in each fangraphs csv file and clean the data using clean_data()
source("clean_data.R")
start_path <- "C:/Users/Ishaan/Documents/R/sabermetric-projects/Project 4/data_files/"

first_half_2015 <- clean_data(read_csv(paste0(start_path, "first_half_2015.csv")), 2015)
second_half_2015 <- clean_data(read_csv(paste0(start_path, "second_half_2015.csv")), 2015)
first_half_2016 <- clean_data(read_csv(paste0(start_path, "first_half_2016.csv")), 2016)
second_half_2016 <- clean_data(read_csv(paste0(start_path, "second_half_2016.csv")), 2016)
first_half_2017 <- clean_data(read_csv(paste0(start_path, "first_half_2017.csv")), 2017)
second_half_2017 <- clean_data(read_csv(paste0(start_path, "second_half_2017.csv")), 2017)
first_half_2018 <- clean_data(read_csv(paste0(start_path, "first_half_2018.csv")), 2018)
second_half_2018 <- clean_data(read_csv(paste0(start_path, "second_half_2018.csv")), 2018)
first_half_2019 <- clean_data(read_csv(paste0(start_path, "first_half_2019.csv")), 2019)
second_half_2019 <- clean_data(read_csv(paste0(start_path, "second_half_2019.csv")), 2019)



first_half <- rbind(first_half_2015, first_half_2016, first_half_2017, first_half_2018, first_half_2019)

second_half <- rbind(second_half_2015, second_half_2016, second_half_2017, second_half_2018, second_half_2019)


full_data_clean <- inner_join(first_half, second_half, by = c("Name", "year"))
source("graphs_and_cors.R")
# Prints first half and second half correlations for each statistic and graphs each one too
graphs_and_cors(full_data_clean)



# Collinearity tests
ggplot(data = full_data_clean, aes(x = o_swing.x, z_swing.x)) +
  geom_point()
cor(full_data_clean$o_swing.x, full_data_clean$z_swing.x)

ggplot(data = full_data_clean, aes(x = o_contact.x, z_contact.x)) +
  geom_point()
cor(full_data_clean$o_contact.x, full_data_clean$z_contact.x)
# For these variables, there is some collinearity but not a tremedous amount.


# Indicates high degree of collinearity. 
cor(full_data_clean$o_swing.x, full_data_clean$swing.x)
cor(full_data_clean$o_contact.y, full_data_clean$contact.y)
cor(full_data_clean$z_contact.y, full_data_clean$swing_strike.y)



# -----------------------------------Actual Analysis--------------------------------------

# I chose 2012 as the first year of the dataset for several reasons. The main one
# is that it is the first year that, at least in BIS plate discipline data, the 
# strikeout trends are present league wide in the form of contact percentage declines
# and higher swinging strike rates.

# I chose 400 PA minimum somewhat arbitrarily, but I thought it was a good cutoff point
# as the reliability for both strikeout and walk rate are above 70% and the PA's are
# still low enough to create a fairly large dataset.



disc_data <- clean_data(read_csv(paste0(start_path, "plate_disc_2012_2019.csv")), 2000, 4, 14)


# Demonstrating "perfect" collinearity. In other words, showing how some variables can be 
# created by combinations of the others. Figured these out using good old dimensional analysis. 

# Calculating predicted swing percentage
o_swing <- disc_data[["o_swing"]] 
z_swing <- disc_data[["z_swing"]] 
o_contact <- disc_data[["o_contact"]]
z_contact <- disc_data[["z_contact"]] 
zone <- disc_data[["zone"]] 

pred_swing_pct <- o_swing * (1 - zone) + z_swing * zone 
                               
cor(pred_swing_pct, disc_data[["swing"]])

# Calculating predicted contact percentage
pred_contact_pct <- (o_contact * o_swing * (1 - zone) + z_contact * z_swing * zone) / pred_swing_pct

cor(pred_contact_pct, disc_data[["contact"]])

# Calculating predicted swinging strike
pred_swing_str <- (1 - o_contact) * o_swing * (1 - zone) + (1 - z_contact) * z_swing * zone 
cor(pred_swing_str, disc_data[["swing_strike"]])



# Remove non-essential data and collinear/redundant variables and add interaction terms 

# Potentially remove a bunch of this for clarity and added o_miss and non_zone
disc_data <- disc_data %>%
  select(-Season, -Name, -Team, -swing, -contact, -year, -swing_strike, -playerid) %>%
  mutate(o_miss = 1 - o_contact, non_zone = 1 - zone) 
        

# Some introductory graphs. Clean up/formalize the two notable ones
source("graph_against_k_bb.R")
graph_against_k_bb(disc_data, 7) # Correlations btwn main effects and strikeout rate


# Cleaned up plots for o-miss and z-contact
source("formal_graph.R")

formal_graph("Z-Contact%", "Strikeout Rate", "Z-Contact% and Strikeout Rate",
             disc_data$z_contact, disc_data$k_pct)

formal_graph("O-Contact%", "Strikeout Rate", "O-Contact% and Strikeout Rate",
             disc_data$o_contact, disc_data$k_pct)


# The two variables that are highly predictive of strikeout rate are o-miss and z-contact. The question
# is whether multiple regression will create a better model than these single variables.



# Set seed to create reproducible training, cv, and test sets

set.seed(1)
train_data_indices <- sample(1:1657, 1000)
train_data <- disc_data[train_data_indices,]


leftover_data <- disc_data[-train_data_indices,]
cv_indices <- sample(1:657, 300)
cv_data <- leftover_data[cv_indices,]
k_cv <- cv_data[["k_pct"]]
bb_cv <- cv_data[["walk_pct"]]


test_data <- leftover_data[-cv_indices,]
k_test <- test_data[["k_pct"]]
bb_test <- test_data[["walk_pct"]]


# For mean centering the variables
for (i in c(1:6, 10, 11)) {
  col_mean <- mean(train_data[[i]])
  print(colnames(train_data)[i])
  print(col_mean)
  train_data[[i]] <- train_data[[i]] - col_mean
  cv_data[[i]] <- cv_data[[i]] - col_mean
  test_data[[i]] <- test_data[[i]] - col_mean
}

# Check that test_data variables are approximately, but not exactly, centered around 0
for (i in c(1:6, 10, 11)) {
  print(summary(test_data[i]))
}


# Add the interaction terms for lasso and ridge regression
source("add_interaction.R")
train_data <- add_interaction(train_data)
cv_data <- add_interaction(cv_data)
test_data <- add_interaction(test_data)


# ---------------------------Multiple Linear Regression----------------------------------------------

# Basic model with main effects minus O-Swing%
lm_fit_main <- lm(k_pct ~ z_swing + o_miss + z_contact + non_zone + f_strike, data = train_data)

source("run_lm.R")
vif(lm_fit_main)
# Low VIF scores => low multicollinearity, which is good.
run_lm(lm_fit_main, cv_data, k_cv, "Strikeout")
# CV error: 0.0004409

test_pred_main <- predict(lm_fit_main, test_data)
mean((k_test - test_pred_main) ^ 2)
# Test MSE:  0.0004743076

# Graph model 1 predictions vs actual values on test set
formal_graph("Predicted Strikeout Rate", "Actual Strikeout Rate", "Model 1 Fit on Test Data",
             test_pred_main, k_test)


# Model with all interaction term
lm_fit_all_inter <- lm(k_pct ~ o_swing*o_miss + z_swing * z_contact + zone + f_strike +
                       o_swing:o_miss:non_zone + z_swing:z_contact:zone, data = train_data)
vif(lm_fit_all_inter)
run_lm(lm_fit_all_inter, cv_data, k_cv, "Strikeout")
# CV error: 0.0004351


# Model with two interaction term
lm_fit <- lm(k_pct ~ o_swing + z_swing + o_miss + z_contact + non_zone + f_strike + o_swing:o_miss +
             o_swing:o_miss:non_zone, data = train_data)
vif(lm_fit)
run_lm(lm_fit, cv_data, k_cv, "Strikeout")
# CV error: 0.0004355

test_pred_inter <- predict(lm_fit, test_data)
mean((k_test - test_pred_inter) ^ 2)
# Test MSE: 0.0004606823

formal_graph("Predicted Strikeout Rate", "Actual Strikeout Rate", "Model 2 Fit on Test Data",
             test_pred_inter, k_test)


# Interaction term plots
library(sjPlot)
plot_model(lm_fit, type = "pred", terms = c("o_swing", "o_miss"), mdrt.values = "meansd", title = 
           "Two Term Interaction Plot", axis.title = c("O-Swing", "Predicted Strikeout Rate"))
plot_model(lm_fit, type = "pred", terms = c("o_swing", "o_miss", "non_zone"), mdrt.values = "meansd",
           title = "Three Term Interaction Plot", axis.title = c("O-Swing", "Predicted Strikeout Rate"))
plot_model(lm_fit, type = "pred", terms = c("z_swing", "z_contact"))


# Models with polynomial terms
lm_fit_poly <- lm(k_pct ~ poly(o_swing, 2) + poly(z_swing, 2) + poly(o_miss, 2) +
                  poly(z_contact, 2) + poly(non_zone, 2) + poly(f_strike, 2), data = train_data)
run_lm(lm_fit_poly, cv_data, k_cv, "Strikeout")
# CV error: 0.0004412


# Cubic terms
lm_fit_cubic <- lm(k_pct ~ poly(o_swing, 3) + poly(z_swing, 3) + poly(o_miss, 3) +
                    poly(z_contact, 3) + poly(non_zone, 3) + poly(f_strike, 3), data = train_data)
run_lm(lm_fit_cubic, cv_data, k_cv, "Strikeout") 
# CV error: 0.0004426


# Simple linear regression models and graphs
lm_o_contact <- lm(k_pct ~ o_contact, data = train_data)
run_lm(lm_o_contact, cv_data, k_cv, "Strikeout")
test_pred_o_contact <- predict(lm_o_contact, test_data)
mean((k_test - test_pred_o_contact) ^ 2) # Test MSE of 0.0008499939

formal_graph("Predicted Strikeout Rate", "Actual Strikeout Rate",
             "Simple Linear Regression (O-Contact) on Test Data", test_pred_o_contact, k_test)


lm_z_contact <- lm(k_pct ~ z_contact, data = train_data)
run_lm(lm_z_contact, cv_data, k_cv, "Strikeout")
test_pred_z_contact <- predict(lm_z_contact, test_data)
mean((k_test - test_pred_z_contact) ^ 2)# Test MSE of 0.0009425313

formal_graph("Predicted Strikeout Rate", "Actual Strikeout Rate",
             "Simple Linear Regression (Z-Contact) on Test Data", test_pred_z_contact, k_test)

# Some interpretations: 
# o_swing is not very predicitve of strikeout rate. Possible reason:
# lots of low K hitters swing aggressively.

# z_swing:z_contact interaction term not helpful

# zone may not cause lower strikeout rates; the real cause might be that hitters with higher 
# strikeout rates tend to chase and/or miss pitches out of the zone more. So zone isn't predicting
# strikeout rate; rather it reflects the general perception of a hitter's strikeout ability from 
# prior data

# Little evidence for o_swing * o_miss * non_zone interaction terms

# Little evidence for polynomial terms


# ---------------------------------------------Lasso---------------------------------------------------

# Consider just using zone rather than non_zone if no interaction terms

library(glmnet)
var_vec <- c(1, 2, 10, 4:6) # Variable numbers I want to use
x_matrix <- as.matrix(train_data[, var_vec])
k_train <- train_data[["k_pct"]]
x_matrix_cv <- as.matrix(cv_data[, var_vec])


source("run_ridge_lasso.R")
run_ridge_lasso(x_matrix, k_train, x_matrix_cv, k_cv, 1)
# CV error:  0.0004407


# Next, try model with all interaction terms
var_vec <- c(1, 2, 10, 4:6, 12:15)
x_matrix_inter_all <- as.matrix(train_data[, var_vec])
x_matrix_cv_inter_all <- as.matrix(cv_data[, var_vec])

run_ridge_lasso(x_matrix_inter_all, k_train, x_matrix_cv_inter_all, k_cv, alph = 1)

# CV error:0.0004351
# Selects everything


# Lasso with two interaction terms(o_swing_miss, o_swing_miss_non_zone)
var_vec <- c(1, 2, 10, 4:6, 12, 14)
x_matrix_inter <- as.matrix(train_data[, var_vec])
x_matrix_cv_inter <- as.matrix(cv_data[, var_vec])

run_ridge_lasso(x_matrix_inter, k_train, x_matrix_cv_inter, k_cv, alph = 1)
# CV error: 0.0004356. Selects everything.

# -----------------------------------Ridge Regression----------------------------------------------------
# Only main effects with ridge


run_ridge_lasso(x_matrix, k_train, x_matrix_cv, k_cv, 0)
# CV error: 0.0004406

# Ridge with all interaction terms
run_ridge_lasso(x_matrix_inter_all, k_train, x_matrix_cv_inter_all, k_cv, alph = 0)
# CV error = 0.0004351

# Ridge with two interaction terms
run_ridge_lasso(x_matrix_inter, k_train, x_matrix_cv_inter, k_cv, alph = 0)
# CV error: 0.0004355

# ---------------------------------- Hitter Walk Rate ----------------------------------------------------

# My intuition is that o_swing is the biggest variable, z_swing is moderately big,
# and the contacts aren't as helpful but actually swing and missing will be more 
# correlated to walk rate 

# Important to recognize strikeout and walk rates for hitters aren't necessarily 
# strictly only good or only bad. Different players make different offensive
# profiles work at the cost of other things (Ex: more strikeouts for more homers)
graph_against_k_bb(disc_data, 8) # Correlations btwn main effects and walk rate

formal_graph("O-Swing", "Walk Rate", "The Relationship between O-Swing and Walk Rate", 
             disc_data$o_swing, disc_data$walk_pct)

formal_graph("F-Strike", "Walk Rate", "The Relationship between F-Strike and Walk Rate", 
             disc_data$f_strike, disc_data$walk_pct)
# o_swing and f_strike have the strongest correlations to walk rate, in that order. Somewhat nonlinear
# for both, will give polynomial terms special attention for them

# For walk rate, I am instead using o_contact and looking at the o_swing * o_contact interaction.
# Both of these variables should work against walk rates, so that's why I am using o_contact over
# o_miss.


# -------------------------------- Multiple Regression --------------------------------------------------

lm_fit_main_bb <- lm(walk_pct ~ o_swing + z_swing + o_contact + z_contact + zone + f_strike,
                     data = train_data)

vif(lm_fit_main_bb)

run_lm(lm_fit_main_bb, cv_data, bb_cv, "Walk")
# CV error: 0.0001944
# Z-swing doesn't seem to help much at all. O-contact also doesn't and f-strike also don't seem
# to help strongly either
# Removing these 3 slightly lowers the cv error

# Robust standard error calculation because of nonlinear pattern in residual plots (heteroskedasticity)
library(sandwich)
library(lmtest)
coeftest(lm_fit_main_bb, vcov. = vcovHC(lm_fit_main_bb, type = "HC0"))

# Model with 4 most influential main effects
lm_fit_main_bb4 <- lm(walk_pct ~ o_swing + z_contact + zone + f_strike,
                     data = train_data)

run_lm(lm_fit_main_bb4, cv_data, bb_cv, "Walk")
# CV MSE 0.0001925
bptest(lm_fit_main_bb4)



# Might want to edit run_lm to fix graph titles
lm_fit_bb <- lm(walk_pct ~ o_swing + I(o_swing^2) +
                z_contact + zone + f_strike + I(f_strike^2), data = train_data)

vif(lm_fit_bb)

run_lm(lm_fit_bb, cv_data, bb_cv, "Walk")
# MSE on CV set: 0.0001853

plot(lm_fit_bb)

bptest(lm_fit_bb)

# anova test to test significance of polynomial terms. Might be invalid
anova(lm_fit_main_bb4, lm_fit_bb)



# Calculates robust standard error
coeftest(lm_fit_bb, vcov. = vcovHC(lm_fit_bb, type = "HC0"))

# Model lm_fit_bb fit on test data
test_pred_bb <- predict(lm_fit_bb, test_data)
mean((bb_test - test_pred_bb) ^ 2)
# Test MSE:  0.0002140294

formal_graph("Predicted Walk Rate", "Actual Walk Rate", "Model Fit on Test Data",
             test_pred_bb, bb_test)


# Model solely based on o-swing
simple_o_swing_fit <- lm(walk_pct ~ o_swing, data = train_data)
run_lm(simple_o_swing_fit, cv_data, bb_cv, "Walk")
test_pred_o_swing_bb <- predict(simple_o_swing_fit, test_data)
mean((bb_test - test_pred_o_swing_bb) ^ 2) # Test MSE of 0.0004483271
formal_graph("Predicted Walk Rate", "Actual Walk Rate", "O-Swing Only Fit on Test Data",
             test_pred_o_swing_bb, bb_test)




# Residual analysis


# Check out points with high positive residuals to see what their feature averages look like
residuals_bb <- lm_fit_bb$residuals

ggplot(data = NULL, aes(x = lm_fit$fitted.values, y = lm_fit$residuals)) +
  geom_point() +
  geom_smooth()

# indices of data with large positive residuals (underestimates)
high_resid_index <- which(residuals_bb > 0.02)
high_resid_data <- cbind(train_data[high_resid_index,], residuals = residuals_bb[high_resid_index],
                         fitted_values = lm_fit_bb$fitted.values[high_resid_index])

ggplot(data = high_resid_data, aes(x = fitted_values, y = walk_pct)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)




residual_abs <- abs(residuals_bb)
summary(residual_abs)
# Training observations with residuals of third quartile and up (high residuals, above 0.01637)
big_res_index <- abs(residuals_bb) > 0.01637  
high_resid_train <- train_data[big_res_index, ]

high_resid_train <- high_resid_train %>%
  mutate(o_swing_squared = o_swing ^ 2, f_strike_squared = f_strike^2, 
         residual = residuals_bb[big_res_index], 
         fitted_values = lm_fit_bb$fitted.values[big_res_index]) %>%
  select(o_swing, o_swing_squared, z_contact, zone, f_strike, f_strike_squared,
         residual, fitted_values, walk_pct) %>%
  arrange(residual)

summary(high_resid_train$o_swing)


# Notes
# -Little evidence for o-contact, z-swing, and their polynomial terms
# -Some very compelling evidence for f_strike degree 2 polynomial terms. O-swing has low p-value but
# only very small improvement on CV set. Not much for degree three
# -Other polynomial terms were significant for other variables, but did not reduce cv error
# - Definitely want to mention those non-linear graphs for o_swing and f_strike
# -Only interaction term with evidence was z_swing*z_contact, but this didn't help cv error at all
# -o_swing * o_contact term slightly helps CV error when o_contact is omitted, but this benefit is very
# slight. Also it is only significant in terms of p-value with some variables omitted
# -Lower error rate is largely due to the fact that walk rates don't
# vary as much numerically relative to strikeout rate (Verify this?)
# -Look at these outlier points that are way off



#----------------------------------- Ridge Regression -----------------------------------------------
source("add_squared_terms.R")
ridge_train <- add_squared_terms(train_data)
ridge_cv <- add_squared_terms(cv_data)
ridge_test <- add_squared_terms(test_data)


model.matrix(walk_pct ~ poly(o_swing, 2) +
             z_contact + zone + poly(f_strike, 2), data = train_data)

var_vec <- c(1, 16, 4:6, 17) # Variable numbers I want to use
x_matrix_bb <- as.matrix(ridge_train[, var_vec])
bb_train <- train_data[["walk_pct"]]
x_matrix_cv_bb <- as.matrix(ridge_cv[, var_vec])


source("run_ridge_lasso.R")
run_ridge_lasso(x_matrix_bb, bb_train, x_matrix_cv_bb, bb_cv, 0)

# Ridge regression model doesn't help prevent any overfitting with polynomial terms.



# ---------------------------------- Decision Trees ---------------------------------------------


# Bagging
library(randomForest)
set.seed(15)
bag_fit <- randomForest(walk_pct ~ o_swing + z_contact + zone + f_strike, 
                        data = train_data, mtry = 4, ntree = 1000)
pred_bag <- predict(bag_fit, newdata = cv_data)
mean((pred_bag - bb_cv) ^2)
# CV MSE of 0.0002263

# Random Forests with mtry = 2

set.seed(25)
rf_fit <- randomForest(walk_pct ~ o_swing + z_swing  + o_contact + z_contact + zone + f_strike, 
                        data = train_data, mtry = 4, ntree = 1000)
pred_rf <- predict(rf_fit, newdata = cv_data)
mean((pred_rf - bb_cv) ^2)
# CV MSE of 0,0002177

import_values <- importance(rf_fit)
ggplot(data = NULL, aes(x = dimnames(import_values)[[1]], y = import_values)) +
  geom_col(fill = "blue") +
  ggtitle("Importance Values for Plate Discipline Statistics") +
  xlab("Plate Discipline Statistics") +
  ylab("Variable Importance (Based on residual sum of squares)") +
  theme(plot.title = element_text(hjust = 0.5))
  


# Boosting with shrinkage = 0.1, n.trees = 1000, interaction.depth = 2, n.trees for prediction = 100
library(gbm)
set.seed(5)
gbm_fit <- gbm(walk_pct ~ o_swing + z_contact + zone + f_strike, 
               data = train_data, shrinkage = 0.001, n.trees = 1e4, distribution = "gaussian",
               interaction.depth = 2)
gbm_pred <- predict(gbm_fit, newdata = cv_data, n.trees = 1e4)
mean((gbm_pred - bb_cv) ^ 2)
# CV MSE of 0.0001962


