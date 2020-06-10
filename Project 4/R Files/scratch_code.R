# Some test/previous code that I'm keeping for reference  




disc_data <- clean_data(read_csv(paste0(start_path, "plate_disc_2012_2019.csv")), 2000, 4, 14)

# I didn't change the discipline variables from percentages into decimals because 
# it doesn't affect the linear regression (coefficients will scale accordingly). 
# However I might change this due to interaction terms.


# Tests for collinearity. In other words, showing how some variables can be created by combinations of
# the others. Figured these out using good old dimensional analysis. Perfect collinearity

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

# Remove non-essential data and collinear/redundant variables and add interaction terms and
# polynomial terms

disc_data <- disc_data %>%
  select(-Season, -Name, -Team, -swing, -contact, -year, -swing_strike, -playerid) %>%
  mutate(o_swing_contact = o_swing * o_contact, z_swing_contact = z_swing * z_contact,
         non_zone = 1 - zone, o_swing_contact_non_zone = o_swing * o_contact * non_zone,
         z_swing_contact_zone = z_swing * z_contact * zone, o_swing2 = o_swing ^ 2,
         z_swing2 = z_swing ^ 2, o_contact2 = o_contact ^ 2, z_contact2 = z_contact ^ 2,
         zone2 = zone ^ 2, f_strike2 = f_strike ^ 2, o_swing3 = o_swing ^ 3,
         z_swing3 = z_swing ^ 3, o_contact3 = o_contact ^ 3, z_contact3 = z_contact ^ 3,
         zone3 = zone ^ 3, f_strike3 = f_strike ^ 3)

set.seed(1)
train_data_indices <- sample(1:1657, 1000)
train_data <- disc_data[train_data_indices,]

leftover_data <- disc_data[-train_data_indices,]
cv_indices <- sample(1:657, 300)
cv_data <- leftover_data[cv_indices,]

test_data <- leftover_data[-cv_indices,]



# Will use lasso regression for this analysis.
# First analysis: Use plate discipline metrics to predict batter strikeout rate
library(glmnet)
x_matrix <- as.matrix(train_data[, 1:6])
y <- train_data[["k_pct"]]
x_matrix_cv <- as.matrix(cv_data[, 1:6])
y_cv <- cv_data[["k_pct"]]


# First model with no interaction terms or polynomial terms
set.seed(1) # For reproducing built in 10 fold CV
simple_lasso_cv <-  cv.glmnet(x_matrix, y, standardize = FALSE)
plot(simple_lasso_cv)
best_lam <- simple_lasso_cv$lambda.min

simple_lasso <- glmnet(x_matrix, y, lambda = best_lam, standardize = FALSE)
lasso_pred <- predict(simple_lasso, newx =  as.matrix((cv_data[, 1:6])))
mean((lasso_pred - y_cv)^2)
ggplot(data = NULL, aes(x = lasso_pred, y = y_cv)) +
  geom_point()

cor(lasso_pred, y_cv)

coef(simple_lasso)
# CV error:  0.0004407

# Next, try model with all interaction terms but no polynomial terms

x_matrix_inter <- as.matrix(train_data[, c(1:6, 10, 11, 13, 14)])
x_matrix_cv_inter <- as.matrix(cv_data[, c(1:6, 10, 11, 13, 14)])

set.seed(1)
lasso_interact_cv <- cv.glmnet(x_matrix_inter, y, standardize = FALSE)
plot(lasso_interact_cv)
best_lam_inter <- lasso_interact_cv$lambda.min

lasso_inter <- glmnet(x_matrix_inter, y, lambda = best_lam_inter, standardize = FALSE)
lasso_pred_inter <- predict(lasso_inter, newx =  x_matrix_cv_inter)
mean((lasso_pred_inter - y_cv)^2)
ggplot(data = NULL, aes(x = lasso_pred_inter, y = y_cv)) +
  geom_point()

cor(lasso_pred_inter, y_cv)
coef(lasso_inter)
# CV error:0.0004410

# Same variables with ridge instead
set.seed(1)
ridge_interact_cv <- cv.glmnet(x_matrix_inter, y, alpha = 0)
plot(ridge_interact_cv)
best_lam_ridge <- ridge_interact_cv$lambda.min

ridge_inter <- glmnet(x_matrix_inter, y, lambda = best_lam_ridge, alpha = 0)
ridge_pred_inter <- predict(ridge_inter, newx = x_matrix_cv_inter)
mean((ridge_pred_inter - y_cv)^2)
ggplot(data = NULL, aes(x = ridge_pred_inter, y = y_cv)) +
  geom_point()

cor(ridge_pred_inter, y_cv)
coef(ridge_inter)
# CV error: 0.0004414

# Test lasso with only 2 interaction terms
x_matrix_inter2 <- as.matrix(train_data[, c(1:6, 10, 11)])
x_matrix_cv_inter2 <- as.matrix(cv_data[, c(1:6, 10, 11)])

set.seed(1)
lasso_interact_cv2 <- cv.glmnet(x_matrix_inter, y)
plot(lasso_interact_cv2)
best_lam_inter2 <- lasso_interact_cv2$lambda.min

lasso_inter2 <- glmnet(x_matrix_inter2, y, lambda = best_lam_inter2)
lasso_pred_inter2 <- predict(lasso_inter2, newx =  x_matrix_cv_inter2)
mean((lasso_pred_inter2 - y_cv)^2)
ggplot(data = NULL, aes(x = lasso_pred_inter2, y = y_cv)) +
  geom_point()

cor(lasso_pred_inter2, y_cv)
coef(lasso_inter2)
# CV error: 0.0004407

# 2 interaction terms with ridge
set.seed(1)
ridge_interact_cv2 <- cv.glmnet(x_matrix_inter2, y, alpha = 0)
plot(ridge_interact_cv2)
best_lam_ridge2 <- ridge_interact_cv2$lambda.min

ridge_inter2 <- glmnet(x_matrix_inter2, y, lambda = best_lam_ridge2, alpha = 0)
ridge_pred_inter2 <- predict(ridge_inter2, newx = x_matrix_cv_inter2)
mean((ridge_pred_inter2 - y_cv)^2)
ggplot(data = NULL, aes(x = ridge_pred_inter2, y = y_cv)) +
  geom_point()

cor(ridge_pred_inter2, y_cv)
coef(ridge_inter2)
# CV error: 0.0004404

# Lasso with everything

x_matrix_all <- as.matrix(train_data[, c(1:6, 10, 11, 13:26)])
x_matrix_cv_all <- as.matrix(cv_data[, c(1:6, 10, 11, 13:26)])

set.seed(1)
lasso_all_cv <- cv.glmnet(x_matrix_inter, y, standardize = FALSE)
plot(lasso_all_cv)
best_lam_all <- lasso_all_cv$lambda.min

lasso_all <- glmnet(x_matrix_all, y, lambda = best_lam_all, standardize = FALSE)
lasso_pred_all <- predict(lasso_all, newx =  x_matrix_cv_all)
mean((lasso_pred_all - y_cv)^2)
ggplot(data = NULL, aes(x = lasso_pred_all, y = y_cv)) +
  geom_point()

cor(lasso_pred_all, y_cv)
coef(lasso_all)
# CV error: 0.0004408

# All variables with ridge did quite badly
set.seed(1)
ridge_all_cv <- cv.glmnet(x_matrix_inter, y, standardize = FALSE, alpha = 0)
plot(ridge_all_cv)
best_lam_all_ridge <- ridge_all_cv$lambda.min

ridge_all <- glmnet(x_matrix_all, y, lambda = best_lam_all_ridge, standardize = FALSE, alpha = 0)
ridge_pred_all <- predict(ridge_all, newx =  x_matrix_cv_all)
mean((ridge_pred_all - y_cv)^2)
ggplot(data = NULL, aes(x = ridge_pred_all, y = y_cv)) +
  geom_point()

cor(ridge_pred_all, y_cv)
coef(ridge_all)
# CV error: 0.0005199





# Regular linear regression
disc_data
lm_fit <- lm(k_pct ~ o_swing + z_swing + o_contact + z_contact + zone + f_strike + o_swing * o_contact +
               z_swing * z_contact + o_swing_contact_non_zone + z_swing_contact_zone, data = train_data)
lm_pred <- predict(lm_fit, cv_data)
mean((lm_pred - y_cv) ^ 2)
summary(lm_fit)
cor(lm_pred, y_cv)
# CS MSE: 0.0004350

# Simpler model that performs nearly as well. non_zone doesn't help but I keep 
# it as it is in an interaction term
lm_fit2 <- lm(k_pct ~ o_swing + z_swing + o_contact + z_contact + f_strike + non_zone
              + o_swing_contact_non_zone, data = train_data)
lm_pred2 <- predict(lm_fit2, cv_data)
mean((lm_pred2 - y_cv) ^ 2)
summary(lm_fit2)
cor(lm_pred2, y_cv)


mean((lm_fit2$residuals) ^ 2)

resid <- lm_pred2 - y_cv
ggplot(data = NULL, aes(x = lm_pred2, y = resid)) +
  geom_point()

ggplot(data = NULL, aes(x = resid)) +
  geom_density()

# CV MSE: 0.0004357


# Only the regular variables. Does roughly as well as ridge and lasso

lm_fit3 <- lm(k_pct ~ o_swing + z_swing + o_contact + z_contact + f_strike + zone, data = train_data)
lm_pred3 <- predict(lm_fit3, cv_data)
mean((lm_pred3 - y_cv) ^ 2)
summary(lm_fit3)
cor(lm_pred3, y_cv)
# CV error: 0.0004407

# All variables regular lm(). Did slightly worse than ridge and lasso
lm_fit4 <- lm(k_pct ~ . - k_pct - walk_pct -PA - non_zone, data = train_data)
lm_pred4 <- predict(lm_fit4, cv_data)
mean((lm_pred4 - y_cv) ^ 2)
summary(lm_fit4)
cor(lm_pred4, y_cv)
mean((lm_fit4$residuals)^2)
coef(lm_fit4)
# CV error: 0.0004356


# Just regular terms and polynomials
poly_data <- train_data %>%
  select(-walk_pct, -PA, -o_swing_contact, - z_swing_contact, -non_zone, -o_swing_contact,
         -z_swing_contact)
lm_fit5 <- lm(k_pct ~ ., data = poly_data)
lm_pred5 <- predict(lm_fit5, cv_data)
mean((lm_pred5 - y_cv) ^ 2)
summary(lm_fit5)
cor(lm_pred5, y_cv)
# CV error: 0.0004367

# Only regular terms, performs similar to ridge/lasso
lm_fit6 <- lm(k_pct ~ o_swing + z_swing + o_contact + z_contact + zone + f_strike, data = train_data)
lm_pred6 <- predict(lm_fit6, cv_data)
mean((lm_pred6 - y_cv) ^ 2)
summary(lm_fit6)
cor(lm_pred6, y_cv)
# CV error: 0.0004407

# Regular terms and squared polynomials
poly_data_squared <- train_data %>%
  select(-walk_pct, -PA, -o_swing_contact, - z_swing_contact, -non_zone, -o_swing_contact,
         -z_swing_contact, -o_swing3, -z_swing3, -o_contact3, -z_contact3, -zone3, -f_strike3,
         -o_swing_contact_non_zone, -z_swing_contact_zone, -zone, -zone2)
lm_fit7 <- lm(k_pct ~ ., data = poly_data_squared)
lm_pred7 <- predict(lm_fit7, cv_data)
mean((lm_pred7 - y_cv) ^ 2)
summary(lm_fit7)
cor(lm_pred7, y_cv)
# CV error: 0.0004501 (Inconsistencies?)

# z_contact2 seems to have some positive benefit. Examine this tomorrow.


library(caret)
library(car)
vif(lm_fit6)





# Lots of weird stuff going on here. Might want to redo all of it.

# Basic model with 6 variables
library(car) # For vif() function

train_data_test <- train_data %>%
  mutate(o_miss = 1 - o_contact, o_swing_miss_non_zone = o_swing * o_miss * non_zone)

cv_data_test <- cv_data %>%
  mutate(o_miss = 1 - o_contact, o_swing_miss_non_zone = o_swing * o_miss * non_zone)

# For "centering" the variables
for (i in c(1:6, 12)) {
  col_mean1 <- mean(train_data_test[[i]])
  col_mean2 <- mean(cv_data_test[[i]])
  train_data_test[[i]] <- train_data_test[[i]] - col_mean1
  cv_data_test[[i]] <- cv_data_test[[i]] - col_mean2
}


simple_lm_fit <- lm(k_pct ~ o_swing + z_swing + o_miss + z_contact + f_strike + zone, data = train_data_test)
simple_pred <- predict(simple_lm_fit, cv_data_test)
mean((simple_pred - y_cv) ^ 2)
summary(simple_lm_fit)
cor(simple_pred, y_cv)
vif(simple_lm_fit)
# CV error: 0.0004403

test_fit <- lm(k_pct ~ o_swing + z_swing + o_miss + z_contact + non_zone + f_strike +
                 o_swing:o_miss, data = train_data_test)
test_pred <- predict(test_fit, cv_data_test)
mean((test_pred - y_cv) ^ 2)
summary(test_fit)
cor(test_pred, y_cv)

vif(test_fit)
# CV error: 0.0004373

# Some interpretations: 
# o_swing is not very predicitve of strikeout rate. Possible reason:
# lots of low K hitters swing aggressively.

# z_swing:z_contact interaction term not helpful

# zone may not cause lower strikeout rates; the real cause might be that hitters with higher 
# strikeout rates tend to chase and/or miss pitches out of the zone more. So zone isn't predicting
# strikeout rate; rather it reflects the general perception of a hitter's strikeout ability from 
# prior data



test_fit <- lm(k_pct ~ o_miss + z_contact + z_swing + o_swing + f_strike + non_zone
               + z_swing + o_miss:o_swing, data = train_data_test)
test_pred <- predict(test_fit, cv_data_test)
mean((test_pred - y_cv) ^ 2)
summary(test_fit)
cor(test_pred, y_cv)

vif(test_fit)

ggplot(data = NULL, aes(x = cv_data$z_swing, y = cv_data$k_pct)) +
  geom_point()

ggplot(data = NULL, aes(x = cv_data$z_contact, y = cv_data$k_pct)) +
  geom_point()

ggplot(data = NULL, aes(x = (1 - cv_data$o_contact), y = cv_data$k_pct)) +
  geom_point()

ggplot(data = NULL, aes(x = test_pred, y = cv_data$k_pct)) +
  geom_point()


disc_data <- disc_data %>%
  select(-Season, -Name, -Team, -swing, -contact, -year, -swing_strike, -playerid) %>%
  mutate(o_miss = 1 - o_contact, o_swing_contact = o_swing * o_contact, z_swing_contact = z_swing * z_contact,
         non_zone = 1 - zone, o_swing_contact_non_zone = o_swing * o_contact * non_zone,
         z_swing_contact_zone = z_swing * z_contact * zone, o_swing2 = o_swing ^ 2,
         z_swing2 = z_swing ^ 2, o_contact2 = o_contact ^ 2, z_contact2 = z_contact ^ 2,
         zone2 = zone ^ 2, f_strike2 = f_strike ^ 2, o_swing3 = o_swing ^ 3,
         z_swing3 = z_swing ^ 3, o_contact3 = o_contact ^ 3, z_contact3 = z_contact ^ 3,
         zone3 = zone ^ 3, f_strike3 = f_strike ^ 3, o_swing_miss_non_zone = o_swing * o_miss * non_zone,
         o_swing_miss = o_swing * o_miss)














# First model with only main effects
set.seed(1) # For reproducing built in 10 fold CV
lasso_main_cv <-  cv.glmnet(x_matrix, y, standardize = FALSE)
plot(lasso_main_cv)
(best_lam <- lasso_main_cv$lambda.min)

lasso_main <- glmnet(x_matrix, y, lambda = best_lam, standardize = FALSE)
lasso_main_pred <- predict(lasso_main, newx =  x_matrix_cv)
mean((lasso_main_pred - y_cv)^2)
ggplot(data = NULL, aes(x = lasso_main_pred, y = y_cv)) +
  geom_point()

cor(lasso_main_pred, y_cv)

coef(lasso_main)
# CV error:  0.0004404


# Next, try model with all interaction terms
var_vec <- c(1, 2, 10, 4:6, 11, 12, 14, 15)
x_matrix_inter_all <- as.matrix(train_data[, var_vec])
x_matrix_cv_inter_all <- as.matrix(cv_data[, var_vec])

set.seed(1)
lasso_interact_all_cv <- cv.glmnet(x_matrix_inter_all, y, standardize = FALSE)
plot(lasso_interact_all_cv)
(best_lam_inter_all <- lasso_interact_all_cv$lambda.min)

lasso_inter_all <- glmnet(x_matrix_inter_all, y, lambda = best_lam_inter_all, standardize = FALSE)
lasso_pred_inter_all <- predict(lasso_inter_all, newx =  x_matrix_cv_inter_all)
mean((lasso_pred_inter_all - y_cv)^2)
ggplot(data = NULL, aes(x = lasso_pred_inter_all, y = y_cv)) +
  geom_point()

cor(lasso_pred_inter_all, y_cv)
coef(lasso_inter_all)
# CV error:0.0004394
# Lasso removed z_swing, z_swing_contact_zone, o_swing_miss_non_zone


# Lasso with one interaction term(o_swing_miss)
var_vec <- c(1, 2, 10, 4:6, 11)
x_matrix_inter <- as.matrix(train_data[, var_vec])
x_matrix_cv_inter <- as.matrix(cv_data[, var_vec])

set.seed(1)
lasso_interact_cv <- cv.glmnet(x_matrix_inter, y, standardize = FALSE)
plot(lasso_interact_cv)
(best_lam_inter <- lasso_interact_cv$lambda.min)

lasso_inter <- glmnet(x_matrix_inter, y, lambda = best_lam_inter, standardize = FALSE)
lasso_pred_inter <- predict(lasso_inter, newx =  x_matrix_cv_inter)
mean((lasso_pred_inter - y_cv)^2)
ggplot(data = NULL, aes(x = lasso_pred_inter, y = y_cv)) +
  geom_point()

cor(lasso_pred_inter, y_cv)
coef(lasso_inter)
# CV error: 0.0004380. Selects all variables

# -----------------------------------Ridge Regression----------------------------------------------------
# Only main effects with ridge
set.seed(1)
ridge_main_cv <- cv.glmnet(x_matrix, y, standardize = FALSE, alpha = 0)
plot(ridge_main_cv)
(best_lam_ridge <- ridge_main_cv$lambda.min)

ridge_main <- glmnet(x_matrix, y, lambda = best_lam_ridge, alpha = 0, standardize = FALSE)
ridge_main_pred <- predict(ridge_main, newx = x_matrix_cv)
mean((ridge_main_pred - y_cv)^2)
ggplot(data = NULL, aes(x = ridge_main_pred, y = y_cv)) +
  geom_point()

cor(ridge_main_pred, y_cv)
coef(ridge_main)
# CV error: 0.0011220



# Ridge with all interaction terms
set.seed(1)
ridge_interact_all_cv <- cv.glmnet(x_matrix_inter_all, y, alpha = 0, standardize = FALSE,)
plot(ridge_interact_all_cv)
(best_lam_ridge_all <- ridge_interact_all_cv$lambda.min)

ridge_inter_all <- glmnet(x_matrix_inter_all, y, lambda = best_lam_ridge_all, alpha = 0, standardize = FALSE)
ridge_pred_inter_all <- predict(ridge_inter_all, newx =  x_matrix_cv_inter_all)
mean((ridge_pred_inter_all - y_cv)^2)
ggplot(data = NULL, aes(x = ridge_pred_inter_all, y = y_cv)) +
  geom_point()

cor(ridge_pred_inter_all, y_cv)
coef(ridge_inter_all)
# CV error = 0.0009972


# Ridge with one interaction term(o_swing_miss)
var_vec <- c(1, 2, 10, 4:6, 11)
x_matrix_inter <- as.matrix(train_data[, var_vec])
x_matrix_cv_inter <- as.matrix(cv_data[, var_vec])

set.seed(1)
ridge_interact_cv <- cv.glmnet(x_matrix_inter, y, standardize = FALSE)
plot(ridge_interact_cv)
(best_lam_ridge_inter <- ridge_interact_cv$lambda.min)

ridge_inter <- glmnet(x_matrix_inter, y, lambda = best_lam_ridge_inter, standardize = FALSE)
ridge_pred_inter <- predict(ridge_inter, newx =  x_matrix_cv_inter)
mean((ridge_pred_inter - y_cv)^2)
ggplot(data = NULL, aes(x = ridge_pred_inter, y = y_cv)) +
  geom_point()

cor(ridge_pred_inter, y_cv)
coef(ridge_inter)
# CV error: 0.0004380



# For "centering" the variables
for (i in c(1:6, 12)) {
  col_mean1 <- mean(train_data[[i]])
  col_mean2 <- mean(cv_data[[i]])
  col_mean3 <- mean(test_data[[i]])
  train_data[[i]] <- train_data[[i]] - col_mean1
  cv_data[[i]] <- cv_data[[i]] - col_mean2
  test_data[[i]] <- test_data[[i]] - col_mean3
}




disc_data <- disc_data %>%
  select(-Season, -Name, -Team, -swing, -contact, -year, -swing_strike, -playerid) %>%
  mutate(o_miss = 1 - o_contact, o_swing_miss = o_swing * o_miss, z_swing_contact = z_swing * z_contact,
         non_zone = 1 - zone, o_swing_miss_non_zone = o_swing * o_miss * non_zone,
         z_swing_contact_zone = z_swing * z_contact * zone) 










