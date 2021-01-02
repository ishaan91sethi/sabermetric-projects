library(readxl)
library(tidyverse)

data_orig <- read_excel("MovementSpinEfficiencyTemplate-v2.xlsx")

# Column 9, yR (y of release) is found using column 24, release_extension

# Column 41 through 43 give the components of transverse spin which I wanted.
# 46 is spin efficiency

data <- cbind(data_orig[, c(17:22, 24, 8:10, 4:6, 25:28, 30, 32:43, 46)],  K = 5.3831e-3)
summary(data$release_extension/data$yR)

# To find yR, compute 60.5 - release_extension
summary(60.5 - data$release_extension)
summary(data$yR)

# (vx0, vy0, vz0) is when y = 50 in the trajectory. Both x and z are also different,
# not equal to release values. Check tweet from 6-25-20 in pictures if confused

# Remember, y distance is negative because positive y is towards pitcher and ball is moving away

# Velocity's y component at release (vyR) calculation
summary(-sqrt(data$vy0 ^ 2 - 2 * data$ay * (50 - data$yR)))
summary(data$vyR)
mean(-sqrt(data$vy0 ^ 2 - 2 * data$ay * (50 - data$yR)) == data$vyR)

# Velocity's x component at release (vxR) calculation
t <- (data$vy0 - data$vyR) / data$ay
summary(data$vx0 - data$ax * t)
summary(data$vxR)
mean(data$vx0 - data$ax * t == data$vxR)

# Velocity's z component at release (vzR) calculation
summary(data$vz0 - data$az * t)
summary(data$vzR)
mean(data$vz0 - data$az * t == data$vzR)

# Flight time calculation

# Returns smaller root. I think it is smaller root b/c ay is positive but vyR is negative
# so larger time is if theoretically ay stayed positive (no gravity, ball never hit ground)
# and eventually turned velocity positive and the ball comes back to 17/12 after reversing
# direction 
quadrat_solver <- function(a, b, c) {
  root1 <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a) 
  root2 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a) 
  if (root1 > root2) {
    return(root2)
  }
  return(root1)
}

quadrat_solver_vec <- Vectorize(quadrat_solver)

summary(quadrat_solver_vec(0.5 * data$ay, data$vyR, -(17/12 - data$yR)))
summary(data$tf)
mean(quadrat_solver_vec(0.5 * data$ay, data$vyR, -(17/12 - data$yR)) == data$tf)


# Average velocity components
# Average x velocity
summary(data$ax * data$tf / 2 + data$vxR)
summary(data$vxbar)
mean(data$ax * data$tf / 2 + data$vxR == data$vxbar)

# Average y velocity
summary(data$ay * data$tf / 2 + data$vyR)
summary(data$vybar)
mean(data$ay * data$tf / 2 + data$vyR == data$vybar)

# Average z velocity
summary(data$az * data$tf / 2 + data$vzR)
summary(data$vzbar)
mean(data$az * data$tf / 2 + data$vzR == data$vzbar)

# Average drag, look at excel form for formula 
library(mosaic)
# Adjust for gravity in FEET not meters (32.1522)
adrag_pred <- numeric(nrow(data))
for (i in 1:nrow(data)) {
  row <- data[i,]
  adrag_pred[i] <- project(as.vector(t(row))[4:6] + c(0, 0, 32.1740), (-as.vector(t(row)))[15:17], 
                           type = "length")
}

summary(adrag_pred)
summary(data$adrag)
all.equal(adrag_pred, data$adrag)


# Magnus acceleration components
unit_vector <- function(vec) {
  return(vec / sqrt(sum(vec^2)))
}

mag_accel_x <- mag_accel_y <- mag_accel_z <- numeric(nrow(data))
avg_velo_magnitude <- sqrt(data$vxbar ^ 2 + data$vybar ^ 2 + data$vzbar ^ 2)

mag_accel_x <- data$ax + data$adrag * data$vxbar / avg_velo_magnitude




for (i in 1:nrow(data)) {
  df <- data[i, ]
  mag_vec <- as.vector(t(df))[4:6] - df$adrag * -unit_vector(as.vector(t(df))[15:17]) + c(0, 0, 32.1740)
  mag_accel_x[i] <- mag_vec[1]
  mag_accel_y[i] <- mag_vec[2]
  mag_accel_z[i] <- mag_vec[3]
}

summary(mag_accel_x)
summary(data$amagx)
all.equal(mag_accel_x, data$amagx)

summary(mag_accel_y)
summary(data$amagy)
all.equal(mag_accel_y, data$amagy)

summary(mag_accel_z)
summary(data$amagz)
all.equal(mag_accel_z, data$amagz)

# Overall magnus acceleration magnitude
summary(sqrt(data$amagx ^ 2 + data$amagy ^ 2 + data$amagz ^ 2))
summary(data$amag)

# Magnus movement
# Multiply equation by 12 to put movement into inches
summary(12 * 0.5 * data$amagx * (data$tf) ^ 2)
summary(data$Mx)
all.equal(12 * 0.5 * data$amagx * data$tf ^ 2, data$Mx)


summary(12 * 0.5 * data$amagz * (data$tf) ^ 2)
summary(data$Mz)
all.equal(12 * 0.5 * data$amagz * data$tf ^ 2, data$Mz)

# Lift Coefficient
K <- 5.3831e-3 # Factor K based on air density, baseball mass, and cross sectional area
# Note that K is exclusively for this data. Will need to calculate K differently for 
# different air densities which vary based on ballpark location, temperature, etc
# Also tinkered with K to get more precise values
Cl_pred <- data$amag / (K * (data$vxbar^2 + data$vybar ^ 2 + data$vzbar ^ 2))
summary(Cl_pred)
summary(data$Cl)
all.equal(Cl_pred, data$Cl)


# Transverse Spin Magnitude
v_mean_magnitude <- sqrt(data$vxbar ^ 2 + data$vybar ^ 2 + data$vzbar ^ 2)
r <- 0.121 # In feet, according to Alan Nathan on Twitter
A <- 0.336; B <- 6.041
# Multiply by (30 / pi) to convert it from radians per second to revolutions per minute
t_spin_pred <- (v_mean_magnitude / (r * B)) * log(A / (A - data$Cl)) * (30 / pi)

# This is an approximation.
summary(t_spin_pred)
summary(data$spinT)
summary(t_spin_pred / data$spinT)


# Transverse Spin Components
library(pracma)

pred_tspin_x <- pred_tspin_y <- pred_tspin_z <- numeric(nrow(data))
for (i in 1:nrow(data)) {
  row <- data[i,]
  cross_prod_curr <- row$spinT * cross(unit_vector(as.vector(t(row))[15:17]), 
                                       unit_vector(as.vector(t(row))[19:21]))
  pred_tspin_x[i] <- cross_prod_curr[1]
  pred_tspin_y[i] <- cross_prod_curr[2]
  pred_tspin_z[i] <- cross_prod_curr[3]
}

summary(pred_tspin_x)
summary(data$spinTX)
all.equal(pred_tspin_x, data$spinTX)

summary(pred_tspin_y)
summary(data$spinTY)
all.equal(pred_tspin_y, data$spinTY)

summary(pred_tspin_z)
summary(data$spinTZ)
all.equal(pred_tspin_z, data$spinTZ)

# Spin Factor
summary((1 / B) * log(A / (A - data$Cl)))
summary(data$S)
summary((1 / B) * log(A / (A - data$Cl)) / data$S)

# Spin efficiency
summary(data$spinT / data_orig$release_spin_rate)
summary(data$`spin eff`)
all.equal(r * data$spinT / data_orig$release_spin_rate, data$`spin eff`)


# Test calc_transverse_spin.R
library(mosaic) # for project() function
library(pracma) # for cross() function
source("calc_transverse_spin.R")
test_df <- cbind(data[, c(1:8, 10)], data_orig[, "release_spin_rate"], K = 5.3831e-3)
start_time <- Sys.time()
test_output <- calc_transverse_spin(test_df, K)
end_time <- Sys.time()
start_time - end_time

df_col_names <- colnames(test_output)

for (i in df_col_names[-c(1:11, 33)]) {
  print(summary(data[, i] / test_output[, i]))
}





library(RSQLite)
conn <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db") 
test <- dbGetQuery(conn, "SELECT * FROM statcast_data WHERE game_year == 2018 LIMIT 1")
dbDisconnect(conn)

















df <- data[1,]
v0 <- df[, 1:3]
a_bar <- df[, 4:6]
as_tibble(df)





