# Function that calculates transverse spin's magnitude, vector components, and related
# quantities. Choose K value, which depends on air density

# Requires mosaic and pracma packages to be loaded in first

calc_transverse_spin <- function(df, K) {
  time1 <- Sys.time()
  
  # Find y value at release and add it to dataframe
  df[, "yR"] <- 60.5 - df$release_extension
  
  # Add velocity's y component at release
  df[, "vyR"] <- -sqrt(df$vy0 ^ 2 - 2 * df$ay * (50 - df$yR))
  
  # Add velocity's x and z components at release
  t <- (df$vy0 - df$vyR) / df$ay # First find time t
  df[, "vxR"] <- df$vx0 - df$ax * t
  df[, "vzR"] <- df$vz0 - df$az * t
  
  # Add flight time
  quadrat_solver <- function(a, b, c) {
    root1 <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a) 
    root2 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a) 
    if (root1 > root2) {
      return(root2)
    }
    return(root1)
  }
  
  quadrat_solver_vec <- Vectorize(quadrat_solver)
  df[, "tf"] <- quadrat_solver_vec(0.5 * df$ay, df$vyR, -(17/12 - df$yR))
  
  # Add average velocity components
  df[, "vxbar"] <- df$ax * df$tf / 2 + df$vxR
  df[, "vybar"] <- df$ay * df$tf / 2 + df$vyR
  df[, "vzbar"] <- df$az * df$tf / 2 + df$vzR
  
  # Add average drag, could try more efficient implementation
  n <- nrow(df)
  adrag <- numeric(n)
  
  a_vec <- c("ax", "ay", "az")
  vbar_vec <- c("vxbar", "vybar", "vzbar")
  amag_vec <- c("amagx", "amagy", "amagz")
  
  time2 <- Sys.time()
  print(time2 - time1)
  
  for (i in 1:n) {
    row <- df[i,]
    adrag[i] <- project(as.vector(t(row[, a_vec])) + c(0, 0, 32.1740), 
                        (-as.vector(t(row[, vbar_vec]))), type = "length")
  }
  
  df[, "adrag"] <- adrag
  
  # Add Magnus Acceleration Components
  unit_vector <- function(vec) {
    return(vec / sqrt(sum(vec^2)))
  }
  
  #mag_accel_x <- mag_accel_y <- mag_accel_z <- numeric(n)
  
  time3 <- Sys.time()
  print(time3 - time2)
  
  avg_velo_magnitude <- sqrt(data$vxbar ^ 2 + data$vybar ^ 2 + data$vzbar ^ 2)
  
  df[, "amagx"] <- data$ax + (data$adrag * data$vxbar / avg_velo_magnitude)
  df[, "amagy"] <- data$ay + (data$adrag * data$vybar / avg_velo_magnitude)
  df[, "amagz"] <- data$az + (data$adrag * data$vzbar / avg_velo_magnitude) + 32.174
  
  # Add overall magnus acceleration magnitude
  df[, "amag"] <- sqrt(df$amagx ^ 2 + df$amagy ^ 2 + df$amagz ^ 2)
  
  # Add magnus movements in x and z directions
  # Multiply equation by 12 to put movement into inches
  df[, "Mx"] <- 12 * 0.5 * df$amagx * (df$tf) ^ 2
  df[, "Mz"] <- 12 * 0.5 * df$amagz * (df$tf) ^ 2
  
  # Add Lift Coefficient
  
  df[, "Cl"] <- df$amag / (df$K * avg_velo_magnitude ^ 2)
  
  # Add transverse spin magnitude
  r <- 0.121 # In feet, according to Alan Nathan on Twitter
  A <- 0.336; B <- 6.041
  
  # Multiply by (30 / pi) to convert it from radians per second to revolutions per minute
  df[, "spinT"] <- (avg_velo_magnitude / (r * B)) * log(A / (A - df$Cl)) * (30 / pi)
  
  time4 <- Sys.time()
  print(time4 - time3)
  
  # Add Transverse Spin Components
  tspin_x <- tspin_y <- tspin_z <- numeric(n)
  for (i in 1:n) {
    row2 <- df[i,]
    cross_prod_curr <- cross(unit_vector(as.vector(t(row2[, vbar_vec]))), 
                             unit_vector(as.vector(t(row2[, amag_vec]))))
    tspin_x[i] <- cross_prod_curr[1]
    tspin_y[i] <- cross_prod_curr[2]
    tspin_z[i] <- cross_prod_curr[3]
  }
  
  spinT <- df$spinT
  df[, "spinTX"] <- spinT * tspin_x
  df[, "spinTY"] <- spinT * tspin_y
  df[, "spinTZ"] <- spinT * tspin_z
  
  # Add Spin Factor
  df[, "S"] <- (1 / B) * log(A / (A - df$Cl))
  
  # Add Spin Efficiency
  df[, "spin_efficiency"] <- df$spinT / df$release_spin_rate
  
  time5 <- Sys.time()
  print(time5 - time4)
  return(df)
  
  
}






