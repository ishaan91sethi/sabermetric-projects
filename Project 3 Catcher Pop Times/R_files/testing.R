# Some code I used for testing.


library(tidyverse)
library(ggplot2)
library(readr)
library(nortest)

start_path <- "C:/Users/Ishaan/Documents/R/sabermetric-projects/Project 3 Catcher Pop Times/data_files/arm_data_"

# Read in function for cleaning data
source("clean_data.R")

# Now I will do the same analysis with a different dataset.
arm_data_2015_min <- read_csv(paste0(start_path, "2015.csv")) %>% clean_data(2015, 1)
arm_data_2016_min <- read_csv(paste0(start_path, "2016.csv")) %>% clean_data(2016, 1)
arm_data_2017_min <- read_csv(paste0(start_path, "2017.csv")) %>% clean_data(2017, 1)
arm_data_2018_min <- read_csv(paste0(start_path, "2018.csv")) %>% clean_data(2018, 1)
arm_data_2019_min <- read_csv(paste0(start_path, "2019.csv")) %>% clean_data(2019, 1)

arm_data_all_min <- rbind(arm_data_2015_min, arm_data_2016_min, arm_data_2017_min, arm_data_2018_min,
                          arm_data_2019_min)

all_throws_count <- sum(arm_data_all_min$throw_count)

pop_times_all_throws <- vector("numeric", all_throws_count)

counter <- 1

# Creates pop time dataset like I want it
for (i in 1:nrow(arm_data_all_min)) {
  pop_time <- arm_data_all_min[[i, "pop_time"]]
  throw_count <- arm_data_all_min[[i, "throw_count"]]
  for (j in 1:throw_count) {
    pop_times_all_throws[counter] <- pop_time
    counter <- counter + 1
  }
}

table(pop_times_all_throws)

arm_data_all2 <- tibble(pop_time = pop_times_all_throws)

pop_times <- arm_data_all2$pop_time

# Standard Deviation and Mean Pop Times
sd_pop_times <- sd(pop_times)
mean_pop_times <- mean(pop_times)

# Distribution graph, using a histogram
distr_graph <- ggplot(data = arm_data_all2, mapping = aes(x = pop_time)) +
  geom_histogram(binwidth = 0.01, aes(y = ..density..)) +
  xlab("Pop Times") +
  ylab("Probability Density Percentage\n") +
  ggtitle("Distribution of Average Catcher Pop Times from 2015 to 2019")

distr_graph

# Test For Normality. p-value of 2.2*10^-16. Once again, I don't need the data to be a normal for anything,
# but I was interested in the results. This test indicates the data is not normal, but these tests tend
# to reject normality the higher the number of observations, so I'm not terribly surprised.
ad.test(pop_times)

qqnorm(pop_times)
# Distribution with Normal Curve Overlayed
distr_graph + stat_function(fun = dnorm, args = list(mean = mean_pop_times, sd = sd_pop_times), size = 2,
                            col = "blue")


pop_times <- arm_data_all2$pop_time
grades <- seq(80, 20, -10)

# Old pop time grades. BA handbook gives ranges, so I just found the mean of the range for the grades.
ba_pop_times <- c(1.74, 1.795, 1.895, 1.995, 2.095, 2.195, 2.25)
ba_mean_pop_times <- mean(ba_pop_times)

# New tool grades by the empirical rule
empir_pop_times <- quantile(pop_times, c(0.003, 0.05, 0.32, 0.5, 0.68, 0.95, 0.997))

# New tool grades by standard deviation 
# These are the pop time on the 20-80 scale based on the actual data standard deviation 
data_pop_times <- (sd_pop_times * c(-3:3)) + mean_pop_times  


# Normal Curves Comparing BA Pop Time Distribution to Statcast Pop Time Distributions
ggplot(data.frame(x = c(1.7, 2.3)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = ba_mean_pop_times, sd = 0.1), size = 2, col = 'red') +
  stat_function(fun = dnorm, args = list(mean = mean_pop_times, sd = sd_pop_times), size = 2,
                col = 'blue') +
  ylab("Probability Density Percentage\n") +
  xlab("Pop Time") +
  ggtitle("Normal Distributions of Baseball America (Red) and Statcast (Blue) Pop Times")

# Tibble with all three tool grades
pop_time_grades_df <- tibble(grades, ba_pop_times, empir_pop_times, data_pop_times)
pop_time_grades_df






