library(tidyverse)
library(ggplot2)
library(readr)
library(nortest)

start_path <- "C:/Users/Ishaan/Documents/R/sabermetric-projects/Project 3 Catcher Pop Times/data_files/arm_data_"

# Read in function for cleaning data
source("clean_data.R")

# Reads in and cleans catcher throwing data for each year. For data set 1, I set a 10 throw min.
arm_data_2015 <- read_csv(paste0(start_path, "2015.csv")) %>% clean_data(2015, 10)
arm_data_2016 <- read_csv(paste0(start_path, "2016.csv")) %>% clean_data(2016, 10)
arm_data_2017 <- read_csv(paste0(start_path, "2017.csv")) %>% clean_data(2017, 10)
arm_data_2018 <- read_csv(paste0(start_path, "2018.csv")) %>% clean_data(2018, 10)
arm_data_2019 <- read_csv(paste0(start_path, "2019.csv")) %>% clean_data(2019, 10)


head(arm_data_2015)
arm_data_2015 %>%
  arrange(pop_time) %>%
  head()

# Combine all catcher throwing data from 2015 through 2019 together for analysis.
arm_data_all <- rbind(arm_data_2015, arm_data_2016, arm_data_2017, arm_data_2018, arm_data_2019)
arrange(arm_data_all, pop_time)


source("output_graphs_and_tables.R")

output_graphs_and_tables(arm_data_all)


# Next, I perform the same operations above on a different data set, which I will call data set 2.
# There are at least two ways to look at tool grades. One way you could look at tool grades is by treating
# each player as a data point. Thus, tool grades would be based on the averages of players, where each player,
# no matter how many throws, would be treated equally in the distribution. This is what the data in 
# the arm_data_all looked at, except that I did set a 10 throws-to-second minimum.

# Another interpretation of tool grades could be based not on player averages, which weight each player equally,
# but on individual throws. While Baseball Savant has not released individual throw pop time data publically,
# you can still account for every throw equally. I did this by first removing the 10 throw minimum from the
# first dataset, and then weighting each player's pop time linearly based on the number of throws. For example,
# let's say the dataset had Catcher A with 2 throws at an average of 1.84 seconds, Catcher B with 3 throws
# at an average of 1.99 seconds, and Catcher C with only 1 throw at 2.04. The dataset, using my methodology
# described above would be (1.84, 1.84, 1.99, 1.99, 1.99, 2.04). Averages don't give me access to a player's
# individual pop time distribution, but I think this is the next best thing in terms of weighting each throw.
# I store data using this methodology in arm_data_all2

arm_data_2015_min <- read_csv(paste0(start_path, "2015.csv")) %>% clean_data(2015, 1)
arm_data_2016_min <- read_csv(paste0(start_path, "2016.csv")) %>% clean_data(2016, 1)
arm_data_2017_min <- read_csv(paste0(start_path, "2017.csv")) %>% clean_data(2017, 1)
arm_data_2018_min <- read_csv(paste0(start_path, "2018.csv")) %>% clean_data(2018, 1)
arm_data_2019_min <- read_csv(paste0(start_path, "2019.csv")) %>% clean_data(2019, 1)

# Combine all throw data (minimum of 1 throw to 2nd Base) into 1 tibble. This is a temporary tibble, as I
# haven't performed the data replication method described above yet.
arm_data_all_min <- rbind(arm_data_2015_min, arm_data_2016_min, arm_data_2017_min, arm_data_2018_min,
                          arm_data_2019_min)

# Checking the number of throws
nrow(arm_data_all_min)
arrange(arm_data_all_min, pop_time)

# Total number of throws to second
all_throws_count <- sum(arm_data_all_min$throw_count)

# Allocating a vector before looping
pop_times_all_throws <- vector("numeric", all_throws_count)

counter <- 1

# Creates pop time dataset according to data set 2 methodology.
for (i in 1:nrow(arm_data_all_min)) {
  pop_time <- arm_data_all_min[[i, "pop_time"]]
  throw_count <- arm_data_all_min[[i, "throw_count"]]
  # Add pop_time to vector throw_count number of times
  for (j in 1:throw_count) {
    pop_times_all_throws[counter] <- pop_time
    counter <- counter + 1
  }
}

table(pop_times_all_throws)

# Tibble with just one column: pop_time
arm_data_all2 <- tibble(pop_time = pop_times_all_throws)


output_graphs_and_tables(arm_data_all2)


