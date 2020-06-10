library(tidyverse)
library(readr)

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
graphs_and_cors(full_data_clean)


ggplot(data = full_data_clean, aes(x = o_swing.x, z_swing.x)) +
  geom_point()

colnames(full_data_clean)
# Collinearity tests
cor(full_data_clean$o_swing.x, full_data_clean$swing.x)
cor(full_data_clean$o_contact.y, full_data_clean$contact.y)
cor(full_data_clean$z_contact.y, full_data_clean$swing_strike.y)


# Use this potentially for naming variables, could include this in cleaning function
paste0(colnames(full_data_clean), "h")





