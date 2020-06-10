library(tidyverse)
library(ggplot2)
library(readr)
library(nortest)

start_path <- "C:/Users/Ishaan/Documents/R/sabermetric-projects/Project 3 Catcher Pop Times/data_files/arm_data_"

# Read in function for cleaning data
source("clean_data.R")

# Reads in and cleans catcher throwing data for each year. For data set 1, I set a 10 throw min.
arm_data_2015 <- read_csv(paste0(start_path, "2015.csv")) 
arm_data_2016 <- read_csv(paste0(start_path, "2016.csv"))
arm_data_2017 <- read_csv(paste0(start_path, "2017.csv")) 
arm_data_2018 <- read_csv(paste0(start_path, "2018.csv"))
arm_data_2019 <- read_csv(paste0(start_path, "2019.csv"))

arm_data_all <- rbind(arm_data_2015, arm_data_2016, arm_data_2017, arm_data_2018, arm_data_2019) %>%
  filter(pop_2b_sba_count >= 5)

mean(as.numeric(arm_data_all$pop_2b_cs), na.rm = TRUE)
mean(as.numeric(arm_data_all$pop_2b_sb), na.rm = TRUE)

