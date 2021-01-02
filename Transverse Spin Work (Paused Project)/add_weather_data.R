# Script to obtain weather data from retrosheet gamelogs
library(tidyverse)
library(lubridate) # For date-time 
library(ggplot2)
library(rvest)
library(RSelenium)
library(parallel)

boxscore_data_original <- read_csv("boxscore_data_gameday.csv") 

boxscore_data <- boxscore_data_original %>%
  filter(substr(game_date, 1, 4) %in% 2015:2019)

unique(boxscore_data$venue_name)

# There are 34 ballparks to consider from 2015 through 2019, including Turner Field 
# and 3 international stadiums

# Tomorrow: Find closest stations on wunderground for all 34 ballparks. Then, check to 
# see which games in the gamelog are omitted after doing a left join (in other words, check if I'm missing
# any regular season game ballparks)

ballparks <- c("Fenway Park", "Wrigley Field", "Dodger Stadium", "Oakland Coliseum", "O.co Coliseum",
               "Angel Stadium",
               "Angel Stadium of Anaheim", "Kauffman Stadium", "Rogers Centre", 
               "Tropicana Field", "Guaranteed Rate Field", "U.S. Cellular Field",
               "Oriole Park at Camden Yards", "Progressive Field", "Coors Field", "Chase Field", 
               "T-Mobile Park", "Safeco Field", "AT&T Park", "Oracle Park", "Minute Maid Park", 
               "Comerica Park", "PNC Park",
               "Miller Park", "Great American Ball Park", "Petco Park", "Citizens Bank Park", 
               "Busch Stadium", "Nationals Park", "Yankee Stadium", "Citi Field", "Target Field",
               "Marlins Park", "SunTrust Park", "Turner Field", "Globe Life Park in Arlington", 
               "Estadio de Beisbol Monterrey", "Tokyo Dome", "London Stadium", "BB&T Ballpark",
               "Fort Bragg Field", "TD Ameritrade Park")

airport_codes <- c("kbos", "kord", "kbur", "koak", "koak", "ksna", "ksna", "kmci", "cytz", "kpie",
                   "kmdw", "kmdw", "kbwi", "kcle", "kden", "kphx", "kbfi", "kbfi", "ksfo", "ksfo",
                   "khou", "kdet", "kpit", "kmke", "kluk", "ksan", "kphl", "kstl", "kdca", "klga", 
                   "klga", "kmsp", "kmia", "katl", "katl", "kdfw", "mmmy", "rjtt", "eglc", "kipt", 
                   "kfay", "koma")


# Tibble holding ballpark names and codes of closest airports
parks_and_codes <- tibble(venue_name = ballparks, airport_code = airport_codes)


unique_ballparks <- unique(boxscore_data$venue_name)
ballparks %in% unique(boxscore_data$venue_name)

table(boxscore_data$game_type)

# There is no historical data for Hiram Bithorn Stadium on wunderground, so I omitted it
regular_season <- boxscore_data %>%
  filter(game_type == "R", venue_name != "Hiram Bithorn Stadium")

missing <- regular_season %>%
  filter(!(venue_name %in% ballparks)) 

table(missing$venue_name)

regular_season %>%
  filter(venue_name == "Angel Stadium") %>%
  nrow()

# A is all star game, D is maybe Division Series, E is maybe exhibition, F is Wild Card Games,
# L is League Championship Series, R is Regular Season, S is Spring Training, W is World Series

full_boxscore_data <- left_join(regular_season, parks_and_codes, by = "venue_name")

# pitch_w_boxscore <- left_join(test, full_boxscore_data, by = "game_pk")


# Need to remove unnecessary info from both dataframes
# I think I will use the gameday data merged with airport codes to add weather instead
# of using the gameday/aircode data merged with pitch data. I think it will be simpler
# to just add on the weather data afterwards and not have to go through each pitch.

base_url <- "https://www.wunderground.com/history/daily/"

rD <- rsDriver(browser = "firefox", port = 4401L, verbose = F)
remDr <- rD[["client"]]


# Note: I am choosing a close time to the start time, not necessarily the closest though.
# Ex: If a game starts at 1 PM and the two closest times are 12:50 and 1:50, the function
# will choose 1:50. I think this is okay because firstly, I'd rather have temp data from
# during the game than slightly before the game (weather data at start time is imprecise 
# due to changing conditions during the game, so choosing a time before is even more off)
# and secondly adding a function to compute the closest time every time could hurt efficiency.

# Input is row from full_boxscore_data
get_weather <- function(row_input) {
  hour_start <- hour(row_input$start_time) + 1
  url <- gsub("-0", "-", paste0(base_url, row_input$airport_code, "/date/", row_input$game_date))
  remDr$navigate(url)
  html <- remDr$getPageSource()[[1]]
  webpage <- read_html(html) # parse HTML
  
  temp_data <- html_nodes(webpage, css = ".mat-column-temperature .wu-value-to") %>%
    html_text()
  row_input[, "temperature_wunder"] <- temp_data[hour_start]
  
  pressure_data <- html_nodes(webpage, css = ".wu-unit-pressure .wu-value-to") %>%
    html_text()
  row_input[, "pressure"] <- pressure_data[hour_start]
  
  humid_data <- html_nodes(webpage, css = ".wu-unit-humidity .wu-value-to") %>%
    html_text()
  row_input[, "humidity"] <- humid_data[hour_start]
  
  return(row_input)
}

library(foreach)
library(doParallel)

# numCores <- detectCores()
# registerDoParallel(numCores)


# Use find_url and line below to generate URLs for all games
find_url <- function(current_row) {
  return_char <- gsub("-0", "-", paste0(base_url, current_row["airport_code"], 
                                        "/date/", current_row["game_date"]))
  return(return_char)
}

url_list <- apply(full_boxscore_data, 1,  find_url)

# List of starting hours to get a specific row of data from
# hour_list <- hour(full_boxscore_data$start_time) + 2
start_time_list <- full_boxscore_data$start_time




# Finds the index of the closest time to game start time. This index gives
# us which row to scrape from for each url
find_time_index <- function(target_time, time_list) {
  # Calculate time differences
  diffs <- difftime(time_list, target_time, units = "mins")
  diffs[diffs < 0] <- Inf
  return(which.min(diffs))
}

time_list_test <- parse_time(c("5:53 AM", "6:29 AM", "6:50 AM", "6:53 AM", "6:57 AM", 
                               "7:05 AM", "7:10 AM", "7:18 AM", "3:00 PM"), format = "%H:%M %p")
time_list_test

find_time_index(parse_time("7:04 PM", format = "%H:%M %p"), time_data)


# When I get back: test find_time_index thoroughly. Finish inserting it in get_weather_df, 
# while COMMENTING code not deleting



remDr$navigate(url_list[53])
Sys.sleep(1)
html <- remDr$getPageSource()[[1]]
webpage <- read_html(html) # parse HTML

time_data <- html_nodes(webpage, css = ".mat-table-sticky .ng-star-inserted") %>%
  html_text() %>%
  parse_time(format = "%H:%M %p")

length(time_data)
time_data
time_data <- html_nodes(webpage, css = ".wu-unit-humidity .wu-value-to") %>%
  html_text() 


# Find and adds weather data to a dataframe, using a for loop. Seems to be less 
# efficient than get_weather2()
get_weather_df <- function(df) {
  n <- nrow(df)
  temperature <- pressure <- humidity <- time_weather <- numeric(n)
  
  for (i in 1:n) {
  #foreach (i = 1:n) %dopar% {
    library(lubridate) # For date-time 
    library(rvest)
    library(RSelenium)
    row_input <- df[i,]
    
    #hour_start <- hour(row_input$start_time) + 1
    time_start <- row_input$start_time
    url <- gsub("-0", "-", paste0(base_url, row_input$airport_code, "/date/", row_input$game_date))
    remDr$navigate(url)
    Sys.sleep(2)
    html <- remDr$getPageSource()[[1]]
    webpage <- read_html(html) # parse HTML
    
    time_data <- html_nodes(webpage, css = ".mat-table-sticky .ng-star-inserted") %>%
      html_text() #%>%
      #parse_time(format = "%H:%M %p")
    
    
    # index of row that is after and closest to start time
    index <- find_time_index(time_start, parse_time(time_data, format = "%H:%M %p"))
    time_weather[i] <- time_data[index]
    
    # I use [index - 1] for the non-time data, b/c the time data has an extra NA at
    # the start which the other columns though. This seems to be constant throughout
    # all the webpages I've checked, but I'm uncertain if it is true for all.
    temp_data <- html_nodes(webpage, css = ".mat-column-temperature .wu-value-to") %>%
      html_text()
    temperature[i] <- temp_data[index - 1]
    
    pressure_data <- html_nodes(webpage, css = ".wu-unit-pressure .wu-value-to") %>%
      html_text()
    pressure[i] <- pressure_data[index - 1]
    
    humid_data <- html_nodes(webpage, css = ".wu-unit-humidity .wu-value-to") %>%
      html_text()
    humidity[i] <- humid_data[index - 1]
    
    # time_data <- html_nodes(webpage, css = ".mat-table-sticky .ng-star-inserted") %>%
    #   html_text()
    # time_weather[i] <- time_data[hour_start + 1]
  }
  
  df[, "temperature_wunder"] <- temperature
  df[, "pressure"] <- pressure
  df[, "humidity"] <- humidity
  df[, "time_weather"] <- time_weather
  return(df)
}

#Test for index offset

# Second weather function that generates weather data only using starting time and url
get_weather2 <- function(url, start_time) {
  library(rvest)
  #library(RSelenium)
  output_list <- numeric(5)
  remDr$navigate(url)
  Sys.sleep(3) # Let page load fully
  html <- remDr$getPageSource()[[1]]
  
  webpage <- read_html(html) # parse HTML
  
  time_data <- html_nodes(webpage, css = ".mat-table-sticky .ng-star-inserted") %>%
    html_text()
  
  index <- find_time_index(start_time, parse_time(time_data, format = "%H:%M %p"))
  output_list[4] <- time_data[index]
  
  temp_data <- html_nodes(webpage, css = ".mat-column-temperature .wu-value-to") %>%
    html_text()
  output_list[1] <- temp_data[index - 1]
  
  pressure_data <- html_nodes(webpage, css = ".wu-unit-pressure .wu-value-to") %>%
    html_text()
  output_list[2] <- pressure_data[index - 1]
  
  humid_data <- html_nodes(webpage, css = ".wu-unit-humidity .wu-value-to") %>%
    html_text()
  output_list[3] <- humid_data[index - 1]
  
  # time_data <- html_nodes(webpage, css = ".mat-table-sticky .ng-star-inserted") %>%
  #   html_text()
  
  # print(time_data)
  # output_list[4] <- time_data[hour_start]
  
  return(output_list)
}



# Several issues: first website is missing. Times are way off. Need closest time function

full_boxscore_data[c(100, 110, 126),] -> df
start_time2 <- Sys.time()
get_weather_df(df) -> output
end_time2 <- Sys.time()
end_time2 - start_time2

output[, c("temperature", "temperature_wunder", "pressure", "humidity", "start_time", "time_weather")]





start_time <- Sys.time()
x <- unlist(mapply(get_weather2, url = url_list[c(100, 110, 126)], 
                   start_time = start_time_list[c(100, 110, 126)]))
end_time <- Sys.time()
end_time - start_time

# Also remember to eventually convert units

rD <- rsDriver(browser = "firefox", port = 4401L, verbose = F)
remDr <- rD[["client"]]


# ---------------------------------------------ACTUAL SCRAPING----------------------------------------
# When I get back, turn this into a function

start_time <- Sys.time()
x_current <- unlist(mapply(get_weather2, url = url_list[3901:4300], # CHANGE ----------------------------
                           start_time = start_time_list[3901:4300])) # CHANGE ------------------------
end_time <- Sys.time()
end_time - start_time


full_boxscore_data_current <- full_boxscore_data[3901:4300,] # CHANGE--------------------------- 
full_boxscore_data_current[, "temperature_wunder"] <- as.vector(x_current[1,])
full_boxscore_data_current[, "pressure"] <- as.vector(x_current[2,])
full_boxscore_data_current[, "humidity"] <- as.vector(x_current[3,])
full_boxscore_data_current[, "time_weather"] <- as.vector(x_current[4,])

full_boxscore_data_current[, c("temperature", "temperature_wunder", "pressure", "humidity", 
                               "start_time", "time_weather")]


write.csv(full_boxscore_data_current, "full_boxscore_data4300.csv") # CHANGE ------------------------

output_current <- read.csv("full_boxscore_data4300.csv") # CHANGE ----------------------------------
output_current

# Things to test output for; missing data in temperature_wunder, pressure, humidity,
# start_time, time_weather
# These all should equal zero
mean(is.na(full_boxscore_data_current$temperature_wunder))
mean(is.na(full_boxscore_data_current$pressure))
mean(is.na(full_boxscore_data_current$humidity))
mean(is.na(full_boxscore_data_current$time_weather))

# No time difference should be greater than an hour
full_boxscore_data_current %>%
  mutate(time_diff = difftime(parse_time(time_weather, format = "%H:%M %p"),
                              start_time, units = "mins")) %>%
  arrange(desc(time_diff)) %>%
  select(time_diff, temperature, temperature_wunder, start_time, 
         time_weather, everything())

# https://www.wunderground.com/history/daily/kbos/date/2015-07-26 has no time at 8 PM,
# explaining the over an hour time difference. Same with
# https://www.wunderground.com/history/daily/kstl/date/2015-07-29

# Check a few random URLs
full_boxscore_data_current[c(5, 100, 300, 400), # -------------------------------MAYBE CHANGE---------
                           c("temperature", "temperature_wunder", "pressure", "humidity", 
                             "start_time", "time_weather")]
# Take a pic then compare to urls I already opened
url_list[c(3905, 4000, 4200, 4300)] # CHANGE-------------------------------------------------------- 

# Look at major temperature outliers
full_boxscore_data_current %>%
  mutate(temp_diff = (temperature - as.numeric(temperature_wunder))) %>%
  arrange(desc(temp_diff)) %>%
  select(temp_diff, everything())


# Choose new urls and check them

# https://www.wunderground.com/history/daily/kcle/date/2015-7-29 has 0 pressure for the 
# time collected and needs to be corrected. Look for weird outliers for all rows after
# scraping all the data0

# https://www.wunderground.com/history/daily/kdca/date/2016-5-29 time is off by 3 hours. 
# temps are similar though (between box score temp and wunderground temp)



df_test <- data.frame(p = 1:3)
df_test[, "temperature_wunder"] <- as.vector(x[1,])
df_test[, "pressure"] <- as.vector(x[2,])
df_test[, "humidity"] <- as.vector(x[3,])
df_test[, "time_weather"] <- as.vector(x[4,])
df_test[, "start_time"] <- full_boxscore_data$start_time[c(100, 110, 126)]
df_test

df_test %>%
  mutate(time_diff = difftime(parse_time(time_weather, format = "%H:%M %p"), 
                              start_time)) %>%
  arrange(desc(time_diff))

mean(is.na(df_test$temperature_wunder))
mean(is.na(df_test$humidity))
mean(is.na(df_test$pressure))
mean(is.na(df_test$time_weather))

as.vector(x[1,])
output$temperature_wunder

as.vector(x[2,])
output$pressure

as.vector(x[3,])
output$humidity




remDr$close()
rD$server$stop()
rm(rD)
rm(remDr)
gc()
# Kills java instances inside RStudio
system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)

# Consider finding closest time rather than approximating/assuming the layout








# Use find_url and line below to generate URLs for all games
find_url <- function(current_row) {
  return_char <- gsub("-0", "-", paste0(base_url, current_row["airport_code"], 
                                        "/date/", current_row["game_date"]))
  return(return_char)
}

url_list <- apply(full_boxscore_data[1:50,], 1,  find_url)

# List of starting hours to get a specific row of data from
hour_list <- hour(full_boxscore_data[1:50,]$start_time) + 1

#

get_weather2 <- function(url, hour_start) {
  library(rvest)
  #library(RSelenium)
  output_list <- numeric(3)
  remDr$navigate(url)
  html <- remDr$getPageSource()[[1]]
  
  webpage <- read_html(html) # parse HTML
  
  temp_data <- html_nodes(webpage, css = ".mat-column-temperature .wu-value-to") %>%
    html_text()
  output_list[1] <- temp_data[hour_start]
  
  pressure_data <- html_nodes(webpage, css = ".wu-unit-pressure .wu-value-to") %>%
    html_text()
  output_list[2] <- pressure_data[hour_start]
  
  humid_data <- html_nodes(webpage, css = ".wu-unit-humidity .wu-value-to") %>%
    html_text()
  output_list[3] <- humid_data[hour_start]
  
  return(output_list)
}



# Correct values are (49, 29,26, 50) and (51, 30.22, 59)
for (i in 1:2) {
  print(get_weather2(url_list[i], hour_list[i]))
}


# The problem is that, for i = 1:2, the foreach loop goes to the second url, doesn't record any
# data, then goes to the first url and gets the data from the first url twice.














# Test
vec <- get_weather2(url_list[1], hour_list[1])
vec

# Start selenium server
rD <- rsDriver(browser = "firefox", port = 4401L, verbose = F)
remDr <- rD[["client"]]

cl <- (detectCores() - 1) %>%
  makeCluster() %>%
  registerDoParallel()
# registerDoParallel() is from doParallel package
clusterEvalQ(cl, {
  library(RSelenium)
  remDr <- remoteDriver()
  remDr$open()
})

output2 <- foreach(i = 1:2, .packages = c("rvest", "RSelenium")) %dopar% {
  output_list <- numeric(3)
  hour_start <- hour_list[i]
  remDr$navigate(url_list[i])
  html <- remDr$getPageSource()[[1]]
  
  webpage <- read_html(html) # parse HTML
  
  temp_data <- html_nodes(webpage, css = ".mat-column-temperature .wu-value-to") %>%
    html_text()
  output_list[1] <- temp_data[hour_start]
  
  pressure_data <- html_nodes(webpage, css = ".wu-unit-pressure .wu-value-to") %>%
    html_text()
  output_list[2] <- pressure_data[hour_start]
  
  humid_data <- html_nodes(webpage, css = ".wu-unit-humidity .wu-value-to") %>%
    html_text()
  output_list[3] <- humid_data[hour_start]
  
  return(output_list)
}


clusterEvalQ (cl, {
  remDr$close()
})

# Maybe try parLapply() approach




# foreach approach
library(doSNOW)


cluster <- makeCluster(2, type = "SOCK")
registerDoSNOW(cluster)



start_time3 <- Sys.time()
temp_hold <- pressure_hold <- humid_hold <- numeric(50)

g1 <- foreach(i = 1:2, .combine = c) %dopar% {
  return(get_weather2(url_list[i], hour_list[i]))
}


g <- foreach (i = 1:2) %dopar% {
  return(get_weather2(url_list[i], hour_list[i]))
}

end_time3 <- Sys.time()
end_time3 - start_time3
stopCluster(cluster)

temp <- big_list[seq(1, 150, by = 3)]
press <- big_list[seq(2, 150, by = 3)]
humid <- big_list[seq(3, 150, by = 3)]




temp <- as.numeric(get_weather2(url_list[1], hour_list[1]))

cl <- makeCluster(4, "SOCK")
clusterEvalQ(cl, {library(rvest); library(RSelenium);
  rD <- rsDriver(browser = "firefox", verbose = F); 
  remDr <- rD[["client"]]})
dat <- parLapply(cl, url_list, get_weather2, hour_start = hour_list)
stopCluster(cl)






full_boxscore_data[1:2,] -> df
start_time2 <- Sys.time()
get_weather_df(df) -> output
end_time2 <- Sys.time()
end_time2 - start_time2
#stopImplicitCluster()

output[, "absolute_difference"] <- abs(output$temperature - as.numeric(output$temperature_wunder))


output %>%
  group_by(other_weather) %>%
  summarise(mean_diff = mean(absolute_difference), count = n())

# In this sample, it seems that Dome and Roof Closed are associated with the biggest errors,
# which makes sense.

ggplot(data = output, aes(x = other_weather, y = absolute_difference)) +
  geom_col()



output %>%
  arrange(desc(absolute_difference)) %>%
  select(temperature, temperature_wunder, absolute_difference, other_weather)

# Try subsetting directly from website to see which is more efficient
# Also remember to eventually convert units

remDr$close()
rD$server$stop()
rm(rD)
rm(remDr)
gc()
# Kills java instances inside RStudio
system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)








# gameday_sw value meanings
# "N" ~ missing, no pitch info
# "Y" ~ standard w/ pitch locations
# "E" ~ w/ pitch f/x
# "P" ~ for 2010, whatever that's supposed to mean







library(RSQLite)
conn <- dbConnect(SQLite(), "C:\\Users\\Ishaan\\Documents\\R\\Databases\\statcast.db") 
test <- dbGetQuery(conn, "SELECT * FROM statcast_data ORDER BY RANDOM() LIMIT 10")
dbDisconnect(conn)



library(rnoaa)
library(rgdal)

token <- "ilJCYuslMPjNwfdeyxZgQhRScZaZJkDO"
options(noaakey = token)


ncdc(datasetid='GHCND', stationid='GHCND:US1ILCK0074', startdate = "2015-05-01", enddate = "2015-06-01")



ncdc_stations(datasetid='GHCND', locationid='FIPS:12017', stationid='GHCND:USC00084289')

weather <- ncdc(datasetid = "NORMAL_DLY", data)
ncdc_stations(locationid='FIPS:12017', stationid='GHCND:USC00084289')

