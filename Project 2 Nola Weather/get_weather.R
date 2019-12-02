# Returns weather for the dataframe nola_data
# round_date from lubridate rounds times
get_weather <- function(df) {
  row_count <- nrow(df)
  dates <- df[["Date"]]
  station_codes <- df[["airport_codes"]]
  day_night <- df[["DayNight"]]
  # df_with_weather <- df # Dataframe to be returned
  # weather_df holds summation of weather data
  weather_df <- tibble(valid = as.Date(character()), tmpf = double(), relh = double())
  # Loops over each game
  for (i in 1:row_count) {
    weather_data <- riem_measures(station = station_codes[i], date_start = dates[i], date_end = dates[i])
    # Omit any observations w/o temperature or humidity
    weather_data <- weather_data %>%
      filter(!is.na(tmpf), !is.na(relh))
    # Converted the time when the weather data was recorded into minutes to help with future calculations.
    time <- (60 * hour(weather_data[["valid"]])) + minute(weather_data[["valid"]])
    # Remove missing values from time vector
    time <- time[!is.na(time)]
    if (length(time) == 0) {
      next()
    }
    # Based on times vector I extracted, I need to figure out how to extract the corresponding
    # row of weather_data (with only the variables I need) and also what if two rows have the same hour
    
    # The start_time_minutes variable measures an approximate of the game start time (in minutes),
    # based on DayNight variable in nola_data. Its initialization assumes the start time is 1 PM on a day game. 
    # The if statement following the minutes declaration fixes this if it is in fact a night game.
    start_time_minutes <- 780 # 60 (minutes in an hour) * 13 (or 1 PM) = 780
    if (day_night[i] == "N") {
      start_time_minutes <- 1140 # 60 (minutes in an hour) * 19 (or 7PM) = 1140
    }
    weather_row_count <- nrow(weather_data)
    # Holds the lowest difference between the game start time and weather measurement time
    diff <- abs(start_time_minutes - time[1])
    # Holds the observation/row in the daily weather data that is closest to game time.
    closest_row <- 1
    for (j in 2:weather_row_count) {
      # Holds the difference between the game start time and weather measurement time of the current row.
      diff2 <- abs(start_time_minutes - time[j])
      if (diff2 < diff) {
        diff <- diff2
        closest_row <- j
      }
        
    }
    # row_to_add stores the row from the daily weather data table that is closest to the start time and
    # that will be added to the original input dataframe.
    row_to_add <- weather_data[closest_row, ]
    # Select for necessary variables: temperature (tmpf variable) and relative humidity (relh)
    row_to_add <- row_to_add %>%
      select(valid, tmpf, relh) %>%
      mutate(valid = as_date(valid))
    
    weather_df <- weather_df %>%
      rbind(row_to_add)
    # df_with_weather <- full_join(df_with_weather, row_to_add, by = c("Date" = "valid"))
  }
  df_with_weather <- full_join(df, weather_df, by = c("Date" = "valid"))
  return(df_with_weather)
}