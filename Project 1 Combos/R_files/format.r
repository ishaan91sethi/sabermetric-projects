# Formats and groups a pitch dataframe (the "data" parameter by pitch/location combinations and filters out combinations
# with less than n instances. It is arranged in ascending order by xwoba.

format <- function(data, n)
{
  formatted_data <- data %>%
    group_by(pitch_type, zone) %>%
    summarize(avg_xwoba = mean(xwoba, na.rm = TRUE), avg_woba = mean(woba, na.rm = TRUE), count = n()) %>%
    filter(zone %in% c(1:9), count >= n) %>%
    arrange(avg_xwoba)
  return(formatted_data)
}