# Groups a df by pitcher ID, then filters out any pitchers with less than n pitches with an attached xwoba for
# that location/pitch. Then, using the master table, find the actual player name using their ID.

group_by_pitcherID <- function(df, n) 
{
  new_df <- df %>%
    group_by(pitcher) %>%
    summarize(avg_xwoba = mean(avg_xwoba, na.rm = TRUE), avg_woba = mean(avg_woba, na.rm = TRUE), count = n()) %>%
    filter(count >= n) %>%
    arrange(avg_xwoba)
  
  master <- read.csv("C:\\Users\\Ishaan\\Documents\\R\\sabermetric-projects\\Project 1 Combos\\data_files\\master.csv",
                     header=TRUE)
  id_names <- master %>%
    select(mlb_id, mlb_name) %>%
    rename(pitcher = mlb_id)
  
  new_df <- left_join(new_df, id_names, by = "pitcher") %>%
    select(mlb_name, everything()) %>%
    select(-pitcher)
  return(new_df)
}