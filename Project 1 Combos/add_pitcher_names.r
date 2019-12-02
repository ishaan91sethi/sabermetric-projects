# Find the name associated with the pitcher ID in a dataframe and add the name in a column of that df.
add_pitcher_names <- function(df)
{
  master <- read.csv("master.csv")
  id_names <- master %>%
    select(mlb_id, mlb_name) %>%
    rename(pitcher = mlb_id)
  new_df <- left_join(df, id_names, by = "pitcher") %>%
    select(pitcher, everything())
  return(new_df)
}