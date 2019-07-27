# Find the name associated with the pitcher ID in a dataframe in a column.
ID_to_name <- function(df)
{
  master <- read.csv("master.csv")
  id_names <- master %>%
    select(mlb_id, mlb_name) %>%
    rename(pitcher = mlb_id)
  new_df <- left_join(df, id_names, by = "pitcher") %>%
    select(pitcher, everything())
  return(new_df)
}