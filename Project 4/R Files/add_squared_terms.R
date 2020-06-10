# Adds o-swing and f-strike squared terms to a dataframe


add_squared_terms <- function(df) {
  df <- df %>%
    mutate(o_swing2 = o_swing ^ 2, f_strike2 = f_strike ^ 2)
  return(df) # Remove intercept
}