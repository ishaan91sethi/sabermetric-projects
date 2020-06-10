# Cleans the data by removing backticks in variable names, converting percentages into numerical variables,
# and adding a year variable for data joining. low_int and up_int are the range of variables
# that need to be converted into percentages


clean_data <- function(df, year = 2000, low_int = 3, up_int = 13) {
  df_return <- df %>%
    rename(o_swing = `O-Swing%`, z_swing = `Z-Swing%`, swing = `Swing%`, o_contact = `O-Contact%`,
           z_contact = `Z-Contact%`, contact = `Contact%`, zone = `Zone%`, f_strike = `F-Strike%`,
           swing_strike = `SwStr%`, walk_pct = `BB%`, k_pct = `K%`)
  for(i in low_int:up_int) {
    df_return[[i]] <- as.numeric(sub(" %", "", df[[i]], fixed = TRUE))
    df_return[[i]] <- df_return[[i]] / 100 # Turn into decimal rather than percentage
  }
  df_return["year"] <- year # Add year variable
  return(df_return)
}