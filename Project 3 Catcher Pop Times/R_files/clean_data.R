# Clears out unnecessary variables, simplifies variable names, and also adds a year variable to the dataframe,
# based on year_input argument. Also filters out players who have less throws than the min_throws argument.

clean_data <-function(df, year_input, min_throws) {
  df_return <- df %>%
    select(-"pop_2b_cs", -"pop_2b_sb", -starts_with("pop_3b")) %>%
    rename(name = catcher, top_arm_speed = maxeff_arm_2b_3b_sba, exchange = exchange_2b_3b_sba,
           throw_count = pop_2b_sba_count, pop_time = pop_2b_sba) %>%
    mutate(year = year_input) %>%
    filter(throw_count >= min_throws)
  return(df_return)
}