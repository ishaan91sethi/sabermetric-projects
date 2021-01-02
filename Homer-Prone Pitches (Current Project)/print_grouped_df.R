

print_grouped_df <- function(df, ...) {
  df_output <- df %>%
    group_by_(...) %>%
    summarize(avg_pred = mean(out_of_bag_pred, na.rm = TRUE)) %>%
    arrange(desc(avg_pred))
  print(df_output) 
}