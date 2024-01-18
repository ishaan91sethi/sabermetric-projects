# Plots avg_xwoba based on combo data on the x-axis with a selected pitcher stat column on the y-axis.
plot_graph <- function(df, pitcher_stat)
{
  ggplot(data = df, aes(x = avg_xwoba, y = pitcher_stat)) +
    geom_point() +
    geom_smooth()
}