# Graphs and calculates correlations between first half and second half values for various
# plate discipline statistics

graphs_and_cors <- function(df) {
  var_names <- colnames(df)
  for (i in 3:13) {
    first_half_stat <- df[[i]]
    second_half_stat <- df[[i + 15]]
    plot <- ggplot(data = NULL, aes(x = first_half_stat, y = second_half_stat)) +
      geom_point() +
      ggtitle((colnames(df))[i])
    print(plot)
    print(var_names[[i]])
    print(cor(first_half_stat, second_half_stat))
  }
}