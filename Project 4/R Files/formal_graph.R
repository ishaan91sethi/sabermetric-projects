# Plots some cleaned up/more formal scatter plot graphs.

formal_graph <- function(x_name, y_name, title, x, y) {
  plot <- ggplot(data = NULL, aes(x = x, y = y)) +
    geom_point() + # Next line is optional
    geom_abline(slope = 1, intercept = 0) +
    xlab(x_name) +
    ylab(y_name) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
}