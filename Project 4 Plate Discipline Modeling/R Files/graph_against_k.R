# Graphs my main effect variables against strikeout rate and also prints the various correlations

graph_against_k <- function(df) {
  k_pct <- df[[7]]
  for (i in c(1:6)) {
    var <- df[[i]]
    plot <- ggplot(data = NULL, aes(x = var, y = k_pct)) +
      geom_point() +
      ggtitle(colnames(df)[i])
    print(plot)
    print(colnames(df[i]))
    print(cor(var, k_pct))
  }
} 