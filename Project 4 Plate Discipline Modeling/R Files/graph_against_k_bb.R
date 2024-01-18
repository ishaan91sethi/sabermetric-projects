# Graphs my main effect variables against strikeout or walk rate (depending on argument) and 
# also prints the various correlations. Index is 7 for strikeout rate and 8 for walk rate

graph_against_k_bb <- function(df, index) {
  k_or_bb <- df[[index]]
  for (i in c(1:6)) {
    var <- df[[i]]
    plot <- ggplot(data = NULL, aes(x = var, y = k_or_bb)) +
      geom_point() +
      ggtitle(colnames(df)[i])
    print(plot)
    print(colnames(df[i]))
    print(cor(var, k_or_bb))
  }
} 