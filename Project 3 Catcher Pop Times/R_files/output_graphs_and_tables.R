# This function outputs various graphs and data when given a tibble with a pop_time variable.

output_graphs_and_tables <- function(arm_data_all) {
  
  pop_times <- arm_data_all$pop_time
  
  # Standard Deviation and Mean Pop Times
  sd_pop_times <- sd(pop_times)
  mean_pop_times <- mean(pop_times)
  
  # Distribution graph, using a histogram
  distr_graph <- ggplot(data = arm_data_all, mapping = aes(x = pop_time)) +
    geom_histogram(binwidth = 0.01, aes(y = ..density..)) +
    xlab("Pop Times") +
    ylab("Probability Density Percentage\n") +
    ggtitle("Distribution of Average Catcher Pop Times from 2015 to 2019") +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(distr_graph)
  
  # Test For Normality
  print("Anderson-Darling test for normality p-value")
  print(ad.test(pop_times)$p.value)
  
  # The p-value for data set 1 was 0.08358 which is over 0.05, so we do not reject the null hypothesis that
  # pop_times is approximately normal, for data set 1. For data set 2, the p-value was miniscule, but 
  # this is to be expected as normality tests tend to reject data sets with a large number of observations.
  
  # Normal Q-Q plots
  qqnorm(pop_times)
  
  
  # Distribution with Normal Curve Overlayed
  norm_overlay <- distr_graph + stat_function(fun = dnorm, args = list(mean = mean_pop_times, sd = sd_pop_times),
                                              size = 2, col = "blue")
  
  print(norm_overlay)
  
  
  pop_times <- arm_data_all$pop_time
  grades <- seq(80, 20, -10)
  
  # Old pop time grades. BA handbook gives ranges, so I just found the average of the range for the grades.
  ba_pop_times <- c(1.74, 1.795, 1.895, 1.995, 2.095, 2.195, 2.25)
  ba_mean_pop_times <- mean(ba_pop_times)
  
  # New tool grades by the empirical rule
  empir_pop_times <- quantile(pop_times, c(0.003, 0.05, 0.32, 0.5, 0.68, 0.95, 0.997))
  
  # New tool grades by standard deviation 
  # These are the pop time on the 20-80 scale based on the actual data standard deviation 
  data_pop_times <- (sd_pop_times * c(-3:3)) + mean_pop_times  
  
  
  # Normal Curves Comparing BA Pop Time Distribution to Statcast Pop Time Distributions
  normal_curve_graphs <- ggplot(data.frame(x = c(1.7, 2.3)), aes(x)) + 
    stat_function(fun = dnorm, args = list(mean = ba_mean_pop_times, sd = 0.1), size = 2, col = 'red') +
    stat_function(fun = dnorm, args = list(mean = mean_pop_times, sd = sd_pop_times), size = 2,
                  col = 'blue') +
    ylab("Probability Density Percentage\n") +
    xlab("Pop Time") +
    ggtitle("Normal Distributions of Baseball America (Red) and Statcast (Blue) Pop Times") +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(normal_curve_graphs)
  
  # Tibble with all three tool grades
  pop_time_grades_df <- tibble(grades, ba_pop_times, empir_pop_times, data_pop_times)
  pop_time_grades_df
  
}