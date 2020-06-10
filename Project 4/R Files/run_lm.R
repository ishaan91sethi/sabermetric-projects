# Runs several operations on a given model built by lm(), including the MSE on the CV set,
# and a summary of the model.

run_lm <- function(lm_fit, cv_data, y_cv, walk_or_k) {
  lm_pred <- predict(lm_fit, cv_data)
  print("Mean Squared Error on CV data")
  print(mean((lm_pred - y_cv) ^ 2))
  print(summary(lm_fit))
  print(paste0("Correlation between predicted and actual : ", cor(lm_pred, y_cv)))
  # Actual versus predicted graph
  plot <- ggplot(data = NULL, aes(x = lm_pred, y = y_cv)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    xlab(paste0("Predicted ", walk_or_k, " Rate")) +
    ylab(paste0("Actuak ", walk_or_k, " Rate")) +
    ggtitle(paste0("Predicted vs Actual ", walk_or_k, " Rate")) +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  print(plot)
  
  # Residual Plot
  plot2 <- ggplot(data = NULL, aes(x = lm_fit$fitted.values, y = lm_fit$residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1.5) +
    xlab(paste0("Predicted ", walk_or_k, " Rate")) +
    ylab("Residual") +
    ggtitle("Residual Plot") +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot2)
}