# Fits a model on a given cv data set and graphs the predicted vs actual values

fit_on_cv <- function(fit, cv_data) {
  print(coef(fit))
  pred <- predict(fit, cv_data)
  print(mean((pred - cv_data$wSB_rate) ^ 2))
  
  ggplot(NULL, aes(x = pred, y = cv_data$wSB_rate)) +
    geom_point() +
    geom_smooth()
}



