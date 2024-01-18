# Runs either ridge regression (alph = 0) or the lasso (alph = 1). Based on ISLR labs

run_ridge_lasso <- function(x_matrix, y, x_matrix_cv, y_cv, alph) {
  # Used below list for cross validating lambda. Default list isn't effective
  lam_list <- c(0, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1) 
  set.seed(1) # For reproducing built in 10 fold CV
  cv_model <- cv.glmnet(x_matrix, y, alpha = alph, standardize = FALSE, lambda = lam_list)
  plot(cv_model)
  print("Lambda value")
  best_lam <- cv_model$lambda.min
  print(best_lam)
  

  model <- glmnet(x_matrix, y, lambda = best_lam, alpha = alph, standardize = FALSE)
  pred <- predict(model, newx =  x_matrix_cv, s = best_lam)
  print(mean((pred - y_cv)^2))
  plot <- ggplot(data = NULL, aes(x = pred, y = y_cv)) +
    geom_point()
  print(plot)
  
  print(cor(pred, y_cv))
  coef(model)
}