evaluate_model <- function(model_results) {
  actual <- model_results$test_data$aqi
  predicted <- model_results$predictions
  residual_sum_squares <- sum((actual - predicted) ^ 2)
  total_sum_squares <- sum((actual - mean(actual)) ^ 2)
  list(
    rmse = sqrt(mean((actual - predicted) ^ 2)),
    r_squared = 1 - (residual_sum_squares / total_sum_squares)
  )
}
