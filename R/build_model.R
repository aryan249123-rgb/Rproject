build_model <- function(data, test_size = 0.2, seed = 123) {
  set.seed(seed)
  sample_size <- nrow(data)
  test_indices <- sample(seq_len(sample_size), size = max(1, floor(sample_size * test_size)))
  train_data <- data[-test_indices, ]
  test_data <- data[test_indices, ]
  model <- stats::lm(aqi ~ pm2_5 + pm10 + no2 + so2 + co + o3 + temperature_c + rainfall_mm + state + season, data = train_data)
  predictions <- stats::predict(model, newdata = test_data)
  list(model = model, train_data = train_data, test_data = test_data, predictions = predictions)
}
