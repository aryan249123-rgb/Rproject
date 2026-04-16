correlation_analysis <- function(data) {
  numeric_data <- data %>%
    dplyr::select(pm2_5, pm10, no2, co, aqi)
  stats::cor(numeric_data, use = "pairwise.complete.obs")
}
