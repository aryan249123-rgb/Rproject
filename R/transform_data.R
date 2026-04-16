transform_data <- function(data) {
  transformed_data <- data %>%
    dplyr::mutate(
      date = as.Date(date),
      city = as.factor(city),
      pm2_5 = as.numeric(pm2_5),
      pm10 = as.numeric(pm10),
      no2 = as.numeric(no2),
      co = as.numeric(co),
      aqi = as.numeric(aqi)
    )
  transformed_data
}
