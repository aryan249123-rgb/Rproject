transform_data <- function(data) {
  transformed_data <- data %>%
    dplyr::mutate(
      date = as.Date(date),
      state = as.factor(state),
      zone = as.factor(zone),
      capital = as.factor(capital),
      season = factor(season, levels = c("Winter", "Summer", "Monsoon", "Post-Monsoon")),
      dominant_pollutant = as.factor(dominant_pollutant),
      rainfall_mm = as.numeric(rainfall_mm),
      temperature_c = as.numeric(temperature_c),
      pm2_5 = as.numeric(pm2_5),
      pm10 = as.numeric(pm10),
      no2 = as.numeric(no2),
      so2 = as.numeric(so2),
      co = as.numeric(co),
      o3 = as.numeric(o3),
      aqi = as.numeric(aqi)
    )
  transformed_data
}
