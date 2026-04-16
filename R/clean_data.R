clean_data <- function(data) {
  numeric_columns <- c("rainfall_mm", "temperature_c", "pm2_5", "pm10", "no2", "so2", "co", "o3", "aqi")
  cleaned_data <- data %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::mutate(
      state = tidyr::replace_na(state, "Unknown"),
      zone = tidyr::replace_na(zone, "Unknown"),
      capital = tidyr::replace_na(capital, "Unknown"),
      season = tidyr::replace_na(season, "Unknown"),
      dominant_pollutant = tidyr::replace_na(dominant_pollutant, "Unknown")
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_columns), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
    dplyr::distinct()
  cleaned_data
}
