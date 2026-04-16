clean_data <- function(data) {
  numeric_columns <- c("pm2_5", "pm10", "no2", "co", "aqi")
  cleaned_data <- data %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::mutate(city = tidyr::replace_na(city, "Unknown")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_columns), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
    dplyr::distinct()
  cleaned_data
}
