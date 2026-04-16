classify_aqi <- function(data) {
  classified_data <- data %>%
    dplyr::mutate(
      aqi_category = dplyr::case_when(
        aqi <= 50 ~ "Good",
        aqi <= 100 ~ "Moderate",
        aqi <= 200 ~ "Poor",
        TRUE ~ "Severe"
      )
    ) %>%
    dplyr::mutate(aqi_category = factor(aqi_category, levels = c("Good", "Moderate", "Poor", "Severe")))
  classified_data
}
