summary_stats <- function(data) {
  city_rankings <- data %>%
    dplyr::group_by(city) %>%
    dplyr::summarise(mean_aqi = mean(aqi, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(mean_aqi))
  list(
    mean_aqi = mean(data$aqi, na.rm = TRUE),
    max_aqi = max(data$aqi, na.rm = TRUE),
    min_aqi = min(data$aqi, na.rm = TRUE),
    top_polluted_cities = dplyr::slice_head(city_rankings, n = 5)
  )
}
