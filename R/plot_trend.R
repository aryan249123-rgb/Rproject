plot_trend <- function(data) {
  trend_data <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(mean_aqi = mean(aqi, na.rm = TRUE), .groups = "drop")
  ggplot2::ggplot(trend_data, ggplot2::aes(x = date, y = mean_aqi)) +
    ggplot2::geom_line(color = "#0072B2", linewidth = 1) +
    ggplot2::geom_point(color = "#D55E00", size = 2) +
    ggplot2::labs(title = "Average AQI Over Time", x = "Date", y = "Average AQI") +
    ggplot2::theme_minimal()
}
