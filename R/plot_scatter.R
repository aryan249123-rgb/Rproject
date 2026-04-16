plot_scatter <- function(data) {
  scatter_data <- data %>%
    tidyr::pivot_longer(cols = c(pm2_5, pm10, no2, co), names_to = "pollutant", values_to = "value")
  ggplot2::ggplot(scatter_data, ggplot2::aes(x = value, y = aqi, color = pollutant)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    ggplot2::facet_wrap(~ pollutant, scales = "free_x") +
    ggplot2::labs(title = "Pollutants vs AQI", x = "Pollutant Level", y = "AQI") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}
