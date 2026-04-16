plot_city_comparison <- function(data) {
  summary_data <- data %>%
    dplyr::group_by(state) %>%
    dplyr::summarise(mean_aqi = mean(aqi, na.rm = TRUE), .groups = "drop") %>%
    dplyr::slice_max(order_by = mean_aqi, n = 12) %>%
    dplyr::arrange(mean_aqi)

  ggplot2::ggplot(summary_data, ggplot2::aes(x = reorder(state, mean_aqi), y = mean_aqi, fill = mean_aqi)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
    ggplot2::scale_fill_gradient(low = "#F4C95D", high = "#C1121F") +
    ggplot2::labs(title = "States With Highest Mean AQI", x = "State", y = "Mean AQI") +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip()
}
