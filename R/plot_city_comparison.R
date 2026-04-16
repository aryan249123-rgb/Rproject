plot_city_comparison <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = reorder(city, aqi, FUN = median), y = aqi, fill = city)) +
    ggplot2::geom_boxplot(show.legend = FALSE, alpha = 0.8) +
    ggplot2::labs(title = "AQI Distribution by City", x = "City", y = "AQI") +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip()
}
