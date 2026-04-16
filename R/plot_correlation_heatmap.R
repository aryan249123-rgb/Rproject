plot_correlation_heatmap <- function(correlation_matrix) {
  heatmap_data <- as.data.frame(as.table(correlation_matrix))
  ggplot2::ggplot(heatmap_data, ggplot2::aes(x = Var1, y = Var2, fill = Freq)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-1, 1)) +
    ggplot2::labs(title = "Correlation Heatmap", x = NULL, y = NULL, fill = "Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
