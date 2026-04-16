suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(lubridate)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  normalizePath(getwd())
}
options(aqi_project_root = script_dir)

source_files <- c(
  "load_data.R",
  "clean_data.R",
  "transform_data.R",
  "summary_stats.R",
  "correlation_analysis.R",
  "classify_aqi.R",
  "plot_trend.R",
  "plot_city_comparison.R",
  "plot_correlation_heatmap.R",
  "plot_scatter.R",
  "build_model.R",
  "evaluate_model.R",
  "save_outputs.R"
)
invisible(lapply(file.path(script_dir, "R", source_files), source))

args <- commandArgs(trailingOnly = TRUE)
csv_path <- if (length(args) >= 1) args[1] else NULL
auto_download <- if (length(args) >= 2) tolower(args[2]) == "true" else FALSE

aqi_data <- load_data(file_path = csv_path, auto_download = auto_download)
cleaned_data <- clean_data(aqi_data)
transformed_data <- transform_data(cleaned_data)
classified_data <- classify_aqi(transformed_data)
stats_summary <- summary_stats(classified_data)
correlation_matrix <- correlation_analysis(classified_data)
trend_plot <- plot_trend(classified_data)
city_plot <- plot_city_comparison(classified_data)
heatmap_plot <- plot_correlation_heatmap(correlation_matrix)
scatter_plot <- plot_scatter(classified_data)
model_results <- build_model(classified_data)
model_metrics <- evaluate_model(model_results)

cat("AQI Summary Statistics\n")
cat(sprintf("Mean AQI: %.2f\n", stats_summary$mean_aqi))
cat(sprintf("Max AQI: %.2f\n", stats_summary$max_aqi))
cat(sprintf("Min AQI: %.2f\n\n", stats_summary$min_aqi))

cat("Top Polluted States\n")
print(stats_summary$top_polluted_states)
cat("\nCorrelation Matrix\n")
print(round(correlation_matrix, 3))
cat("\nAQI Category Counts\n")
print(table(classified_data$aqi_category))
cat("\nModel Evaluation Metrics\n")
cat(sprintf("RMSE: %.3f\n", model_metrics$rmse))
cat(sprintf("R-squared: %.3f\n", model_metrics$r_squared))

save_outputs(
  cleaned_data = classified_data,
  plots = list(
    aqi_trend = trend_plot,
    city_comparison = city_plot,
    correlation_heatmap = heatmap_plot,
    pollutant_scatter = scatter_plot
  )
)

cat(sprintf("\nOutputs saved to: %s\n", file.path(script_dir, "output")))
