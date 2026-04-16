save_outputs <- function(cleaned_data, plots, output_dir = file.path(getOption("aqi_project_root", default = getwd()), "output")) {
  if (!grepl("^(?:[A-Za-z]:|/|\\\\\\\\)", output_dir)) {
    output_dir <- file.path(getOption("aqi_project_root", default = getwd()), output_dir)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cleaned_data_export <- cleaned_data %>%
    dplyr::mutate(date = format(date, "%Y-%m-%d"))
  readr::write_csv(cleaned_data_export, file.path(output_dir, "cleaned_aqi_data.csv"))
  purrr::iwalk(plots, ~ ggplot2::ggsave(filename = file.path(output_dir, paste0(.y, ".png")), plot = .x, width = 10, height = 6, dpi = 300))
  invisible(output_dir)
}
