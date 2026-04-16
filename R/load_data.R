load_data <- function(file_path = NULL, auto_download = FALSE, download_url = "https://example.com/aqi_data.csv") {
  project_root <- getOption("aqi_project_root", default = getwd())
  resolved_path <- if (is.null(file_path)) {
    file.path(project_root, "data", "aqi_data.csv")
  } else if (grepl("^(?:[A-Za-z]:|/|\\\\\\\\)", file_path)) {
    file_path
  } else {
    file.path(project_root, file_path)
  }
  if (!is.null(file_path) && file.exists(resolved_path)) {
    return(readr::read_csv(resolved_path, show_col_types = FALSE))
  }
  if (file.exists(resolved_path)) {
    return(readr::read_csv(resolved_path, show_col_types = FALSE))
  }
  if (isTRUE(auto_download)) {
    dir.create(dirname(resolved_path), recursive = TRUE, showWarnings = FALSE)
    utils::download.file(download_url, resolved_path, mode = "wb", quiet = TRUE)
    return(readr::read_csv(resolved_path, show_col_types = FALSE))
  }
  stop("AQI dataset not found. Provide a valid CSV path or enable auto_download.")
}
