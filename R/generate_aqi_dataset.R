generate_aqi_dataset <- function(output_path = file.path(getOption("aqi_project_root", getwd()), "data", "aqi_data.csv")) {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
  })

  state_profiles <- tibble::tribble(
    ~state, ~zone, ~capital, ~base_aqi, ~pm25_base, ~pm10_base, ~no2_base, ~so2_base, ~co_base, ~o3_base, ~temperature_base, ~rainfall_base,
    "Andhra Pradesh", "South", "Amaravati", 112, 51, 92, 24, 11, 0.78, 27, 30, 72,
    "Arunachal Pradesh", "Northeast", "Itanagar", 58, 24, 43, 11, 4, 0.38, 20, 19, 185,
    "Assam", "Northeast", "Dispur", 88, 36, 63, 17, 7, 0.56, 23, 24, 210,
    "Bihar", "East", "Patna", 176, 82, 144, 34, 13, 1.08, 30, 27, 95,
    "Chhattisgarh", "Central", "Raipur", 122, 57, 101, 24, 14, 0.82, 26, 28, 98,
    "Goa", "West", "Panaji", 66, 26, 49, 12, 5, 0.42, 21, 29, 125,
    "Gujarat", "West", "Gandhinagar", 136, 62, 112, 28, 15, 0.90, 29, 31, 62,
    "Haryana", "North", "Chandigarh", 168, 78, 136, 33, 14, 1.04, 30, 26, 68,
    "Himachal Pradesh", "North", "Shimla", 72, 29, 51, 13, 6, 0.44, 22, 18, 88,
    "Jharkhand", "East", "Ranchi", 138, 64, 114, 27, 15, 0.88, 27, 25, 105,
    "Karnataka", "South", "Bengaluru", 92, 39, 72, 19, 8, 0.60, 24, 27, 82,
    "Kerala", "South", "Thiruvananthapuram", 61, 25, 47, 12, 5, 0.40, 21, 28, 168,
    "Madhya Pradesh", "Central", "Bhopal", 126, 58, 104, 25, 13, 0.84, 27, 28, 76,
    "Maharashtra", "West", "Mumbai", 116, 50, 90, 24, 10, 0.74, 28, 29, 96,
    "Manipur", "Northeast", "Imphal", 69, 28, 50, 12, 5, 0.43, 22, 22, 154,
    "Meghalaya", "Northeast", "Shillong", 56, 22, 40, 10, 4, 0.35, 20, 20, 240,
    "Mizoram", "Northeast", "Aizawl", 52, 21, 38, 9, 4, 0.34, 19, 21, 205,
    "Nagaland", "Northeast", "Kohima", 61, 24, 44, 11, 4, 0.37, 21, 20, 162,
    "Odisha", "East", "Bhubaneswar", 109, 48, 87, 22, 11, 0.72, 26, 29, 118,
    "Punjab", "North", "Chandigarh", 162, 76, 132, 31, 14, 1.00, 29, 24, 58,
    "Rajasthan", "North", "Jaipur", 148, 68, 125, 28, 13, 0.92, 31, 30, 42,
    "Sikkim", "Northeast", "Gangtok", 49, 19, 35, 8, 3, 0.31, 18, 16, 176,
    "Tamil Nadu", "South", "Chennai", 96, 41, 76, 20, 9, 0.64, 25, 30, 92,
    "Telangana", "South", "Hyderabad", 118, 53, 96, 23, 11, 0.76, 27, 29, 74,
    "Tripura", "Northeast", "Agartala", 74, 30, 54, 13, 5, 0.46, 22, 25, 178,
    "Uttar Pradesh", "North", "Lucknow", 184, 86, 151, 36, 15, 1.12, 31, 27, 82,
    "Uttarakhand", "North", "Dehradun", 84, 34, 61, 15, 7, 0.52, 23, 20, 102,
    "West Bengal", "East", "Kolkata", 144, 65, 118, 28, 12, 0.90, 28, 28, 128
  )

  month_index <- seq.Date(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "month")
  seasonal_map <- c(
    "01" = 1.20, "02" = 1.15, "03" = 1.05, "04" = 0.98, "05" = 0.94, "06" = 0.80,
    "07" = 0.72, "08" = 0.74, "09" = 0.82, "10" = 0.98, "11" = 1.14, "12" = 1.22
  )

  season_label <- function(date) {
    month_num <- lubridate::month(date)
    dplyr::case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Summer",
      month_num %in% c(6, 7, 8, 9) ~ "Monsoon",
      TRUE ~ "Post-Monsoon"
    )
  }

  clamp <- function(x, lower, upper) pmin(pmax(x, lower), upper)

  set.seed(20260416)

  dataset <- tidyr::crossing(state_profiles, date = month_index) %>%
    dplyr::mutate(
      month_code = format(date, "%m"),
      year_index = lubridate::year(date) - min(lubridate::year(date)),
      season = season_label(date),
      seasonal_multiplier = unname(seasonal_map[month_code]),
      trend_multiplier = 1 - (year_index * 0.012),
      temperature_c = round(temperature_base + c(-6, -4, 0, 3, 5, 3, 1, 0, 1, 0, -2, -5)[as.integer(month_code)] + rnorm(dplyr::n(), 0, 1.2), 1),
      rainfall_mm = round(clamp(rainfall_base * c(0.18, 0.16, 0.15, 0.22, 0.38, 1.05, 1.28, 1.20, 0.90, 0.42, 0.22, 0.16)[as.integer(month_code)] + rnorm(dplyr::n(), 0, 10), 4, 420), 1),
      pm2_5 = round(clamp(pm25_base * seasonal_multiplier * trend_multiplier + rnorm(dplyr::n(), 0, 4.5), 12, 180), 1),
      pm10 = round(clamp(pm10_base * seasonal_multiplier * trend_multiplier + rnorm(dplyr::n(), 0, 7.5), 22, 320), 1),
      no2 = round(clamp(no2_base * seasonal_multiplier * trend_multiplier + rnorm(dplyr::n(), 0, 2.8), 6, 80), 1),
      so2 = round(clamp(so2_base * (0.96 + (seasonal_multiplier - 1) * 0.4) * trend_multiplier + rnorm(dplyr::n(), 0, 1.6), 2, 38), 1),
      co = round(clamp(co_base * (0.94 + (seasonal_multiplier - 1) * 0.55) * trend_multiplier + rnorm(dplyr::n(), 0, 0.06), 0.2, 2.5), 2),
      o3 = round(clamp(o3_base * c(0.88, 0.94, 1.02, 1.12, 1.18, 0.96, 0.84, 0.82, 0.86, 0.94, 0.90, 0.86)[as.integer(month_code)] + rnorm(dplyr::n(), 0, 2.1), 8, 72), 1),
      aqi = round(clamp(
        0.42 * pm2_5 + 0.26 * pm10 + 0.16 * no2 + 0.07 * so2 + 18 * co + 0.14 * o3 +
          ifelse(season == "Winter", 18, ifelse(season == "Monsoon", -12, 0)) +
          ifelse(zone == "North", 10, ifelse(zone == "Northeast", -8, 0)) +
          rnorm(dplyr::n(), 0, 6),
        35, 420
      )),
      dominant_pollutant = c("PM2.5", "PM10", "NO2", "SO2", "CO", "O3")[max.col(cbind(pm2_5, pm10, no2, so2, co * 40, o3), ties.method = "first")]
    ) %>%
    dplyr::select(date, state, zone, capital, season, rainfall_mm, temperature_c, pm2_5, pm10, no2, so2, co, o3, aqi, dominant_pollutant) %>%
    dplyr::arrange(state, date)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(dataset, output_path)
  invisible(dataset)
}
