options(aqi_project_root = normalizePath(getwd()))

suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(ggplot2)
  library(lubridate)
})

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
  "evaluate_model.R"
)
invisible(lapply(file.path(getOption("aqi_project_root"), "R", source_files), source))

run_aqi_pipeline <- function() {
  raw_data <- load_data()
  cleaned_data <- clean_data(raw_data)
  transformed_data <- transform_data(cleaned_data)
  classified_data <- classify_aqi(transformed_data)
  stats_summary <- summary_stats(classified_data)
  correlation_matrix <- correlation_analysis(classified_data)
  model_results <- build_model(classified_data)
  model_metrics <- evaluate_model(model_results)

  list(
    data = classified_data,
    stats = stats_summary,
    correlation_matrix = correlation_matrix,
    trend_plot = plot_trend(classified_data),
    city_plot = plot_city_comparison(classified_data),
    heatmap_plot = plot_correlation_heatmap(correlation_matrix),
    scatter_plot = plot_scatter(classified_data),
    model_metrics = model_metrics
  )
}

dashboard_data <- run_aqi_pipeline()

status_smiley <- function(mean_aqi) {
  dplyr::case_when(
    mean_aqi <= 50 ~ ":) Good",
    mean_aqi <= 100 ~ ":| Moderate",
    mean_aqi <= 200 ~ ":( Poor",
    TRUE ~ "!! Severe"
  )
}

ui <- fluidPage(
  titlePanel("India State Air Quality Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Dashboard Summary"),
      p("Monthly state-level AQI and pollutant trends across India."),
      p(strong("Status:"), status_smiley(dashboard_data$stats$mean_aqi)),
      p(strong("States covered:"), dplyr::n_distinct(dashboard_data$data$state)),
      p(strong("Date range:"),
        paste(
          format(min(dashboard_data$data$date), "%d %b %Y"),
          "to",
          format(max(dashboard_data$data$date), "%d %b %Y")
        )
      ),
      selectInput("selected_state", "Pick a state", choices = sort(unique(as.character(dashboard_data$data$state))), selected = "Maharashtra")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          fluidRow(
            column(4, wellPanel(h4("Mean AQI"), textOutput("mean_aqi"))),
            column(4, wellPanel(h4("Max AQI"), textOutput("max_aqi"))),
            column(4, wellPanel(h4("Min AQI"), textOutput("min_aqi")))
          ),
          h4("Top Polluted States"),
          tableOutput("top_states"),
          h4("AQI Category Count"),
          tableOutput("aqi_categories")
        ),
        tabPanel(
          "State Explorer",
          fluidRow(
            column(6, wellPanel(h4("Selected State Mean AQI"), textOutput("selected_state_mean_aqi"))),
            column(6, wellPanel(h4("Dominant Pollutant"), textOutput("selected_state_dominant_pollutant")))
          ),
          h4("Average Pollutant Profile"),
          plotOutput("selected_state_pollutants"),
          h4("Monthly AQI Trend"),
          plotOutput("selected_state_trend")
        ),
        tabPanel(
          "Visualizations",
          h4("AQI Trend"),
          plotOutput("trend_plot"),
          h4("State Comparison"),
          plotOutput("city_plot"),
          h4("Correlation Heatmap"),
          plotOutput("heatmap_plot"),
          h4("Pollutant Scatter"),
          plotOutput("scatter_plot")
        ),
        tabPanel(
          "Model Results",
          fluidRow(
            column(6, wellPanel(h4("RMSE"), textOutput("rmse"))),
            column(6, wellPanel(h4("R-squared"), textOutput("r_squared")))
          )
        ),
        tabPanel(
          "Data",
          h4("Cleaned AQI Data"),
          tableOutput("cleaned_data")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  cleaned_data_view <- dashboard_data$data %>%
    dplyr::mutate(date = format(date, "%Y-%m-%d"))

  selected_state_data <- reactive({
    dashboard_data$data %>%
      dplyr::filter(state == input$selected_state)
  })

  output$mean_aqi <- renderText(sprintf("%.2f", dashboard_data$stats$mean_aqi))
  output$max_aqi <- renderText(sprintf("%.2f", dashboard_data$stats$max_aqi))
  output$min_aqi <- renderText(sprintf("%.2f", dashboard_data$stats$min_aqi))
  output$rmse <- renderText(sprintf("%.3f", dashboard_data$model_metrics$rmse))
  output$r_squared <- renderText(sprintf("%.3f", dashboard_data$model_metrics$r_squared))

  output$top_states <- renderTable(dashboard_data$stats$top_polluted_states)
  output$aqi_categories <- renderTable(as.data.frame(table(dashboard_data$data$aqi_category)))
  output$cleaned_data <- renderTable(cleaned_data_view)

  output$selected_state_mean_aqi <- renderText({
    sprintf("%.2f", mean(selected_state_data()$aqi, na.rm = TRUE))
  })
  output$selected_state_dominant_pollutant <- renderText({
    names(sort(table(selected_state_data()$dominant_pollutant), decreasing = TRUE))[1]
  })
  output$selected_state_pollutants <- renderPlot({
    pollutant_means <- selected_state_data() %>%
      dplyr::summarise(
        `PM2.5` = mean(pm2_5, na.rm = TRUE),
        PM10 = mean(pm10, na.rm = TRUE),
        NO2 = mean(no2, na.rm = TRUE),
        SO2 = mean(so2, na.rm = TRUE),
        CO = mean(co, na.rm = TRUE),
        O3 = mean(o3, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "pollutant", values_to = "value")

    ggplot(pollutant_means, aes(x = reorder(pollutant, value), y = value, fill = pollutant)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      coord_flip() +
      labs(title = paste("Average Pollutant Levels in", input$selected_state), x = "Pollutant", y = "Mean Level") +
      theme_minimal()
  })
  output$selected_state_trend <- renderPlot({
    trend_data <- selected_state_data() %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(mean_aqi = mean(aqi, na.rm = TRUE), .groups = "drop")

    ggplot(trend_data, aes(x = date, y = mean_aqi)) +
      geom_line(color = "#9A031E", linewidth = 1) +
      geom_point(color = "#005F73", size = 1.8) +
      labs(title = paste("Monthly AQI Trend for", input$selected_state), x = "Month", y = "Mean AQI") +
      theme_minimal()
  })
  output$trend_plot <- renderPlot(dashboard_data$trend_plot)
  output$city_plot <- renderPlot(dashboard_data$city_plot)
  output$heatmap_plot <- renderPlot(dashboard_data$heatmap_plot)
  output$scatter_plot <- renderPlot(dashboard_data$scatter_plot)
}

shinyApp(ui = ui, server = server)
