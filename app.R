# app.R — Basic Shiny dashboard: AQI trend only
# Launch: setwd to this folder; shiny::runApp()

source(file.path(getwd(), "utils.R"), local = FALSE)
ensure_dirs()

if (!file.exists(file.path(data_dir(), "processed_air_quality.rds"))) {
  message("Running preprocessing...")
  source(file.path(getwd(), "data_preprocessing.R"), local = FALSE)
}
if (!file.exists(file.path(models_dir(), "artifacts.rds"))) {
  message("Training models (first launch)...")
  source(file.path(getwd(), "model_training.R"), local = FALSE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(dplyr)
  library(ggplot2)
})

artifacts <- readRDS(file.path(models_dir(), "artifacts.rds"))
aqi_df <- readRDS(file.path(data_dir(), "processed_air_quality.rds"))

ui <- fluidPage(
  titlePanel("Air Quality Index — India (exploratory dashboard)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      selectInput("city_filter", "City", choices = c("All cities" = "all", sort(unique(aqi_df$city))), selected = "all"),
      dateRangeInput(
        "date_rng",
        "Date range",
        start = min(aqi_df$date, na.rm = TRUE),
        end = max(aqi_df$date, na.rm = TRUE),
        min = min(aqi_df$date, na.rm = TRUE),
        max = max(aqi_df$date, na.rm = TRUE)
      ),
      selectizeInput(
        "cities_compare",
        "Cities to compare (trend)",
        choices = sort(unique(aqi_df$city)),
        selected = NULL,
        multiple = TRUE,
        options = list(placeholder = "Optional")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("AQI trend", plotlyOutput("plot_trend", height = "440px")),
        tabPanel(
          "Pollutant comparison",
          plotlyOutput("plot_bars", height = "440px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_df <- reactive({
    d <- aqi_df %>% filter(date >= input$date_rng[1], date <= input$date_rng[2])
    if (!is.null(input$city_filter) && input$city_filter != "all") {
      d <- d %>% filter(city == input$city_filter)
    }
    d
  })

  output$plot_trend <- renderPlotly({
    d <- filtered_df()
    if (length(input$cities_compare) > 0) {
      d <- d %>% filter(city %in% input$cities_compare)
    }
    if (nrow(d) < 2) {
      return(plotly_empty() %>% layout(title = "Not enough data for this selection"))
    }
    p <- ggplot(d, aes(
      x = date, y = aqi, color = city, group = city,
      text = paste0(city, "<br>", format(date, format = "%Y-%m-%d"), "<br>AQI: ", round(aqi, 1))
    )) +
      geom_line(linewidth = 0.35, alpha = 0.9) +
      geom_point(size = 0.5, alpha = 0.45) +
      scale_color_hue(c = 50, l = 55) +
      labs(x = NULL, y = "AQI", color = "City", title = "AQI over time") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
  })

  output$plot_bars <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) < 1) {
      return(plotly_empty() %>% layout(title = "No rows in range"))
    }
    long_df <- data.frame(
      pollutant = c("PM2.5", "PM10", "NO2", "CO", "SO2"),
      value = c(
        mean(d$pm25, na.rm = TRUE),
        mean(d$pm10, na.rm = TRUE),
        mean(d$no2, na.rm = TRUE),
        mean(d$co, na.rm = TRUE),
        mean(d$so2, na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    )
    p <- ggplot(long_df, aes(x = pollutant, y = value, fill = pollutant, text = paste0(pollutant, ": ", round(value, 2)))) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "Mean level (dataset units)", title = "Mean pollutants (filtered)") +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
