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
        tabPanel("AQI trend", plotlyOutput("plot_trend", height = "440px"))
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
}

shinyApp(ui, server)
