# app.R — Shiny dashboard: trend, map, pollutants, correlation, models, prediction
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
  library(leaflet)
  library(dplyr)
  library(ggplot2)
})

artifacts <- readRDS(file.path(models_dir(), "artifacts.rds"))
aqi_df <- readRDS(file.path(data_dir(), "processed_air_quality.rds"))

pollutant_ranges <- aqi_df %>%
  summarise(
    pm25 = max(pm25, na.rm = TRUE),
    pm10 = max(pm10, na.rm = TRUE),
    no2 = max(no2, na.rm = TRUE),
    co = max(co, na.rm = TRUE),
    so2 = max(so2, na.rm = TRUE)
  )
pollutant_ranges <- as.list(pollutant_ranges)
for (nm in names(pollutant_ranges)) {
  pollutant_ranges[[nm]] <- max(10, ceiling(pollutant_ranges[[nm]] * 1.1))
}

aqi_css <- HTML("
body { font-family: 'Segoe UI', system-ui, sans-serif; background: #f4f6f8; }
.sidebar-panel { background: #fff !important; border-radius: 10px; border: 1px solid #e2e6ea; }
.section-card { background: #fff; border-radius: 10px; border: 1px solid #e2e6ea; padding: 1rem; margin-bottom: 0.75rem; }
.section-title { font-weight: 600; margin-bottom: 0.5rem; color: #1a3d2e; }
.prediction-box { border-radius: 10px; padding: 0.85rem; margin-top: 0.5rem; font-weight: 600; text-align: center; }
")

ui <- fluidPage(
  tags$head(tags$style(aqi_css)),
  titlePanel("Air Quality Index — India · dashboard"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
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
        "City comparison (trend)",
        choices = sort(unique(aqi_df$city)),
        selected = NULL,
        multiple = TRUE,
        options = list(placeholder = "Optional")
      ),
      hr(),
      h4("Predict AQI"),
      radioButtons("model_pick", "Model", choices = c("Best (auto)" = "best", "Linear regression" = "lm", "Random forest" = "rf"), selected = "best", inline = TRUE),
      sliderInput("in_pm25", "PM2.5 (µg/m³)", min = 0, max = pollutant_ranges$pm25, value = round(median(aqi_df$pm25))),
      sliderInput("in_pm10", "PM10 (µg/m³)", min = 0, max = pollutant_ranges$pm10, value = round(median(aqi_df$pm10))),
      sliderInput("in_no2", "NO2 (µg/m³)", min = 0, max = pollutant_ranges$no2, value = round(median(aqi_df$no2))),
      sliderInput("in_co", "CO (mg/m³)", min = 0, max = max(5, pollutant_ranges$co), value = round(median(aqi_df$co), 2), step = 0.1),
      sliderInput("in_so2", "SO2 (µg/m³)", min = 0, max = pollutant_ranges$so2, value = round(median(aqi_df$so2))),
      uiOutput("prediction_ui"),
      hr(),
      downloadButton("dl_report", "Download summary report", class = "btn-primary", style = "width:100%;")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "AQI trend",
          div(class = "section-card", div(class = "section-title", "AQI over time"), plotlyOutput("plot_trend", height = "420px"))
        ),
        tabPanel(
          "Pollution map",
          div(class = "section-card", div(class = "section-title", "Mean AQI by city"), leafletOutput("map_aqi", height = "460px"))
        ),
        tabPanel(
          "Pollutant comparison",
          div(class = "section-card", div(class = "section-title", "Mean pollutant levels"), plotlyOutput("plot_bars", height = "420px"))
        ),
        tabPanel(
          "Correlation",
          div(class = "section-card", div(class = "section-title", "Correlation heatmap"), plotlyOutput("plot_cor", height = "420px"))
        ),
        tabPanel(
          "Model performance",
          div(class = "section-card", div(class = "section-title", "Test metrics"), tableOutput("tbl_metrics_test")),
          div(class = "section-card", div(class = "section-title", "Cross-validation summary"), tableOutput("tbl_metrics_cv"))
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
      labs(x = NULL, y = "AQI", color = "City") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
  })

  output$map_aqi <- renderLeaflet({
    d <- filtered_df()
    coords <- default_city_coords()
    summ <- d %>%
      group_by(city) %>%
      summarise(mean_aqi = mean(aqi, na.rm = TRUE), .groups = "drop") %>%
      left_join(coords, by = "city")
    miss <- is.na(summ$lat)
    if (any(miss)) {
      other <- coords[coords$city == "Other", ]
      summ$lat[miss] <- other$lat[1]
      summ$lng[miss] <- other$lng[1]
    }
    summ$color <- aqi_category(summ$mean_aqi)$color
    leaflet(summ) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat,
        radius = ~pmax(8, mean_aqi / 8),
        color = ~color,
        fillColor = ~color,
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1,
        label = ~paste0(city, ": AQI ", round(mean_aqi, 1))
      ) %>%
      fitBounds(
        lng1 = min(summ$lng, na.rm = TRUE) - 2,
        lat1 = min(summ$lat, na.rm = TRUE) - 2,
        lng2 = max(summ$lng, na.rm = TRUE) + 2,
        lat2 = max(summ$lat, na.rm = TRUE) + 2
      )
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
      labs(x = NULL, y = "Mean level (dataset units)") +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = "text")
  })

  output$plot_cor <- renderPlotly({
    d <- filtered_df()
    num_cols <- c("pm25", "pm10", "no2", "co", "so2", "aqi")
    cm <- cor(d[, num_cols], use = "pairwise.complete.obs")
    lbl <- c("PM2.5", "PM10", "NO2", "CO", "SO2", "AQI")
    dimnames(cm) <- list(lbl, lbl)
    plot_ly(
      x = colnames(cm), y = rownames(cm), z = cm,
      type = "heatmap",
      colorscale = list(c(0, "#3b76bd"), c(0.5, "#f7f7f7"), c(1, "#c0392b")),
      zmin = -1, zmax = 1,
      colorbar = list(title = "r")
    ) %>%
      layout(xaxis = list(side = "bottom"), yaxis = list(autorange = "reversed"))
  })

  output$tbl_metrics_test <- renderTable({
    artifacts$metrics_test
  }, striped = TRUE, spacing = "s", width = "100%")

  output$tbl_metrics_cv <- renderTable({
    artifacts$metrics_resample
  }, striped = TRUE, spacing = "s", width = "100%")

  pred_reactive <- reactive({
    new_row <- data.frame(
      pm25 = input$in_pm25,
      pm10 = input$in_pm10,
      no2 = input$in_no2,
      co = input$in_co,
      so2 = input$in_so2
    )
    pick <- input$model_pick
    fit <- if (pick == "best") {
      if (artifacts$best == "rf") artifacts$fit_rf else artifacts$fit_lm
    } else if (pick == "rf") {
      artifacts$fit_rf
    } else {
      artifacts$fit_lm
    }
    p <- as.numeric(predict(fit, newdata = new_row))
    p <- p[!is.na(p)][1]
    list(value = p, fit_name = pick)
  })

  output$prediction_ui <- renderUI({
    pr <- pred_reactive()
    val <- pr$value
    if (is.null(val) || is.na(val)) {
      return(div(class = "prediction-box", style = "background:#ddd;", "Unable to predict"))
    }
    ac <- aqi_category(val)
    alert <- aqi_alert_message(val)
    div(
      div(
        class = "prediction-box",
        style = paste0("background:", ac$color[1], ";color:#fff;"),
        paste0("Predicted AQI: ", round(val, 1))
      ),
      div(style = "margin-top:0.4rem;font-weight:600;text-align:center;", ac$category[1]),
      div(style = "color:#555;font-size:0.85rem;margin-top:0.25rem;text-align:center;", alert)
    )
  })

  output$dl_report <- downloadHandler(
    filename = function() paste0("aqi_report_", Sys.Date(), ".txt"),
    content = function(file) {
      d <- filtered_df()
      pr <- pred_reactive()
      ac <- aqi_category(pr$value)
      lines <- c(
        "AQI Dashboard — summary report",
        paste("Generated:", format(Sys.time(), format = "%Y-%m-%d %H:%M")),
        "",
        paste("Date range:", input$date_rng[1], "to", input$date_rng[2]),
        paste("City filter:", input$city_filter),
        "",
        "Filtered n:", nrow(d),
        paste("Mean AQI:", round(mean(d$aqi, na.rm = TRUE), 2)),
        "",
        capture.output(print(artifacts$metrics_test)),
        "",
        paste("Predicted AQI:", round(pr$value, 2), "|", ac$category[1])
      )
      writeLines(lines, file)
    }
  )
}

shinyApp(ui, server)
