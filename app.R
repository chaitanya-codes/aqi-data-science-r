# app.R — Interactive AQI dashboard (Shiny + bslib + Plotly + Leaflet)
# Launch: set working directory to this folder, then shiny::runApp()

# ---- Bootstrap data & models if missing ----
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
  library(bslib)
  library(bsicons)
  library(plotly)
  library(leaflet)
  library(dplyr)
  library(ggplot2)
})

artifacts <- readRDS(file.path(models_dir(), "artifacts.rds"))
aqi_df <- readRDS(file.path(data_dir(), "processed_air_quality.rds"))

best_fit <- if (artifacts$best == "rf") artifacts$fit_rf else artifacts$fit_lm

# ---- Dark theme + AQI accent palette (Good → Hazard) ----
aqi_theme <- bs_theme(
  version = 5,
  bootswatch = "cyborg",
  primary = "#00A86B",
  secondary = "#e6c200",
  success = "#00A86B",
  warning = "#FF8C00",
  danger = "#E63946",
  info = "#4cc9f0",
  `navbar-bg` = "#0d1117",
  `body-bg` = "#0f1419",
  `body-color` = "#e8eaed",
  `card-bg` = "#1a1f26",
  `card-border-color` = "#2d3540",
  `input-bg` = "#252b35",
  `input-border-color` = "#3d4654",
  `input-color` = "#f0f2f5"
)

dash_css <- HTML("
:root {
  --aqi-good: #00A86B;
  --aqi-mod: #e6c200;
  --aqi-orange: #FF8C00;
  --aqi-red: #E63946;
  --aqi-hazard: #7E0023;
  --card-shadow: 0 4px 24px rgba(0,0,0,0.35);
  --card-radius: 14px;
}
.kpi-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
  gap: 1rem;
  margin-bottom: 1.25rem;
}
.kpi-card {
  background: linear-gradient(145deg, #1e252e 0%, #1a1f26 100%);
  border-radius: var(--card-radius);
  padding: 1rem 1.15rem;
  border: 1px solid #2d3540;
  box-shadow: var(--card-shadow);
  transition: transform 0.18s ease, box-shadow 0.18s ease, border-color 0.18s ease;
}
.kpi-card:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 32px rgba(0,0,0,0.45);
}
.kpi-card.ac-good { border-left: 4px solid var(--aqi-good); }
.kpi-card.ac-mod { border-left: 4px solid var(--aqi-mod); }
.kpi-card.ac-orange { border-left: 4px solid var(--aqi-orange); }
.kpi-card.ac-red { border-left: 4px solid var(--aqi-red); }
.kpi-card.ac-neutral { border-left: 4px solid #4cc9f0; }
.kpi-label {
  font-size: 0.72rem;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: #9aa5b4;
  margin-bottom: 0.35rem;
  font-weight: 600;
}
.kpi-value {
  font-size: 1.65rem;
  font-weight: 700;
  color: #f0f4f8;
  line-height: 1.15;
}
.kpi-sub { font-size: 0.75rem; color: #7d8a9a; margin-top: 0.25rem; }
.viz-card {
  border-radius: var(--card-radius) !important;
  border: 1px solid #2d3540 !important;
  box-shadow: var(--card-shadow) !important;
  overflow: hidden;
  transition: box-shadow 0.2s ease;
}
.viz-card:hover { box-shadow: 0 6px 28px rgba(0,0,0,0.4) !important; }
.viz-card .card-header {
  font-weight: 600;
  font-size: 0.95rem;
  padding: 0.85rem 1.1rem;
  background: #161b22 !important;
  border-bottom: 1px solid #2d3540 !important;
  color: #e8eaed !important;
}
.sidebar-section-title {
  font-size: 0.7rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: #7d8a9a;
  font-weight: 700;
  margin: 1.1rem 0 0.65rem 0;
  padding-top: 0.5rem;
  border-top: 1px solid #2d3540;
}
.sidebar-section-title.first { border-top: none; padding-top: 0; margin-top: 0; }
.control-gap .form-group, .control-gap .shiny-input-container { margin-bottom: 1.15rem !important; }
.prediction-box {
  border-radius: 12px;
  padding: 1rem 1.1rem;
  margin-top: 0.75rem;
  font-weight: 600;
  text-align: center;
  box-shadow: 0 4px 16px rgba(0,0,0,0.3);
}
.metrics-table-wrap { overflow-x: auto; border-radius: 10px; border: 1px solid #2d3540; }
table.metrics-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.88rem;
}
table.metrics-table th {
  background: #161b22;
  color: #9aa5b4;
  text-align: left;
  padding: 0.65rem 0.85rem;
  font-weight: 600;
  text-transform: uppercase;
  font-size: 0.68rem;
  letter-spacing: 0.05em;
}
table.metrics-table td {
  padding: 0.6rem 0.85rem;
  border-top: 1px solid #2d3540;
  color: #e8eaed;
}
table.metrics-table tr.row-best td {
  background: rgba(0, 168, 107, 0.12);
  font-weight: 600;
  color: #7bed9f;
}
table.metrics-table tr:hover td { background: rgba(255,255,255,0.03); }
.nav-tabs .nav-link {
  border-radius: 10px 10px 0 0 !important;
  margin-right: 4px;
  padding: 0.55rem 1rem !important;
  font-weight: 500;
  transition: background 0.15s ease, color 0.15s ease;
}
.nav-tabs .nav-link.active {
  background: #1a1f26 !important;
  border-color: #2d3540 #2d3540 #1a1f26 !important;
  color: #7bed9f !important;
}
")

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

# ---- ggplot theme for dark Plotly charts (visual only) ----
theme_aqi_dark <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "#1a1f26", color = NA),
      panel.grid.major = element_line(color = "#2d3540", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#c8d0da"),
      axis.text = element_text(color = "#9aa5b4"),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom"
    )
}

sidebar_controls <- sidebar(
  title = tagList(bs_icon("sliders"), " Controls"),
  width = 340,
  bg = "#12171d",
  fg = "#e8eaed",
  class = "border-end border-secondary",
  collapsible = TRUE,
  open = list(desktop = TRUE, mobile = "always"),
  div(
    class = "control-gap",
    div(class = "sidebar-section-title first", "Scope"),
    selectInput(
      "city_filter",
      label = tagList(icon("map-marker-alt"), " City filter"),
      choices = c("All cities" = "all", sort(unique(aqi_df$city))),
      selected = "all"
    ),
    dateRangeInput(
      "date_rng",
      label = tagList(icon("calendar"), " Date range"),
      start = min(aqi_df$date, na.rm = TRUE),
      end = max(aqi_df$date, na.rm = TRUE),
      min = min(aqi_df$date, na.rm = TRUE),
      max = max(aqi_df$date, na.rm = TRUE)
    ),
    selectizeInput(
      "cities_compare",
      label = tagList(icon("layer-group"), " City comparison (trend)"),
      choices = sort(unique(aqi_df$city)),
      selected = NULL,
      multiple = TRUE,
      options = list(placeholder = "Optional multi-city overlay")
    ),
    div(class = "sidebar-section-title", "Model & predictors"),
    radioButtons(
      "model_pick",
      label = tagList(icon("microchip"), " Prediction model"),
      choices = c("Best (auto)" = "best", "Linear regression" = "lm", "Random forest" = "rf"),
      selected = "best"
    ),
    tags$div(
      title = "Fine particulate matter ≤2.5 µm; major driver of health risk in Indian urban data.",
      sliderInput("in_pm25", "PM2.5 (µg/m³)", min = 0, max = pollutant_ranges$pm25, value = round(median(aqi_df$pm25)))
    ),
    tags$div(
      title = "Coarser particles; often correlated with PM2.5 in CPCB city-day records.",
      sliderInput("in_pm10", "PM10 (µg/m³)", min = 0, max = pollutant_ranges$pm10, value = round(median(aqi_df$pm10)))
    ),
    tags$div(
      title = "Nitrogen dioxide, traffic and combustion indicator (µg/m³).",
      sliderInput("in_no2", "NO2 (µg/m³)", min = 0, max = pollutant_ranges$no2, value = round(median(aqi_df$no2)))
    ),
    tags$div(
      title = "Carbon monoxide as reported in the dataset (check units in source).",
      sliderInput("in_co", "CO (mg/m³)", min = 0, max = max(5, pollutant_ranges$co), value = round(median(aqi_df$co), 2), step = 0.1)
    ),
    tags$div(
      title = "Sulfur dioxide from industrial and combustion sources.",
      sliderInput("in_so2", "SO2 (µg/m³)", min = 0, max = pollutant_ranges$so2, value = round(median(aqi_df$so2)))
    ),
    uiOutput("prediction_ui"),
    div(class = "sidebar-section-title", "Export"),
    downloadButton("dl_report", tagList(icon("download"), " Download report"), class = "btn-outline-success w-100")
  )
)

ui <- page_navbar(
  title = tags$div(
    class = "d-flex flex-column py-1",
    tags$div(
      class = "d-flex align-items-center gap-2",
      bs_icon("wind"),
      tags$span("India AQI Analytics", class = "fw-semibold fs-5")
    ),
    tags$span(class = "text-secondary small", "CPCB-style city-day data · exploration & prediction")
  ),
  theme = aqi_theme,
  fillable = TRUE,
  navbar_options = navbar_options(bg = "#0d1117", theme = "dark", underline = TRUE),
  header = tags$head(tags$style(dash_css)),
  nav_spacer(),
  nav_panel(
    value = "dashboard",
    title = tagList(icon("tachometer-alt"), "Dashboard"),
    layout_sidebar(
      sidebar = sidebar_controls,
      fillable = TRUE,
      div(
        class = "p-2 p-md-3",
        uiOutput("kpi_cards"),
        navset_card_tab(
          id = "tabs",
          full_screen = TRUE,
          nav_panel(
            title = tagList(icon("chart-line"), "AQI trend"),
            card(
              class = "viz-card m-0 border-0 shadow-none",
              card_header("AQI over time (interactive Plotly)"),
              card_body(class = "p-2 pt-0", plotlyOutput("plot_trend", height = "420px"))
            )
          ),
          nav_panel(
            title = tagList(icon("map-marked-alt"), "Pollution map"),
            card(
              class = "viz-card m-0 border-0 shadow-none",
              card_header("Mean AQI by city (Leaflet)"),
              card_body(class = "p-2 pt-0", leafletOutput("map_aqi", height = "480px"))
            )
          ),
          nav_panel(
            title = tagList(icon("chart-bar"), "Pollutant comparison"),
            card(
              class = "viz-card m-0 border-0 shadow-none",
              card_header("Average pollutant levels (filtered period)"),
              card_body(class = "p-2 pt-0", plotlyOutput("plot_bars", height = "420px"))
            )
          ),
          nav_panel(
            title = tagList(icon("th"), "Correlation"),
            card(
              class = "viz-card m-0 border-0 shadow-none",
              card_header("Pollutant & AQI correlation heatmap"),
              card_body(class = "p-2 pt-0", plotlyOutput("plot_cor", height = "440px"))
            )
          ),
          nav_panel(
            title = tagList(icon("flask"), "Model performance"),
            card(
              class = "viz-card m-0 border-0 shadow-none",
              card_header("Test-set RMSE comparison"),
              card_body(class = "p-2 pt-0", plotlyOutput("plot_rmse_compare", height = "280px"))
            ),
            card(
              class = "viz-card mt-3 border-0 shadow-none",
              card_header("Hold-out test metrics"),
              card_body(class = "p-0", uiOutput("tbl_metrics_test_ui"))
            ),
            card(
              class = "viz-card mt-3 border-0 shadow-none",
              card_header("Cross-validation summary (best tune per model)"),
              card_body(class = "p-0", uiOutput("tbl_metrics_cv_ui"))
            )
          )
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

  # ---- KPI row (filtered data) ----
  output$kpi_cards <- renderUI({
    d <- filtered_df()
    n <- nrow(d)
    if (n < 1L) {
      return(div(class = "alert alert-warning", "No rows match the current filters."))
    }
    avg_aqi <- mean(d$aqi, na.rm = TRUE)
    max_aqi <- max(d$aqi, na.rm = TRUE)
    min_aqi <- min(d$aqi, na.rm = TRUE)
    n_cities <- length(unique(d$city))
    cls_avg <- if (avg_aqi <= 50) "ac-good" else if (avg_aqi <= 100) "ac-mod" else if (avg_aqi <= 150) "ac-orange" else if (avg_aqi <= 200) "ac-red" else "ac-orange"
    cls_max <- if (max_aqi <= 50) "ac-good" else if (max_aqi <= 100) "ac-mod" else if (max_aqi <= 150) "ac-orange" else if (max_aqi <= 200) "ac-red" else "ac-red"

    div(
      class = "kpi-grid",
      div(
        class = paste("kpi-card", cls_avg),
        div(class = "kpi-label", icon("chart-simple"), " Average AQI"),
        div(class = "kpi-value", round(avg_aqi, 1)),
        div(class = "kpi-sub", "Mean over selection")
      ),
      div(
        class = paste("kpi-card", cls_max),
        div(class = "kpi-label", icon("arrow-trend-up"), " Maximum AQI"),
        div(class = "kpi-value", round(max_aqi, 1)),
        div(class = "kpi-sub", "Peak in selection")
      ),
      div(
        class = "kpi-card ac-good",
        div(class = "kpi-label", icon("arrow-trend-down"), " Minimum AQI"),
        div(class = "kpi-value", round(min_aqi, 1)),
        div(class = "kpi-sub", "Lowest in selection")
      ),
      div(
        class = "kpi-card ac-neutral",
        div(class = "kpi-label", icon("city"), " Cities"),
        div(class = "kpi-value", n_cities),
        div(class = "kpi-sub", paste0(format(n, big.mark = ","), " obs. in selection"))
      )
    )
  })

  output$plot_trend <- renderPlotly({
    d <- filtered_df()
    if (length(input$cities_compare) > 0) {
      d <- d %>% filter(city %in% input$cities_compare)
    }
    if (nrow(d) < 2) {
      return(plotly_empty() %>% layout(title = "Not enough data for this selection"))
    }
    p <- ggplot(d, aes(x = date, y = aqi, color = city, group = city, text = paste0(
      city, "<br>", format(date, format = "%Y-%m-%d"), "<br>AQI: ", round(aqi, 1)
    ))) +
      geom_line(size = 0.35, alpha = 0.9) +
      geom_point(size = 0.6, alpha = 0.5) +
      scale_color_hue(c = 50, l = 65) +
      labs(x = NULL, y = "AQI", color = "City") +
      theme_aqi_dark()
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

    leaflet(summ, options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      addCircleMarkers(
        ~lng, ~lat,
        radius = ~pmax(8, mean_aqi / 8),
        color = ~color,
        fillColor = ~color,
        fillOpacity = 0.78,
        stroke = TRUE,
        weight = 1.2,
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
      scale_fill_manual(values = c("#00A86B", "#4cc9f0", "#e6c200", "#FF8C00", "#E63946")) +
      labs(x = NULL, y = "Mean level (dataset units)") +
      theme_aqi_dark()
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
      colorscale = list(c(0, "#00A86B"), c(0.5, "#1a1f26"), c(1, "#E63946")),
      zmin = -1, zmax = 1,
      colorbar = list(title = "r", tickfont = list(color = "#c8d0da"), titlefont = list(color = "#c8d0da"))
    ) %>%
      layout(
        plot_bgcolor = "#1a1f26",
        paper_bgcolor = "#0f1419",
        font = list(color = "#c8d0da"),
        xaxis = list(side = "bottom", gridcolor = "#2d3540"),
        yaxis = list(autorange = "reversed", gridcolor = "#2d3540")
      )
  })

  # RMSE comparison (test set); best model highlighted
  output$plot_rmse_compare <- renderPlotly({
    mt <- artifacts$metrics_test
    best_i <- which.min(mt$RMSE)
    mt$band <- ifelse(seq_len(nrow(mt)) == best_i, "Best", "Other")
    p <- ggplot(mt, aes(
      x = reorder(model, RMSE), y = RMSE, fill = band,
      text = paste0(model, "<br>RMSE: ", round(RMSE, 4), "<br>R²: ", round(Rsquared, 4))
    )) +
      geom_col(width = 0.65) +
      scale_fill_manual(values = c(Best = "#00A86B", Other = "#3d4a5c"), guide = "none") +
      coord_flip() +
      labs(x = NULL, y = "RMSE (test set)", title = NULL) +
      theme_aqi_dark()
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })

  output$tbl_metrics_test_ui <- renderUI({
    mt <- artifacts$metrics_test
    best_i <- which.min(mt$RMSE)
    rows <- apply(seq_len(nrow(mt)), 1L, function(i) {
      tr_class <- if (i == best_i) "row-best" else ""
      tags$tr(
        class = tr_class,
        tags$td(mt$model[i]),
        tags$td(format(round(mt$RMSE[i], 5), nsmall = 5)),
        tags$td(format(round(mt$Rsquared[i], 5), nsmall = 5))
      )
    })
    div(
      class = "metrics-table-wrap",
      tags$table(
        class = "metrics-table",
        tags$thead(tags$tr(tags$th("Model"), tags$th("RMSE"), tags$th("R²"))),
        tags$tbody(rows)
      ),
      if (best_i >= 1L) {
        tags$p(class = "small text-success mt-2 mb-0 px-2", icon("check-circle"), " Best test RMSE: ", mt$model[best_i])
      }
    )
  })

  output$tbl_metrics_cv_ui <- renderUI({
    mt <- artifacts$metrics_resample
    best_i <- if (all(is.na(mt$RMSE))) NA_integer_ else which.min(mt$RMSE)
    rows <- apply(seq_len(nrow(mt)), 1L, function(i) {
      tr_class <- if (!is.na(best_i) && i == best_i) "row-best" else ""
      tags$tr(
        class = tr_class,
        tags$td(mt$model[i]),
        tags$td(if (is.na(mt$RMSE[i])) "—" else format(round(mt$RMSE[i], 5), nsmall = 5)),
        tags$td(if (is.na(mt$Rsquared[i])) "—" else format(round(mt$Rsquared[i], 5), nsmall = 5))
      )
    })
    div(
      class = "metrics-table-wrap",
      tags$table(
        class = "metrics-table",
        tags$thead(tags$tr(tags$th("Model"), tags$th("CV RMSE (best tune)"), tags$th("CV R²"))),
        tags$tbody(rows)
      )
    )
  })

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
      return(div(class = "prediction-box", style = "background:#3d4a5c;color:#e8eaed;", "Unable to predict"))
    }
    ac <- aqi_category(val)
    alert <- aqi_alert_message(val)
    div(
      div(
        class = "prediction-box",
        style = paste0(
          "background:linear-gradient(135deg, ", ac$color[1], " 0%, #1a1a1a 180%);",
          "color:#fff;border:1px solid rgba(255,255,255,0.15);"
        ),
        icon("bullseye"), " ", paste0("Predicted AQI: ", round(val, 1))
      ),
      div(style = "margin-top:0.55rem;font-weight:600;text-align:center;color:#e8eaed;", ac$category[1]),
      div(style = "color:#9aa5b4;font-size:0.82rem;margin-top:0.35rem;text-align:center;line-height:1.35;", alert)
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
        "Selection:",
        paste("  Date range:", input$date_rng[1], "to", input$date_rng[2]),
        paste("  City filter:", input$city_filter),
        "",
        "Filtered data: n =", nrow(d),
        paste("  Mean AQI:", round(mean(d$aqi, na.rm = TRUE), 2)),
        paste("  Max AQI:", round(max(d$aqi, na.rm = TRUE), 2)),
        "",
        "Model comparison (test set):",
        capture.output(print(artifacts$metrics_test)),
        "",
        "Current prediction inputs:",
        paste("  PM2.5:", input$in_pm25, " PM10:", input$in_pm10, " NO2:", input$in_no2, " CO:", input$in_co, " SO2:", input$in_so2),
        paste("  Predicted AQI:", round(pr$value, 2), "|", ac$category[1]),
        paste("  Alert:", aqi_alert_message(pr$value))
      )
      writeLines(lines, file)
    }
  )
}

shinyApp(ui, server)
