# utils.R — Helper functions for AQI project
# Run the app and pipeline with working directory set to this project folder.

#' Project root: working directory (setwd to this folder before runApp / sourcing).
project_root <- function() {
  if (nzchar(Sys.getenv("AQI_PROJECT_ROOT"))) {
    return(normalizePath(Sys.getenv("AQI_PROJECT_ROOT"), winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/")
}

#' Paths for data and model artifacts
data_dir <- function() file.path(project_root(), "data")
models_dir <- function() file.path(project_root(), "models")

#' Ensure directories exist
ensure_dirs <- function() {
  dir.create(data_dir(), showWarnings = FALSE, recursive = TRUE)
  dir.create(models_dir(), showWarnings = FALSE, recursive = TRUE)
}

#' AQI category and color (US EPA–style breakpoints, simplified for display)
#' @param aqi Numeric vector of AQI values
#' @return data.frame with columns category, color, alert_level
aqi_category <- function(aqi) {
  aqi <- as.numeric(aqi)
  category <- rep(NA_character_, length(aqi))
  color <- rep("#808080", length(aqi))
  alert_level <- rep("unknown", length(aqi))

  category[aqi <= 50] <- "Good"
  color[aqi <= 50] <- "#00A86B"
  alert_level[aqi <= 50] <- "ok"

  category[aqi > 50 & aqi <= 100] <- "Moderate"
  color[aqi > 50 & aqi <= 100] <- "#FFD700"
  alert_level[aqi > 50 & aqi <= 100] <- "caution"

  category[aqi > 100 & aqi <= 150] <- "Unhealthy for Sensitive"
  color[aqi > 100 & aqi <= 150] <- "#FF8C00"
  alert_level[aqi > 100 & aqi <= 150] <- "warning"

  category[aqi > 150 & aqi <= 200] <- "Unhealthy"
  color[aqi > 150 & aqi <= 200] <- "#FF4500"
  alert_level[aqi > 150 & aqi <= 200] <- "poor"

  category[aqi > 200 & aqi <= 300] <- "Very Unhealthy"
  color[aqi > 200 & aqi <= 300] <- "#8B008B"
  alert_level[aqi > 200 & aqi <= 300] <- "severe"

  category[aqi > 300] <- "Hazardous"
  color[aqi > 300] <- "#7E0023"
  alert_level[aqi > 300] <- "hazardous"

  category[is.na(aqi)] <- "Unknown"
  color[is.na(aqi)] <- "#999999"
  alert_level[is.na(aqi)] <- "unknown"

  data.frame(category = category, color = color, alert_level = alert_level, stringsAsFactors = FALSE)
}

#' Human-readable alert message for dashboard
aqi_alert_message <- function(aqi) {
  ac <- aqi_category(aqi)
  al <- ac$alert_level[1]
  if (al == "ok") return("Air quality is satisfactory.")
  if (al == "caution") return("Acceptable; unusually sensitive people may be affected.")
  if (al == "warning") return("Members of sensitive groups may experience health effects.")
  if (al == "poor") return("Everyone may begin to experience health effects.")
  if (al == "severe") return("Health alert: everyone may experience serious effects.")
  if (al == "hazardous") return("Hazardous air quality — emergency conditions possible.")
  if (al == "unknown") return("AQI could not be classified.")
  "AQI value is outside typical range or missing."
}

#' Normalize common column name variants from Kaggle / open datasets
standardize_pollution_names <- function(df) {
  n <- names(df)
  lower <- tolower(gsub("[^a-zA-Z0-9]+", "_", n))
  lower <- gsub("^_|_$", "", lower)
  lower <- gsub("_+", "_", lower)
  name_map <- c(
    "pm2_5" = "pm25",
    "pm25" = "pm25",
    "pm10" = "pm10",
    "no2" = "no2",
    "co" = "co",
    "so2" = "so2",
    "aqi" = "aqi",
    "aqi_calculated" = "aqi",
    "overall_aqi" = "aqi",
    # CPCB daily bulletin column name (urbanemissions / open data)
    "index_value" = "aqi",
    "date" = "date",
    "dt" = "date",
    "timestamp" = "date",
    "city" = "city"
  )
  # Map dotted forms like "pm2.5" after gsub -> "pm2_5"
  new_names <- lower
  for (i in seq_along(lower)) {
    key <- lower[i]
    if (key %in% names(name_map)) {
      new_names[i] <- unname(name_map[[key]])
    }
  }
  names(df) <- new_names
  df
}

#' Approximate lat/lon for cities (for Leaflet when dataset has no coordinates)
default_city_coords <- function() {
  data.frame(
    city = c(
      "Delhi", "Mumbai", "Kolkata", "Chennai", "Bangalore", "Bengaluru", "Hyderabad",
      "Pune", "Ahmedabad", "Jaipur", "Lucknow", "Varanasi", "Patna",
      "Kanpur", "Noida", "Gurgaon", "Gurugram", "Visakhapatnam", "Bhopal", "Other"
    ),
    lat = c(
      28.61, 19.08, 22.57, 13.08, 12.97, 12.97, 17.39,
      18.52, 23.03, 26.91, 26.85, 25.32, 25.59,
      26.45, 28.54, 28.46, 28.46, 17.69, 23.26, 20.59
    ),
    lng = c(
      77.21, 72.88, 88.36, 80.27, 77.59, 77.59, 78.49,
      73.86, 72.59, 75.79, 80.95, 82.97, 85.14,
      80.33, 77.39, 77.03, 77.03, 83.22, 77.41, 78.96
    ),
    stringsAsFactors = FALSE
  )
}

#' Create a reproducible demo CSV when no Kaggle file is present (structure matches typical Indian AQI data)
create_demo_dataset <- function(path) {
  set.seed(42)
  cities <- c("Delhi", "Mumbai", "Kolkata", "Chennai", "Bangalore", "Hyderabad", "Pune", "Ahmedabad")
  dates <- seq(as.Date("2019-01-01"), as.Date("2023-12-01"), by = "week")
  rows <- list()
  for (d in dates) {
    for (ct in cities) {
      base <- switch(
        ct,
        "Delhi" = 120, "Mumbai" = 80, "Kolkata" = 95,
        "Chennai" = 70, "Bangalore" = 65, "Hyderabad" = 75,
        "Pune" = 72, "Ahmedabad" = 88
      )
      yday <- as.POSIXlt(d)$yday + 1L
      seasonal <- 15 * sin(2 * pi * yday / 365)
      pm25 <- pmax(5, rnorm(1, base * 0.35 + seasonal, 25))
      pm10 <- pmax(10, pm25 * 1.8 + rnorm(1, 0, 15))
      no2 <- pmax(5, pm25 * 0.12 + rnorm(1, 15, 8))
      co <- pmax(0.2, pm25 * 0.015 + rnorm(1, 0.8, 0.25))
      so2 <- pmax(2, pm25 * 0.06 + rnorm(1, 8, 4))
      # Synthetic AQI correlated with pollutants (not using EPA formula; for ML demo)
      aqi <- pmax(10, pmin(500,
        0.8 * pm25 + 0.35 * pm10 + 0.6 * no2 + 25 * co + 0.4 * so2 + rnorm(1, 0, 12)
      ))
      if (runif(1) < 0.03) pm25 <- NA
      rows[[length(rows) + 1]] <- data.frame(
        Date = d, City = ct,
        PM25 = pm25, PM10 = pm10, NO2 = no2, CO = co, SO2 = so2, AQI = aqi,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  # ISO strings so read.csv round-trips reliably (numeric dates confuse as.Date)
  out$Date <- format(out$Date, format = "%Y-%m-%d")
  write.csv(out, path, row.names = FALSE)
  message("Demo dataset written to: ", path)
  invisible(path)
}
