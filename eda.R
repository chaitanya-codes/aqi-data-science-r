# eda.R — Exploratory data analysis plots
# Run from project root after data_preprocessing.R, e.g.:
#   setwd("path/to/Projects/R"); source("eda.R")

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

source(file.path(getwd(), "utils.R"))

proc <- file.path(data_dir(), "processed_air_quality.rds")
if (!file.exists(proc)) {
  stop("Run data_preprocessing.R first. Missing: ", proc)
}

df <- readRDS(proc)
message("EDA — rows: ", nrow(df), " | cities: ", length(unique(df$city)))

# Distribution of AQI
print(
  ggplot(df, aes(x = aqi)) +
    geom_histogram(bins = 40, fill = "#0d7a5c", color = "white", alpha = 0.9) +
    labs(title = "Distribution of AQI", x = "AQI", y = "Count") +
    theme_minimal(base_size = 12)
)

# Time series (sample of cities for readability)
top_cities <- df %>%
  count(city, sort = TRUE) %>%
  head(8) %>%
  pull(city)
df_sub <- df %>% filter(city %in% top_cities)

print(
  ggplot(df_sub, aes(x = date, y = aqi, group = city, color = city)) +
    geom_line(alpha = 0.75, linewidth = 0.3) +
    labs(title = "AQI over time (top cities by row count)", x = NULL, y = "AQI") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
)

# Pollutant distributions (faceted) — long format without tidyr
long_p <- data.frame(
  pollutant = rep(c("pm25", "pm10", "no2", "co", "so2"), each = nrow(df)),
  value = c(df$pm25, df$pm10, df$no2, df$co, df$so2)
)

print(
  ggplot(long_p, aes(x = value)) +
    geom_histogram(bins = 35, fill = "#457b9d", color = "white", alpha = 0.85) +
    facet_wrap(~pollutant, scales = "free", ncol = 3) +
    labs(title = "Distribution of pollutant concentrations", x = NULL, y = "Count") +
    theme_minimal(base_size = 11)
)

nums <- df[, c("pm25", "pm10", "no2", "co", "so2", "aqi")]
cm <- cor(nums, use = "pairwise.complete.obs")
message("Correlation matrix (AQI vs pollutants) computed. AQI~PM2.5 r = ", round(cm["aqi", "pm25"], 3))
