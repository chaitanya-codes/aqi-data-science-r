# data_preprocessing.R — Load CSV, clean, impute, parse dates, save processed RDS
# Working directory must be this project folder (Projects/R).

source(file.path(getwd(), "utils.R"))

ensure_dirs()

RAW_CSV <- file.path(data_dir(), "air_quality.csv")
PROCESSED_RDS <- file.path(data_dir(), "processed_air_quality.rds")

PREDICTOR_COLS <- c("pm25", "pm10", "no2", "co", "so2")
TARGET_COL <- "aqi"

#' Median impute numeric columns
impute_median <- function(x) {
  m <- median(x, na.rm = TRUE)
  if (is.na(m)) m <- 0
  x[is.na(x)] <- m
  x
}

#' Parse dates with common formats (base R only)
parse_dates_safe <- function(x) {
  x <- trimws(as.character(x))
  num <- suppressWarnings(as.numeric(x))
  if (sum(!is.na(num)) / max(length(x), 1L) > 0.9) {
    dnum <- as.Date(num, origin = "1970-01-01")
    if (sum(!is.na(dnum)) > length(x) / 2) {
      return(dnum)
    }
  }
  d <- suppressWarnings(as.Date(x))
  na1 <- is.na(d)
  if (any(na1)) {
    d[na1] <- suppressWarnings(as.Date(x[na1], format = "%m/%d/%Y"))
  }
  na2 <- is.na(d)
  if (any(na2)) {
    d[na2] <- suppressWarnings(as.Date(x[na2], format = "%d/%m/%Y"))
  }
  na3 <- is.na(d)
  if (any(na3)) {
    d[na3] <- suppressWarnings(as.Date(substr(x[na3], 1, 10), format = "%Y-%m-%d"))
  }
  d
}

# --- Local CSV or reproducible demo ---
if (!file.exists(RAW_CSV)) {
  message("No ", RAW_CSV, " found; creating demo dataset for local development.")
  create_demo_dataset(RAW_CSV)
}

raw_df <- read.csv(RAW_CSV, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "null"))
raw_df <- standardize_pollution_names(raw_df)

if (!"date" %in% names(raw_df)) {
  stop("Dataset must include a date column (Date, dt, or timestamp).")
}
raw_df$date <- parse_dates_safe(raw_df$date)

if (!"city" %in% names(raw_df)) {
  raw_df$city <- "Unknown"
}
raw_df$city <- as.character(raw_df$city)

need <- c(PREDICTOR_COLS, TARGET_COL)
missing_cols <- setdiff(need, names(raw_df))
if (length(missing_cols) > 0) {
  stop(
    "Missing columns after standardization: ",
    paste(missing_cols, collapse = ", "),
    ". Expected: pm25, pm10, no2, co, so2, aqi (or common Kaggle names)."
  )
}

df <- raw_df[!is.na(raw_df[[TARGET_COL]]), , drop = FALSE]

for (col in PREDICTOR_COLS) {
  df[[col]] <- as.numeric(df[[col]])
  df[[col]] <- impute_median(df[[col]])
}

for (col in PREDICTOR_COLS) {
  df[[col]][df[[col]] < 0] <- 0
}

df[[TARGET_COL]] <- as.numeric(df[[TARGET_COL]])

df <- df[!is.na(df$date), , drop = FALSE]

saveRDS(df, PROCESSED_RDS)
message("Preprocessing complete. Saved: ", PROCESSED_RDS)
message("Rows: ", nrow(df), " | Cities: ", length(unique(df$city)))
