# data_acquisition.R — Automatic download of India multi-city air quality data
#
# Primary source: Hugging Face mirror of the widely used India AQI dataset
# (city-day CPCB-style records: Delhi, Mumbai, Chennai, Bengaluru, Ahmedabad, …).
# Public URL, no manual upload required.
#
# Optional env vars:
#   AQI_DATA_URL      — override download URL (must be CSV with City, Date, pollutants, AQI)
#   AQI_SKIP_DOWNLOAD — if "1", skip download and use existing data/air_quality.csv
#   AQI_FORCE_DOWNLOAD — if "1", re-download even when air_quality.csv exists

if (!exists("ensure_dirs", mode = "function")) {
  source(file.path(getwd(), "utils.R"))
}

#' Public CSV endpoints (India only). First success wins.
india_air_quality_download_urls <- function() {
  custom <- trimws(Sys.getenv("AQI_DATA_URL", unset = ""))
  urls <- c(
    if (nzchar(custom)) custom,
    # CPCB-style city-day table: PM2.5, PM10, NO2, CO, SO2, AQI, City, Date
    "https://huggingface.co/datasets/AdityaaXD/AQI-Of-India/resolve/main/city_day.csv"
  )
  unique(urls[nzchar(urls)])
}

#' Best download method for current platform (follows HTTPS redirects for Hugging Face).
download_method_safe <- function() {
  m <- getOption("download.file.method")
  if (is.character(m) && nzchar(m)) return(m)
  if (capabilities("libcurl")) return("libcurl")
  "auto"
}

#' Download India dataset to dest_path; stop() with a clear message if all URLs fail.
acquire_india_air_quality_csv <- function(dest_path) {
  ensure_dirs()
  urls <- india_air_quality_download_urls()
  tmp <- paste0(dest_path, ".part")
  errs <- character()

  for (u in urls) {
    ok <- tryCatch(
      {
        status <- download.file(
          u,
          destfile = tmp,
          mode = "wb",
          quiet = FALSE,
          method = download_method_safe()
        )
        if (status != 0L) stop("download.file returned non-zero status")
        if (!file.exists(tmp)) stop("temporary file missing after download")
        sz <- file.info(tmp)$size
        if (is.na(sz) || sz < 5000L) stop("downloaded file is too small to be city_day.csv")

        line1 <- paste(readLines(tmp, n = 1L, warn = FALSE, encoding = "UTF-8"), collapse = "")
        if (!nzchar(line1)) stop("CSV appears empty")
        if (!grepl("city", line1, ignore.case = TRUE) || !grepl("date", line1, ignore.case = TRUE)) {
          stop("CSV header must include City and Date")
        }
        if (!grepl("pm2|pm10|no2|co|so2|aqi", line1, ignore.case = TRUE)) {
          stop("CSV header must include pollutant / AQI columns (e.g. PM2.5, AQI)")
        }

        if (file.exists(dest_path)) file.remove(dest_path)
        if (!file.rename(tmp, dest_path)) {
          file.copy(tmp, dest_path, overwrite = TRUE)
          unlink(tmp)
        }
        message("Saved India air quality CSV to: ", normalizePath(dest_path, winslash = "/"))
        TRUE
      },
      error = function(e) {
        errs <<- c(errs, paste0(u, " — ", conditionMessage(e)))
        if (file.exists(tmp)) unlink(tmp, force = TRUE)
        FALSE
      }
    )
    if (isTRUE(ok)) return(invisible(TRUE))
  }

  stop(
    "Could not download India air quality data automatically.\n",
    "Attempts:\n",
    paste0("  • ", errs, collapse = "\n"),
    "\n\nCheck your network, corporate proxy, or Hugging Face rate limits ",
    "(set HF_TOKEN for higher limits). You may set AQI_DATA_URL to another public CSV ",
    "with columns City, Date, PM2.5, PM10, NO2, CO, SO2, and AQI.",
    call. = FALSE
  )
}
