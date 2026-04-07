# data_acquisition.R — Automatic download of India multi-city air quality data
#
# Primary source (preferred): Kaggle dataset
#   rohanrao/air-quality-data-in-india
# File expected: city_day.csv
#
# Kaggle access requires the Kaggle CLI + credentials (kaggle.json or env vars).
# If Kaggle download is not available, we fall back to a public mirror URL so the
# project remains reproducible for reviewers.
#
# Optional env vars:
#   AQI_DATA_URL           — override public CSV fallback URL
#   AQI_SKIP_DOWNLOAD      — if "1", skip download and use existing data/air_quality.csv
#   AQI_FORCE_DOWNLOAD     — if "1", re-download even when air_quality.csv exists
#   AQI_KAGGLE_DATASET     — override Kaggle dataset slug (default: rohanrao/air-quality-data-in-india)
#   AQI_KAGGLE_FILE        — override Kaggle file name (default: city_day.csv)
#   KAGGLE_USERNAME / KAGGLE_KEY or %USERPROFILE%/.kaggle/kaggle.json

if (!exists("ensure_dirs", mode = "function")) {
  source(file.path(getwd(), "utils.R"))
}

#' Public CSV endpoints (India only). First success wins.
india_air_quality_download_urls <- function() {
  custom <- trimws(Sys.getenv("AQI_DATA_URL", unset = ""))
  urls <- c(
    if (nzchar(custom)) custom,
    # Public mirror of the Kaggle city_day.csv (for environments without Kaggle API).
    "https://huggingface.co/datasets/AdityaaXD/AQI-Of-India/resolve/main/city_day.csv"
  )
  unique(urls[nzchar(urls)])
}

#' Check whether Kaggle CLI is available
kaggle_cli_available <- function() {
  nzchar(Sys.which("kaggle"))
}

#' Download the Kaggle dataset (city_day.csv) into dest_path if possible.
#' Returns TRUE on success, FALSE on failure (no stop).
try_acquire_from_kaggle <- function(dest_path) {
  if (!kaggle_cli_available()) return(FALSE)

  dataset <- trimws(Sys.getenv("AQI_KAGGLE_DATASET", unset = "rohanrao/air-quality-data-in-india"))
  file_name <- trimws(Sys.getenv("AQI_KAGGLE_FILE", unset = "city_day.csv"))

  tmp_dir <- file.path(tempdir(), "aqi_kaggle_dl")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  # Use `kaggle datasets download` (requires credentials)
  # Note: Kaggle CLI usually writes a zip named like "<file>.zip" into tmp_dir.
  cmd <- sprintf(
    "kaggle datasets download -d %s -f %s -p %s --force --quiet",
    shQuote(dataset), shQuote(file_name), shQuote(tmp_dir)
  )

  ok <- tryCatch({
    system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

    # Kaggle CLI typically downloads "<file_name>.zip" into tmp_dir
    src <- file.path(tmp_dir, file_name)
    zip_guess <- file.path(tmp_dir, paste0(file_name, ".zip"))
    if (!file.exists(src)) {
      if (file.exists(zip_guess)) {
        suppressWarnings(utils::unzip(zip_guess, exdir = tmp_dir, overwrite = TRUE))
      } else {
        zips <- list.files(tmp_dir, pattern = "\\\\.zip$", full.names = TRUE)
        for (z in zips) suppressWarnings(utils::unzip(z, exdir = tmp_dir, overwrite = TRUE))
      }
    }

    # If unzip still didn't yield the file, try streaming it out of the zip
    if (!file.exists(src)) {
      zips <- list.files(tmp_dir, pattern = "\\\\.zip$", full.names = TRUE)
      if (length(zips) > 0) {
        listing <- tryCatch(utils::unzip(zips[1], list = TRUE), error = function(e) NULL)
        if (!is.null(listing) && "Name" %in% names(listing)) {
          match_name <- listing$Name[grepl(paste0("(^|/)", file_name, "$"), listing$Name)]
          if (length(match_name) > 0) {
            con_in <- unz(zips[1], match_name[1], open = "rb")
            bytes <- readBin(con_in, what = "raw", n = 1e9)
            close(con_in)
            writeBin(bytes, src)
          }
        }
      }
    }

    if (!file.exists(src)) return(FALSE)
    file.copy(src, dest_path, overwrite = TRUE)
  }, error = function(e) FALSE)

  isTRUE(ok)
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

  # 1) Prefer Kaggle (reliable canonical source)
  if (try_acquire_from_kaggle(dest_path)) {
    message("Saved India air quality CSV from Kaggle to: ", normalizePath(dest_path, winslash = "/"))
    return(invisible(TRUE))
  }

  # 2) Fall back to public URL mirror (keeps project runnable without Kaggle credentials)
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
    "\n\nKaggle option: install Kaggle CLI and configure credentials (kaggle.json), then rerun.\n",
    "Fallback option: check your network/proxy; or set AQI_DATA_URL to a public CSV\n",
    "with columns City, Date, PM2.5, PM10, NO2, CO, SO2, and AQI.",
    call. = FALSE
  )
}
