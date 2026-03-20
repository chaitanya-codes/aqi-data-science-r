# model_training.R — Train linear regression with caret (first modeling pass)
# Run from project root after data_preprocessing.R.

source(file.path(getwd(), "utils.R"))
ensure_dirs()

suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
})

PROCESSED_RDS <- file.path(data_dir(), "processed_air_quality.rds")
ARTIFACTS_RDS <- file.path(models_dir(), "artifacts.rds")

PREDICTOR_COLS <- c("pm25", "pm10", "no2", "co", "so2")
TARGET_COL <- "aqi"

if (!file.exists(PROCESSED_RDS)) {
  stop("Missing processed data. Run data_preprocessing.R first: ", PROCESSED_RDS)
}

df <- readRDS(PROCESSED_RDS)

model_formula <- as.formula(
  paste(TARGET_COL, "~", paste(PREDICTOR_COLS, collapse = " + "))
)

set.seed(42)
train_idx <- createDataPartition(df[[TARGET_COL]], p = 0.8, list = FALSE)
train_df <- df[train_idx, , drop = FALSE]
test_df <- df[-train_idx, , drop = FALSE]

ctrl <- trainControl(method = "cv", number = 5)

message("Training linear regression...")
fit_lm <- train(
  model_formula,
  data = train_df,
  method = "lm",
  trControl = ctrl,
  preProcess = c("center", "scale")
)

pred_lm_test <- predict(fit_lm, newdata = test_df)
m_lm <- postResample(pred_lm_test, test_df[[TARGET_COL]])

safe_cv_rmse <- function(fit) {
  r <- fit$results
  if (is.null(r) || !("RMSE" %in% names(r))) return(NA_real_)
  min(r$RMSE, na.rm = TRUE)
}
safe_cv_r2 <- function(fit) {
  r <- fit$results
  if (is.null(r) || !("Rsquared" %in% names(r))) return(NA_real_)
  max(r$Rsquared, na.rm = TRUE)
}

metrics_test <- data.frame(
  model = "Linear Regression",
  RMSE = as.numeric(m_lm["RMSE"]),
  Rsquared = as.numeric(m_lm["Rsquared"]),
  stringsAsFactors = FALSE
)

metrics_resample <- data.frame(
  model = "Linear Regression",
  RMSE = safe_cv_rmse(fit_lm),
  Rsquared = safe_cv_r2(fit_lm),
  stringsAsFactors = FALSE
)

artifacts <- list(
  fit_lm = fit_lm,
  fit_rf = NULL,
  best = "lm",
  metrics_test = metrics_test,
  metrics_resample = metrics_resample,
  predictors = PREDICTOR_COLS,
  target = TARGET_COL,
  trained_at = Sys.time()
)

saveRDS(artifacts, ARTIFACTS_RDS)
message("Linear model test RMSE: ", round(metrics_test$RMSE, 4), " | R²: ", round(metrics_test$Rsquared, 4))
message("Saved: ", ARTIFACTS_RDS)
