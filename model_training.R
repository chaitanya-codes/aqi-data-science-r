# model_training.R — Train Linear Regression and Random Forest with caret; save artifacts
# Run from project root after data_preprocessing.R (working directory = Projects/R).
# Outputs: models/artifacts.rds (fits, best flag, test + CV metrics for the Shiny app).

source(file.path(getwd(), "utils.R"))
ensure_dirs()

suppressPackageStartupMessages({
  library(caret)
  library(randomForest)
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

message("Training random forest (this may take a minute)...")
fit_rf <- train(
  model_formula,
  data = train_df,
  method = "rf",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 5,
  metric = "RMSE"
)

pred_lm_test <- predict(fit_lm, newdata = test_df)
pred_rf_test <- predict(fit_rf, newdata = test_df)

m_lm <- postResample(pred_lm_test, test_df[[TARGET_COL]])
m_rf <- postResample(pred_rf_test, test_df[[TARGET_COL]])

best_model <- if (as.numeric(m_rf["RMSE"]) <= as.numeric(m_lm["RMSE"])) "rf" else "lm"

metrics_test <- data.frame(
  model = c("Linear Regression", "Random Forest"),
  RMSE = c(as.numeric(m_lm["RMSE"]), as.numeric(m_rf["RMSE"])),
  Rsquared = c(as.numeric(m_lm["Rsquared"]), as.numeric(m_rf["Rsquared"])),
  stringsAsFactors = FALSE
)

# Training-set resample metrics for dashboard comparison
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
metrics_resample <- data.frame(
  model = c("Linear Regression", "Random Forest"),
  RMSE = c(safe_cv_rmse(fit_lm), safe_cv_rmse(fit_rf)),
  Rsquared = c(safe_cv_r2(fit_lm), safe_cv_r2(fit_rf)),
  stringsAsFactors = FALSE
)

artifacts <- list(
  fit_lm = fit_lm,
  fit_rf = fit_rf,
  best = best_model,
  metrics_test = metrics_test,
  metrics_resample = metrics_resample,
  predictors = PREDICTOR_COLS,
  target = TARGET_COL,
  trained_at = Sys.time()
)

saveRDS(artifacts, ARTIFACTS_RDS)
message("Best model (lower test RMSE): ", best_model)
message("Test metrics:\n", paste(capture.output(print(metrics_test)), collapse = "\n"))
message("Saved: ", ARTIFACTS_RDS)
