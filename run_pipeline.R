# run_pipeline.R — Run preprocessing then model training
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
  setwd(script_dir)
} else {
  root <- Sys.getenv("AQI_PROJECT_ROOT", unset = "")
  if (nzchar(root)) setwd(normalizePath(root))
}

source(file.path(getwd(), "data_preprocessing.R"))
source(file.path(getwd(), "model_training.R"))
message("Pipeline finished. Launch the app with: shiny::runApp()")
