# Runs the project

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
  setwd(script_dir)
}

port <- 3940
url <- paste0("http://127.0.0.1:", port)

cat(" Open in browser:\n ", url, "\n")
cat("==========================================\n\n")

# Run pipeline
source("run_pipeline.R")

# Launch app
shiny::runApp(host = "127.0.0.1", port = port, launch.browser = TRUE)