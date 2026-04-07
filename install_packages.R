# install_packages.R — One-time dependency install for the AQI project
pkgs <- c(
  "shiny", "bslib", "bsicons", "plotly", "leaflet", "dplyr", "ggplot2", "caret",
  "randomForest", "e1071", "rmarkdown", "knitr"
)
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
message("All packages ready.")
