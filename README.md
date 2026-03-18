# Air Quality Index Prediction and Visualization (R)

Course project: end-to-end data science pipeline in **R** for Indian city-level air quality — preprocessing, exploratory analysis, regression models (linear + random forest), evaluation, and a **Shiny** dashboard.

**Status:** early setup — data pipeline and app to be added incrementally.

## Planned structure

- `data_preprocessing.R` — cleaning, imputation, date parsing  
- `model_training.R` — `caret` models, RMSE / R²  
- `app.R` — Shiny + Plotly + Leaflet  
- `utils.R` — shared helpers  
- `data/` — CSV / processed outputs  

## Requirements

- R 4.x, packages: shiny, plotly, leaflet, dplyr, ggplot2, caret, randomForest, e1071  

Run the app from the project folder: `shiny::runApp()`.
