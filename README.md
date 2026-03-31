# Air Quality Index Prediction and Visualization (R)

End-to-end **data science in R** for **India** city–day air quality: ingest public CPCB-style data, clean and explore it, train **linear regression** and **random forest** models (`caret`), evaluate with **RMSE** and **R²**, and explore results in an interactive **Shiny** app (**Plotly**, **Leaflet**, **bslib**).

## Project structure

| File | Purpose |
|------|--------|
| `data_acquisition.R` | Downloads the India `city_day` table from a public URL (Hugging Face mirror). |
| `data_preprocessing.R` | Reads CSV, parses dates, median-imputes pollutants, saves `processed_air_quality.rds`. |
| `model_training.R` | Trains LM + RF with 5-fold CV; writes `models/artifacts.rds`. |
| `app.R` | Shiny dashboard: KPIs, trends, map, pollutant bars, correlation, model comparison, AQI prediction. |
| `utils.R` | Paths, column renaming, AQI categories/colors, demo CSV helper, city coordinates for the map. |
| `eda.R` | Standalone exploratory plots (histograms, time series, faceted pollutants). |
| `run_pipeline.R` | Runs preprocessing then training in one step. |
| `install_packages.R` | Installs required CRAN packages. |
| `data/` | Raw/processed data (large CSV/RDS are gitignored; regenerate locally). |

## Quick start

1. **Install R 4.x** and run once from the project folder:

   ```r
   source("install_packages.R")
   ```

2. **Build data + models** (downloads data automatically on first run unless `AQI_SKIP_DOWNLOAD=1`):

   ```r
   source("run_pipeline.R")
   ```

   Or stepwise: `source("data_preprocessing.R")` then `source("model_training.R")`.

3. **Launch the app**

   ```r
   shiny::runApp()
   ```

4. **Optional EDA** (after preprocessing):

   ```r
   source("eda.R")
   ```

## Environment variables

- `AQI_FORCE_DOWNLOAD=1` — refresh `data/air_quality.csv` from the URL.
- `AQI_SKIP_DOWNLOAD=1` — use an existing CSV only (offline).
- `AQI_DATA_URL` — override the default public dataset URL.

## Data source

Default download: **Hugging Face** dataset `AdityaaXD/AQI-Of-India` (`city_day.csv`), aligned with common **Kaggle / CPCB** India air-quality fields (City, Date, PM2.5, PM10, NO2, CO, SO2, AQI).

## Author

Student project — *Air Quality Index Prediction and Visualization using Data Science in R*.
