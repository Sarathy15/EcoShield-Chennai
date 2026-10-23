# EcoShield – Chennai Environmental Intelligence Dashboard

EcoShield is an interactive environmental intelligence dashboard developed using R Shiny to explore environmental datasets related to Chennai.

🌐 **Live Demo:** [https://sarathy15.shinyapps.io/ecoshield-chennai/](https://sarathy15.shinyapps.io/ecoshield-chennai/)

## Features

- Environmental data overview
- Rainfall data visualization
- Flood-related geographic visualization
- Air quality analysis
- Parks data exploration
- Live environmental telemetry & monitoring interface
- Executive Enterprise Design System (clean white cards, neutral slate canvas `#f8fafc`, subtle 1px borders, high-contrast data readability)
- Refined Typography: Google Font `Inter` with tabular digits and disciplined visual hierarchy
- Custom Vector Iconography: Native inline SVG icons for crisp cross-platform rendering
- Quick Keyboard Shortcuts: Keys `1`–`6` for instant navigation between tabs

## Technologies Used

- R
- Shiny (bslib)
- ggplot2
- Plotly
- Leaflet
- dplyr
- tidyr
- readr

## Project Structure

- `app.R` – Main Shiny application
- `clean_datasets.R` – Dataset cleaning
- `rainfall_visualization.R` – Rainfall visualizations
- `flood_map.R` – Flood-related geographic visualization
- `air_quality_visualization.R` – Air quality visualizations
- `park_visualization.R` – Parks data visualizations
- `data/` – Project datasets
- `www/styles.css` – Enterprise design system & stylesheet
- `www/app.js` – Client-side utilities and keyboard shortcuts

## How to Run

1. Install R and RStudio.
2. Open `EcoShield.Rproj`.
3. Open `app.R`.
4. Install any missing required R packages.
5. Click **Run App** in RStudio or run `shiny::runApp()` in the R console.

## Data Limitations

Historical datasets and live environmental information must be distinguished clearly. Data availability, observation periods, missing values, and source limitations may affect the results.

## Future Improvements

- Improve live environmental data retrieval.
- Enhance interactive geographic visualizations.
- Add additional environmental indicators.
- Improve data quality validation.

## Author

Sarathy P.
Integrated M.Tech – Computer Science and Engineering (Data Science)
