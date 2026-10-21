# ============================================
# ECOSHIELD: ANNUAL RAINFALL VISUALIZATION
# ============================================

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# Load the prepared dataset
rainfall_analysis <- read_csv(
  "data/rainfall_analysis.csv",
  show_col_types = FALSE
)

# Exclude station-date groups with conflicting values
rainfall_clean_analysis <- rainfall_analysis %>%
  filter(Data_Quality == "No conflicting values")

# Calculate annual average recorded rainfall
annual_rainfall <- rainfall_clean_analysis %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    Average_Rainfall = mean(Rainfall, na.rm = TRUE),
    Number_of_Records = n(),
    Number_of_Stations = n_distinct(Station),
    .groups = "drop"
  )

# Create annual rainfall chart
ggplot(
  annual_rainfall,
  aes(
    x = Year,
    y = Average_Rainfall
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 0.9
  ) +
  labs(
    title = "Annual Rainfall Pattern in Chennai",
    subtitle = "Average of recorded rainfall observations without identified conflicts",
    x = "Year",
    y = "Annual average recorded rainfall",
    caption = paste(
      "Source: Chennai Historical Rainfall Dataset.",
      "Rainfall units require verification."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(face = "bold")
  )

# Save the annual summary
write_csv(
  annual_rainfall,
  "data/annual_rainfall_summary.csv"
)

cat("Annual rainfall visualization completed!\n")
cat("Annual summary saved to data/annual_rainfall_summary.csv\n")

# ============================================
# ECOSHIELD: IMPROVED STATION-WISE COMPARISON
# ============================================

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Load analysis dataset
rainfall_analysis <- read_csv(
  "data/rainfall_analysis.csv",
  show_col_types = FALSE
)

# Exclude records with identified conflicts
rainfall_station_clean <- rainfall_analysis %>%
  filter(Data_Quality == "No conflicting values")

# Calculate average recorded rainfall by station
station_rainfall <- rainfall_station_clean %>%
  group_by(Station) %>%
  summarise(
    Average_Rainfall = mean(Rainfall, na.rm = TRUE),
    Number_of_Records = n(),
    .groups = "drop"
  ) %>%
  arrange(Average_Rainfall)

# Create improved chart
station_plot <- ggplot(
  station_rainfall,
  aes(
    x = Average_Rainfall,
    y = reorder(Station, Average_Rainfall)
  )
) +
  geom_col(width = 0.7) +
  labs(
    title = "Station-Wise Rainfall Comparison",
    subtitle = "Average recorded rainfall without identified conflicts",
    x = "Average recorded rainfall",
    y = "Monitoring station",
    caption = paste(
      "Source: Chennai Historical Rainfall Dataset.",
      "Rainfall units require verification."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

# Display chart
print(station_plot)

# Save summary
write_csv(
  station_rainfall,
  "data/station_rainfall_summary.csv"
)

# Save a high-resolution image
ggsave(
  filename = "data/station_rainfall_comparison.png",
  plot = station_plot,
  width = 10,
  height = max(7, nrow(station_rainfall) * 0.22),
  dpi = 300,
  limitsize = FALSE
)

cat("Improved station-wise rainfall chart saved successfully!\n")

# ============================================
# ECOSHIELD: TOP 15 STATIONS BY AVERAGE RAINFALL
# ============================================

library(dplyr)
library(ggplot2)
library(readr)

# Load analysis dataset
rainfall_analysis <- read_csv(
  "data/rainfall_analysis.csv",
  show_col_types = FALSE
)

# Exclude station-date groups with identified conflicts
rainfall_station_clean <- rainfall_analysis %>%
  filter(Data_Quality == "No conflicting values")

# Calculate station-wise averages
station_rainfall <- rainfall_station_clean %>%
  group_by(Station) %>%
  summarise(
    Average_Rainfall = mean(Rainfall, na.rm = TRUE),
    Number_of_Records = n(),
    .groups = "drop"
  )

# Select the 15 stations with the highest averages
top_15_stations <- station_rainfall %>%
  slice_max(
    order_by = Average_Rainfall,
    n = 15,
    with_ties = FALSE
  ) %>%
  arrange(Average_Rainfall)

# Create readable chart
ggplot(
  top_15_stations,
  aes(
    x = Average_Rainfall,
    y = reorder(Station, Average_Rainfall)
  )
) +
  geom_col() +
  labs(
    title = "Top 15 Chennai Stations by Average Recorded Rainfall",
    subtitle = "Records without identified station-date conflicts",
    x = "Average recorded rainfall",
    y = "Monitoring station",
    caption = "Rainfall units require verification."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(face = "bold")
  )

# Save the summary
write_csv(
  top_15_stations,
  "data/top_15_rainfall_stations.csv"
)

cat("Top 15 station chart created successfully!\n")