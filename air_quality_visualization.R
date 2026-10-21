# ============================================
# ECOSHIELD: CHENNAI AIR-QUALITY ANALYSIS
# ============================================

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load air-quality dataset
air_quality <- read_csv(
  "data/Chennai_Air_Quality_Index.csv",
  show_col_types = FALSE
)

# Inspect the dataset
cat("Total air-quality records:", nrow(air_quality), "\n")

cat("\nAvailable columns:\n")
print(names(air_quality))

# Clean station names and AQI values
air_quality_clean <- air_quality %>%
  mutate(
    Station = str_squish(Station),
    `AQI Value` = parse_number(
      as.character(`AQI Value`)
    )
  ) %>%
  filter(
    !is.na(Station),
    Station != "",
    !is.na(`AQI Value`)
  )

# Calculate station-wise AQI summaries
aqi_summary <- air_quality_clean %>%
  group_by(Station) %>%
  summarise(
    Average_AQI = mean(`AQI Value`),
    Median_AQI = median(`AQI Value`),
    Number_of_Records = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_AQI))

# Display summary
cat("\n--- AQI Summary by Station ---\n")
print(aqi_summary, n = Inf)

# Create station-wise AQI chart
aqi_plot <- ggplot(
  aqi_summary,
  aes(
    x = Average_AQI,
    y = reorder(Station, Average_AQI)
  )
) +
  geom_col() +
  geom_text(
    aes(label = round(Average_AQI, 1)),
    hjust = -0.15,
    size = 3.5
  ) +
  labs(
    title = "Air Quality Comparison Across Chennai",
    subtitle = "Average recorded AQI by monitoring station",
    x = "Average AQI value",
    y = "Monitoring station",
    caption = paste(
      "Source: Chennai Air Quality Index Dataset.",
      "Observation years require verification."
    )
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

# Display chart
print(aqi_plot)

# Save results
write_csv(
  aqi_summary,
  "data/air_quality_station_summary.csv"
)

ggsave(
  "data/chennai_air_quality_comparison.png",
  plot = aqi_plot,
  width = 10,
  height = 6,
  dpi = 300
)
# Display the AQI chart
print(aqi_plot)

cat("\nAir-quality analysis completed successfully!\n")