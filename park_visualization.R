# ============================================
# ECOSHIELD: CHENNAI PARK DISTRIBUTION
# ============================================

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load the parks dataset
parks <- read_csv(
  "data/chennai parks and location dataset.csv",
  show_col_types = FALSE
)

# Clean text fields
parks_clean <- parks %>%
  mutate(
    Zone = str_squish(as.character(Zone)),
    Ward = str_squish(as.character(Ward)),
    `Name / Location of the Park` =
      str_squish(as.character(`Name / Location of the Park`))
  ) %>%
  filter(
    !is.na(Zone),
    Zone != "",
    !is.na(`Name / Location of the Park`),
    `Name / Location of the Park` != ""
  )

# Count listed parks by zone
parks_by_zone <- parks_clean %>%
  group_by(Zone) %>%
  summarise(
    Listed_Parks = n_distinct(`Name / Location of the Park`),
    .groups = "drop"
  ) %>%
  arrange(Listed_Parks)

# Display the summary
cat("Total park records:", nrow(parks_clean), "\n")
cat("\nListed parks by zone:\n")
print(parks_by_zone, n = Inf)

# Create the chart
park_plot <- ggplot(
  parks_by_zone,
  aes(
    x = Listed_Parks,
    y = reorder(Zone, Listed_Parks)
  )
) +
  geom_col() +
  geom_text(
    aes(label = Listed_Parks),
    hjust = -0.2,
    size = 3.5
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Distribution of Listed Parks Across Chennai Zones",
    subtitle = "Number of distinct park names in the dataset",
    x = "Number of listed parks",
    y = "Zone",
    caption = paste(
      "Source: Chennai Parks and Location Dataset.",
      "Park counts do not measure green-cover area."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

# Display the chart
print(park_plot)

# Save the results
write_csv(
  parks_by_zone,
  "data/parks_by_zone_summary.csv"
)

ggsave(
  "data/chennai_parks_by_zone.png",
  plot = park_plot,
  width = 10,
  height = 7,
  dpi = 300
)

cat("\nPark distribution analysis completed successfully!\n")