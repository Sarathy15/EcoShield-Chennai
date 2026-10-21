# ============================================
# ECOSHIELD: INSPECT FLOOD DESCRIPTIONS
# ============================================

# Extract non-empty descriptions
non_empty_descriptions <- flood_map_data$Description[
  !is.na(flood_map_data$Description) &
    trimws(flood_map_data$Description) != ""
]

# Count non-empty descriptions
cat(
  "Total flood records:",
  nrow(flood_map_data),
  "\n"
)

cat(
  "Non-empty descriptions:",
  length(non_empty_descriptions),
  "\n"
)

# Display the first 10 descriptions
cat("\n--- First 10 Flood Descriptions ---\n")

print(head(non_empty_descriptions, 10))

# Display one complete description
if (length(non_empty_descriptions) > 0) {
  cat("\n--- Full Description Example ---\n")
  cat(non_empty_descriptions[1], "\n")
} else {
  cat("\nNo non-empty descriptions were found.\n")
}

# ============================================
# ECOSHIELD: CHECK FLOOD DATA ATTRIBUTES
# ============================================

library(sf)

# Check all column names
cat("\n--- Column Names ---\n")
print(names(flood_map_data))

# Check the first 10 records
cat("\n--- First 10 Records ---\n")
print(
  head(
    st_drop_geometry(flood_map_data),
    10
  )
)

# Check geometry types
cat("\n--- Geometry Types ---\n")
print(table(st_geometry_type(flood_map_data)))

# Check missing values in each column
cat("\n--- Missing Values ---\n")
print(
  sapply(
    st_drop_geometry(flood_map_data),
    function(x) sum(is.na(x) | trimws(as.character(x)) == "")
  )
)

# ============================================
# ECOSHIELD: BASIC FLOOD LOCATION MAP
# ============================================

library(leaflet)

flood_map <- leaflet(flood_map_data) %>%
  addTiles(
  urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  attribution = '&copy; OpenStreetMap contributors'
   ) %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    radius = 5,
    color = "#d73027",
    fillColor = "#fc8d59",
    fillOpacity = 0.75,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0(
      "<b>Recorded Flood Location</b><br>",
      "Latitude: ", round(Latitude, 5), "<br>",
      "Longitude: ", round(Longitude, 5)
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = "#fc8d59",
    labels = "Recorded flood location",
    title = "EcoShield"
  )

flood_map