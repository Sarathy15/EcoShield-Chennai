# ============================================
# ECOSHIELD: DATA CLEANING
# ============================================

library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Load rainfall data
rainfall <- read_csv(
  "data/Chennai_Historical_Rainfall_Dataset.csv",
  show_col_types = FALSE
)

# Inspect exact duplicate rows
rainfall_duplicates <- rainfall %>%
  count(District, Station, Rainfall, Date) %>%
  filter(n > 1)

cat("Exact duplicate groups:", nrow(rainfall_duplicates), "\n")
cat(
  "Rows beyond one copy per exact duplicate group:",
  sum(rainfall_duplicates$n - 1),
  "\n"
)

# Create cleaned working copy
rainfall_clean <- rainfall %>%
  mutate(
    District = str_squish(District),
    Station = str_squish(Station),
    Date = dmy(Date),
    Rainfall = as.numeric(Rainfall)
  )

# Check date conversion
cat(
  "Dates that failed to parse:",
  sum(is.na(rainfall_clean$Date)),
  "\n"
)

# Check duplicate station-date combinations
station_date_check <- rainfall_clean %>%
  count(District, Station, Date) %>%
  filter(n > 1)

cat(
  "Repeated station-date combinations:",
  nrow(station_date_check),
  "\n"
)

# Do not remove duplicates automatically.
# Investigate them before deciding how to handle them.

summary(rainfall_clean)


# ============================================
# ECOSHIELD: INVESTIGATE RAINFALL DUPLICATES
# ============================================

library(dplyr)

# 1. Inspect repeated station-date combinations
station_date_details <- rainfall_clean %>%
  group_by(District, Station, Date) %>%
  summarise(
    record_count = n(),
    distinct_rainfall_values = n_distinct(Rainfall),
    rainfall_values = paste(
      sort(unique(Rainfall)),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  filter(record_count > 1) %>%
  arrange(desc(record_count))

cat("Repeated station-date groups:\n")
print(head(station_date_details, 20))

# 2. Count groups with different rainfall values
different_values <- station_date_details %>%
  filter(distinct_rainfall_values > 1)

cat(
  "\nGroups with different rainfall values:",
  nrow(different_values),
  "\n"
)

# 3. Display examples
print(head(different_values, 10))

# 4. Calculate potential duplicate impact
cat(
  "\nRows in repeated station-date groups:",
  sum(station_date_details$record_count),
  "\n"
)

# ============================================
# ECOSHIELD: PRELIMINARY RAINFALL CLEANING
# ============================================

library(dplyr)
library(readr)

# Remove only exact duplicate rows
rainfall_preliminary <- rainfall_clean %>%
  distinct()

cat("Original rows:", nrow(rainfall_clean), "\n")

cat(
  "Rows after removing exact duplicates:",
  nrow(rainfall_preliminary),
  "\n"
)

cat(
  "Exact duplicate rows removed:",
  nrow(rainfall_clean) - nrow(rainfall_preliminary),
  "\n"
)

# Check remaining repeated station-date combinations
remaining_repeats <- rainfall_preliminary %>%
  count(District, Station, Date) %>%
  filter(n > 1)

cat(
  "Repeated station-date groups remaining:",
  nrow(remaining_repeats),
  "\n"
)

# Save preliminary dataset
write_csv(
  rainfall_preliminary,
  "data/rainfall_preliminary.csv"
)

cat("Preliminary rainfall dataset saved successfully!\n")

# ============================================
# ECOSHIELD: INVESTIGATE CONFLICTING RECORDS
# ============================================

library(dplyr)
library(readr)

# Find station-date groups with different rainfall values
conflicting_records <- rainfall_preliminary %>%
  group_by(District, Station, Date) %>%
  filter(n_distinct(Rainfall) > 1) %>%
  arrange(Station, Date) %>%
  ungroup()

# Display the number of conflicting records
cat(
  "Total conflicting records:",
  nrow(conflicting_records),
  "\n"
)

# Display the first 20 records
print(head(conflicting_records, 20))

# Save the conflicting records for investigation
write_csv(
  conflicting_records,
  "data/rainfall_conflicting_records.csv"
)

cat("Conflicting records saved successfully!\n")


# ============================================
# ECOSHIELD: CONFLICT ANALYSIS SUMMARY
# ============================================

library(dplyr)
library(readr)

# 1. Count conflicting groups by station
conflicts_by_station <- conflicting_records %>%
  group_by(Station) %>%
  summarise(
    Conflicting_Groups = n_distinct(Date),
    Conflicting_Records = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Conflicting_Groups))

cat("\n--- Conflicts by Station ---\n")
print(conflicts_by_station, n = Inf)


# 2. Examine how many values occur in each group
conflict_group_summary <- rainfall_preliminary %>%
  group_by(District, Station, Date) %>%
  summarise(
    Record_Count = n(),
    Distinct_Rainfall_Values = n_distinct(Rainfall),
    Rainfall_Values = paste(
      sort(unique(Rainfall)),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  filter(Distinct_Rainfall_Values > 1) %>%
  arrange(Station, Date)

cat("\n--- Conflicting Group Summary ---\n")
print(head(conflict_group_summary, 20))


# 3. Count records by year
conflicts_by_year <- conflicting_records %>%
  mutate(Year = format(Date, "%Y")) %>%
  count(Year, name = "Conflicting_Records") %>%
  arrange(Year)

cat("\n--- Conflicts by Year ---\n")
print(conflicts_by_year, n = Inf)


# 4. Save the summaries
write_csv(
  conflicts_by_station,
  "data/rainfall_conflicts_by_station.csv"
)

write_csv(
  conflict_group_summary,
  "data/rainfall_conflict_group_summary.csv"
)

write_csv(
  conflicts_by_year,
  "data/rainfall_conflicts_by_year.csv"
)

cat("\nConflict analysis summaries saved successfully!\n")

# ============================================
# ECOSHIELD: PREPARE RAINFALL ANALYSIS DATA
# ============================================

library(dplyr)
library(readr)

# Start with the preliminary dataset
rainfall_analysis <- rainfall_preliminary

# Identify station-date combinations with conflicts
rainfall_conflict_flags <- rainfall_analysis %>%
  group_by(District, Station, Date) %>%
  summarise(
    Distinct_Rainfall_Values = n_distinct(Rainfall),
    .groups = "drop"
  ) %>%
  mutate(
    Data_Quality = if_else(
      Distinct_Rainfall_Values > 1,
      "Conflicting values",
      "No conflicting values"
    )
  )

# Add the data-quality flag to each record
rainfall_analysis <- rainfall_analysis %>%
  left_join(
    rainfall_conflict_flags,
    by = c("District", "Station", "Date")
  )

# Display the results
cat("Total analysis records:", nrow(rainfall_analysis), "\n")

cat(
  "Records flagged as conflicting:",
  sum(rainfall_analysis$Data_Quality == "Conflicting values"),
  "\n"
)

cat(
  "Records without conflicting values:",
  sum(rainfall_analysis$Data_Quality == "No conflicting values"),
  "\n"
)

# Save the analysis dataset
write_csv(
  rainfall_analysis,
  "data/rainfall_analysis.csv"
)

cat("Rainfall analysis dataset saved successfully!\n")