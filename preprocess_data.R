library(data.table)
library(sf)
library(dplyr)

# Load data
setwd("C:/Users/Hema_ASU/OneDrive/Desktop/ASU/Project/Final Output")
affordable_housing <- fread("Affordable_Housing_Philly.csv")
geojson_data <- st_read("Affordable_Housing.geojson")

# Preprocess data
merged_data <- st_as_sf(geojson_data) %>%
  left_join(affordable_housing, by = "PROJECT_NAME") %>%
  mutate(
    accessibility_score = case_when(
      !is.na(ACCESSIBLE_UNITS.y) & !is.na(SENSORY_UNITS) & !is.na(VISITABLE_UNITS) & !is.na(TOTAL_UNITS.y) & TOTAL_UNITS.y > 0 ~ 
        (ACCESSIBLE_UNITS.y + SENSORY_UNITS + VISITABLE_UNITS) / TOTAL_UNITS.y * 100,
      TRUE ~ NA_real_
    ),
    affordability_ratio = case_when(
      !is.na(TOTAL_UNITS.y) & !is.na(median_income) & median_income > 0 ~ TOTAL_UNITS.y / median_income,
      TRUE ~ NA_real_
    ),
    age_friendly = ifelse(median_age > 65, 1, 0),
    high_disability_area = ifelse(pct_disability > mean(pct_disability, na.rm = TRUE), 1, 0)
  ) %>%
  select(PROJECT_NAME, TOTAL_UNITS = TOTAL_UNITS.y, ACCESSIBLE_UNITS = ACCESSIBLE_UNITS.y, 
         SENSORY_UNITS, VISITABLE_UNITS, PROJECT_TYPE, accessibility_score, 
         affordability_ratio, age_friendly, high_disability_area, geometry)

# Save preprocessed data
saveRDS(merged_data, "preprocessed_data.rds")
