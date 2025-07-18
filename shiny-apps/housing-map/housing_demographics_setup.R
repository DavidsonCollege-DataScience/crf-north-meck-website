# =============================================================================
# SETUP SCRIPT (REFINED)
# =============================================================================
library(sf)
library(tigris)
library(dplyr)
library(tidycensus)
library(stringr)
library(ggplot2)
library(tidyr)

# =============================================================================
# USER CONFIGURATION
# =============================================================================
census_api_key("92b735510e08db6b8b8f1526b68bea947313349a")

npa_shapefile_path <- "NPA_HLT.shp"
census_shapefile_path <- "Census_Population_Block_Groups.shp"

overlap_threshold <- 0.3
analysis_year <- 2023

# =============================================================================
# LOAD SPATIAL SHAPEFILES
# =============================================================================
cat("Loading shapefiles...\n")
stopifnot(file.exists(npa_shapefile_path), file.exists(census_shapefile_path))

npa <- st_read(npa_shapefile_path, quiet = TRUE)
census <- st_read(census_shapefile_path, quiet = TRUE)

npa_clean <- npa %>%
  mutate(row_id = row_number(), npa_area = st_area(geometry)) %>%
  select(row_id, npa_area, everything())

# =============================================================================
# DEFINE NORTH MECK BLOCK GROUPS
# =============================================================================
# Define target towns
north_meck_towns <- c("Davidson", "Cornelius", "Huntersville")

# Load towns shapefile (places)
places_north_meck <- places(state = "NC", year = 2023, cb = TRUE) %>%
  filter(NAME %in% north_meck_towns)

# Load block groups and preserve original GEOID as GEOID_bg
block_groups <- block_groups(state = "NC", county = "Mecklenburg", year = 2024) %>%
  rename(GEOID_bg = GEOID) %>%
  st_transform(st_crs(places_north_meck))

# Spatial join: assign block groups to towns
bg_with_towns <- st_join(block_groups, places_north_meck)

# Keep only block groups within our towns
north_meck_bg <- bg_with_towns %>%
  filter(NAME %in% north_meck_towns) %>%
  st_transform(st_crs(npa_clean))

# Vector of GEOIDs
north_meck_block_groups_vec <- as.character(north_meck_bg$GEOID_bg)

# =============================================================================
# SPATIAL INTERSECTIONS
# =============================================================================
# Perform spatial intersection between NPA polygons and North Meck block groups
filtered_intersections <- st_intersection(npa_clean, north_meck_bg) %>%
  mutate(
    intersection_area = st_area(geometry),
    percent_of_npa    = as.numeric(intersection_area / npa_area)
  )

# Standardize GEOID and NPA column names
filtered_intersections <- filtered_intersections %>%
  mutate(GEOID = GEOID_bg) %>%
  select(-GEOID_bg, everything())

# Rename NPA column if necessary
if (!"NPA" %in% names(filtered_intersections) && "NPAs_NPA" %in% names(filtered_intersections)) {
  filtered_intersections <- filtered_intersections %>%
    rename(NPA = NPAs_NPA)
}

# Filter to only overlaps that meet threshold (e.g., 30% of NPA)
filtered_major_overlaps <- filtered_intersections %>%
  filter(percent_of_npa >= overlap_threshold) %>%
  select(
    row_id, GEOID, intersection_area, npa_area, percent_of_npa, NPA,
    everything()
  )

# Safety checks
stopifnot("GEOID" %in% names(filtered_major_overlaps))
stopifnot("NPA" %in% names(filtered_major_overlaps))

cat("✅ Spatial intersection analysis completed. Valid overlaps found.\n")
# =============================================================================
# ACS HOUSING DATA
# =============================================================================
housing_vars <- c(
  vacant_units = "B25002_003", total_units = "B25002_001",
  owner_occupied = "B25003_002", renter_occupied = "B25003_003",
  cost_burdened = "B25070_007", med_income = "B19013_001",
  year_built_2000_2009 = "B25034_009"
)

meck_block_groups <- get_acs(
  geography = "block group", variables = housing_vars,
  state = "NC", county = "Mecklenburg",
  year = analysis_year, geometry = TRUE, output = "wide"
) %>%
  rename(GEOID_bg = GEOID) %>%
  filter(GEOID_bg %in% north_meck_block_groups_vec) %>%
  rename_with(~ str_remove(.x, "E$"), ends_with("E")) %>%
  mutate(
    vacancy_rate = 100 * vacant_units / total_units,
    owner_share = 100 * owner_occupied / total_units,
    renter_share = 100 * renter_occupied / total_units
  )

# =============================================================================
# ACS RACE DATA
# =============================================================================
race_vars <- c(
  total_pop = "B03002_001", white = "B03002_003",
  black = "B03002_004", asian = "B03002_006",
  hispanic = "B03002_012"
)

race_data <- get_acs(
  geography = "block group", variables = race_vars,
  state = "NC", county = "Mecklenburg",
  year = analysis_year, geometry = TRUE, output = "wide"
) %>%
  rename(GEOID_bg = GEOID) %>%
  filter(GEOID_bg %in% north_meck_block_groups_vec) %>%
  mutate(
    pct_white = 100 * whiteE / total_popE,
    pct_black = 100 * blackE / total_popE,
    pct_asian = 100 * asianE / total_popE,
    pct_hispanic = 100 * hispanicE / total_popE
  )

# =============================================================================
# TENURE
# =============================================================================
tenure_vars <- c(
  total_units = "B25003_001",
  owner_occupied = "B25003_002",
  renter_occupied = "B25003_003"
)

meck_tenure <- get_acs(
  geography = "block group", variables = tenure_vars,
  state = "NC", county = "Mecklenburg",
  year = analysis_year, geometry = TRUE, output = "wide"
) %>%
  rename(GEOID_bg = GEOID) %>%
  filter(GEOID_bg %in% north_meck_block_groups_vec) %>%
  rename_with(~ str_remove(.x, "E$"), ends_with("E")) %>%
  mutate(
    owner_share = 100 * owner_occupied / total_units,
    renter_share = 100 * renter_occupied / total_units
  )

# =============================================================================
# RACE-TENURE
# =============================================================================
race_tenure_vars <- c(
  white_total = "B25003A_001", white_owner = "B25003A_002",
  black_total = "B25003B_001", black_owner = "B25003B_002",
  asian_total = "B25003D_001", asian_owner = "B25003D_002",
  hispanic_total = "B25003I_001", hispanic_owner = "B25003I_002"
)

meck_race_tenure <- get_acs(
  geography = "block group", variables = race_tenure_vars,
  state = "NC", county = "Mecklenburg",
  year = analysis_year, geometry = TRUE, output = "wide"
) %>%
  rename(GEOID_bg = GEOID) %>%
  filter(GEOID_bg %in% north_meck_block_groups_vec) %>%
  rename_with(~ str_remove(.x, "E$"), ends_with("E")) %>%
  mutate(
    white_own_rate = 100 * white_owner / white_total,
    black_own_rate = 100 * black_owner / black_total,
    asian_own_rate = 100 * asian_owner / asian_total,
    hispanic_own_rate = 100 * hispanic_owner / hispanic_total
  )

# =============================================================================
# COMBINE + WEIGHT
# =============================================================================
cleaned_acs <- meck_block_groups %>%
  left_join(race_data %>%
              st_drop_geometry() %>%
              select(GEOID_bg, pct_white, pct_black, pct_asian, pct_hispanic),
            by = "GEOID_bg") %>%
  left_join(meck_race_tenure %>%
              st_drop_geometry() %>%
              select(GEOID_bg, white_own_rate, black_own_rate, asian_own_rate, hispanic_own_rate),
            by = "GEOID_bg") %>%
  mutate(across(
    c(vacancy_rate, owner_share, renter_share,
      pct_white, pct_black, pct_asian, pct_hispanic,
      white_own_rate, black_own_rate, asian_own_rate, hispanic_own_rate),
    ~ replace_na(.x, 0)
  )) %>%
  st_transform(4326)


filtered_major_overlaps_nogeom <- filtered_major_overlaps %>%
  st_drop_geometry() %>%
  select(GEOID_bg, NPA, percent_of_npa)


acs_with_npa <- cleaned_acs %>%
  inner_join(filtered_major_overlaps_nogeom, by = "GEOID_bg") %>%
  group_by(NPA) %>%
  mutate(weight = percent_of_npa / sum(percent_of_npa, na.rm = TRUE)) %>%
  ungroup()

npa_summary <- acs_with_npa %>%
  group_by(NPA) %>%
  summarize(
    vacancy_rate     = sum(vacancy_rate * weight, na.rm = TRUE),
    owner_share      = sum(owner_share * weight, na.rm = TRUE),
    renter_share     = sum(renter_share * weight, na.rm = TRUE),
    pct_white        = sum(pct_white * weight, na.rm = TRUE),
    pct_black        = sum(pct_black * weight, na.rm = TRUE),
    pct_asian        = sum(pct_asian * weight, na.rm = TRUE),
    pct_hispanic     = sum(pct_hispanic * weight, na.rm = TRUE),
    med_income       = sum(med_income * weight, na.rm = TRUE),
    white_own_rate   = sum(white_own_rate * weight, na.rm = TRUE),
    black_own_rate   = sum(black_own_rate * weight, na.rm = TRUE),
    asian_own_rate   = sum(asian_own_rate * weight, na.rm = TRUE),
    hispanic_own_rate = sum(hispanic_own_rate * weight, na.rm = TRUE),
    .groups = "drop"
  )


npa_geom <- filtered_major_overlaps %>%
  select(NPA, geometry) %>%
  group_by(NPA) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

npa_summary_sf <- npa_geom %>%
  left_join(st_drop_geometry(npa_summary), by = "NPA") %>%
  st_transform(4326)

# =============================================================================
# SAVE + VERIFY
# =============================================================================
saveRDS(meck_tenure, "meck_tenure.rds")
saveRDS(race_data, "race_data.rds")
saveRDS(meck_block_groups, "meck_block_groups.rds")
saveRDS(meck_race_tenure, "meck_race_tenure.rds")
saveRDS(npa_summary_sf, "npa_summary_sf.rds")
saveRDS(cleaned_acs, "cleaned_acs.rds")
saveRDS(list(
  analysis_year = analysis_year,
  overlap_threshold = overlap_threshold,
  n_block_groups = nrow(cleaned_acs),
  n_npas = nrow(npa_summary_sf),
  processing_date = Sys.Date()
), "processing_summary.rds")

cat("✅ Setup completed. Files saved.\n")

# Verification plot
ggplot(npa_summary_sf) +
  geom_sf(aes(fill = renter_share)) +
  scale_fill_viridis_c(name = "Renter Share (%)") +
  labs(
    title = "Renter Share by NPA",
    subtitle = paste("North Mecklenburg,", analysis_year, "ACS")
  ) +
  theme_minimal()