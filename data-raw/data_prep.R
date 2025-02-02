# Script to prepare gcdf_country_commitments dataset
library(dplyr)
library(tidyr)
library(chinadevfin3)
library(wbwdi)
library(imfweo)
library(janitor)

# Get GCDF data and calculate total commitments
gcdf_data <- get_gcdf3_dataset() |>
  filter(recommended_for_aggregates == "Yes")

total_commitments <- gcdf_data |>
  group_by(country_name, iso3c) |>
  summarize(
    total_commitments_bn = sum(amount_constant_usd_2021, na.rm = TRUE) * 1e-9
  ) |>
  ungroup() |>
  filter(!is.na(iso3c)) |>
  arrange(desc(total_commitments_bn))

# Calculate weighted interest rates
weighted_interest_rate_tbl <- gcdf_data |>
  filter(!is.na(interest_rate)) |>
  mutate(
    commitment_x_interest = amount_constant_usd_2021 * interest_rate
  ) |>
  group_by(iso3c) |>
  summarize(
    total_commitment_x_interest = sum(commitment_x_interest, na.rm = TRUE),
    total_commitments = sum(amount_constant_usd_2021, na.rm = TRUE)
  ) |>
  mutate(
    weighted_interest_rate = total_commitment_x_interest / total_commitments
  ) |>
  select(
    iso3c,
    weighted_interest_rate
  ) |>
  filter(!is.na(weighted_interest_rate))

# Get World Bank country classifications
country_features <- wdi_get_geographies() |>
  filter(geography_type == "Country") |>
  select(
    iso3c = geography_id,
    region_name,
    income_level_name
  )

# Get IMF WEO GDP and population data
country_features_pop_gdp <- weo_bulk(
  year = 2024,
  release = "Fall"
) |>
  filter(
    series %in% c(
      "NGDPD",
      "LP"
    ),
    year == 2021
  ) |>
  select(
    iso,
    subject,
    value
  ) |>
  pivot_wider(
    names_from = subject,
    values_from = value,
  ) |>
  clean_names() |>
  rename(
    iso3c = iso,
    gdp_usd_bn = gross_domestic_product_current_prices,
    population_mn = population
  )

# Combine all data
gcdf_country_commitments <- total_commitments |>
  left_join(
    weighted_interest_rate_tbl,
    by = join_by(iso3c)
  ) |>
  left_join(
    country_features,
    by = join_by(iso3c)
  ) |>
  left_join(
    country_features_pop_gdp,
    by = join_by(iso3c)
  ) |>
  select(
    country_name,
    iso3c,
    total_commitments_bn,
    weighted_interest_rate,
    region_name,
    income_level_name,
    gdp_usd_bn,
    population_mn
  )

# Save the data to the package
usethis::use_data(gcdf_country_commitments, overwrite = TRUE)

# Create yearly flow class dataset
gcdf_yearly_flows <- gcdf_data |>
  filter(recommended_for_aggregates == "Yes") |>
  group_by(commitment_year, flow_class) |>
  summarize(
    commitments_bn = sum(amount_constant_usd_2021, na.rm = TRUE) * 1e-9,
    .groups = "drop"
  ) |>
  filter(
    !is.na(commitment_year),
    !is.na(flow_class),
    commitment_year >= 2000,  # Focus on more reliable recent data
    commitment_year <= 2021   # Last full year in dataset
  ) |>
  arrange(commitment_year, flow_class)

# Save the data to the package
usethis::use_data(gcdf_yearly_flows, overwrite = TRUE)
