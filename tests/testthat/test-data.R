test_that("gcdf_country_commitments has correct structure", {
  # Check basic properties
  expect_s3_class(gcdf_country_commitments, "data.frame")
  expect_true(nrow(gcdf_country_commitments) > 100)
  expect_equal(ncol(gcdf_country_commitments), 8)

  # Check column names
  expected_cols <- c(
    "country_name", "iso3c", "total_commitments_bn", "weighted_interest_rate",
    "region_name", "income_level_name", "gdp_usd_bn", "population_mn"
  )
  expect_equal(sort(names(gcdf_country_commitments)), sort(expected_cols))

  # Check data types
  expect_type(gcdf_country_commitments$country_name, "character")
  expect_type(gcdf_country_commitments$iso3c, "character")
  expect_type(gcdf_country_commitments$total_commitments_bn, "double")
  expect_type(gcdf_country_commitments$weighted_interest_rate, "double")
  expect_type(gcdf_country_commitments$region_name, "character")
  expect_type(gcdf_country_commitments$income_level_name, "character")
  expect_type(gcdf_country_commitments$gdp_usd_bn, "double")
  expect_type(gcdf_country_commitments$population_mn, "double")

  # Check for missing values in key fields
  expect_true(!any(is.na(gcdf_country_commitments$country_name)))
  expect_true(!any(is.na(gcdf_country_commitments$iso3c)))
  expect_true(!any(is.na(gcdf_country_commitments$total_commitments_bn)))

  # Check numeric ranges
  expect_true(all(gcdf_country_commitments$total_commitments_bn >= 0))
  expect_true(all(gcdf_country_commitments$weighted_interest_rate >= 0,
                  na.rm = TRUE))
  expect_true(all(gcdf_country_commitments$gdp_usd_bn >= 0, na.rm = TRUE))
  expect_true(all(gcdf_country_commitments$population_mn >= 0, na.rm = TRUE))

  # Check ISO codes format
  expect_true(all(nchar(gcdf_country_commitments$iso3c) == 3))
})



test_that("gcdf_yearly_flows has correct structure", {
  # Check basic properties
  expect_s3_class(gcdf_yearly_flows, "data.frame")
  expect_equal(ncol(gcdf_yearly_flows), 3)

  # Check column names
  expected_cols <- c("commitment_year", "flow_class", "commitments_bn")
  expect_equal(names(gcdf_yearly_flows), expected_cols)

  # Check data types
  expect_type(gcdf_yearly_flows$commitment_year, "integer")
  expect_type(gcdf_yearly_flows$flow_class, "character")
  expect_type(gcdf_yearly_flows$commitments_bn, "double")

  # Check for missing values
  expect_true(!any(is.na(gcdf_yearly_flows$commitment_year)))
  expect_true(!any(is.na(gcdf_yearly_flows$flow_class)))
  expect_true(!any(is.na(gcdf_yearly_flows$commitments_bn)))

  # Check numeric ranges
  expect_true(min(gcdf_yearly_flows$commitment_year) >= 2000)
  expect_true(max(gcdf_yearly_flows$commitment_year) <= 2021)
  expect_true(all(gcdf_yearly_flows$commitments_bn >= 0))
})

test_that("gcdf_yearly_flows data is consistent", {
  # Check flow classes
  expected_classes <- c("ODA-like", "OOF-like", "Vague (Official Finance)")
  actual_classes <- unique(gcdf_yearly_flows$flow_class)
  expect_equal(sort(actual_classes), sort(expected_classes))

  # Check years are continuous
  years <- unique(gcdf_yearly_flows$commitment_year)
  expect_equal(years, seq(min(years), max(years)))

  # Total should match approximately with country totals
  yearly_total <- sum(gcdf_yearly_flows$commitments_bn)
  country_total <- sum(gcdf_country_commitments$total_commitments_bn)
  # Allow for small rounding differences and different time periods
  expect_true(abs(yearly_total - country_total) / country_total < 0.1)
})
