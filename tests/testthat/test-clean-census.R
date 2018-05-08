context("test-clean-census.R")
library(nzcensr)

test_that("clean_census works", {

  # Import long data (without GIS for speed)
  nz_regions_long <- transform_census(dwelling_regions, include_gis = FALSE, long = TRUE)

  # Clean
  nz_regions_long_clean <- clean_census_columns(nz_regions_long)

  # Test
  expect_equal(nrow(nz_regions_long_clean), 2142)
  expect_equal(ncol(nz_regions_long_clean), 7)
})
