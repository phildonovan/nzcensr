context("test-table-to-long.R")
library(nzcensr)

test_that("test-table-to-long", {

  # Read in data in long format
  dwelling_regions_long <- read_nz_census_data(dwelling_regions, include_gis = FALSE, long = TRUE)

  expect_equal(nrow(dwelling_regions_long), 2142)
})
