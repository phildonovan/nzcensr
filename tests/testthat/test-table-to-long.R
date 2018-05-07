context("test-table-to-long.R")
library(nzcensr)

test_that("test-table-to-long", {

  # Read in data in long format
  dwelling_regions_long <- transform_census(dwelling_regions, include_gis = FALSE, long = TRUE)

  expect_equal(nrow(dwelling_regions_long), 2142)
})
