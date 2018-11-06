context("test-transform-census.R")
library(sf)
library(tidyr)
library(nzcensr)


test_that("transform-census is the same as the data imported", {

  # Retrieve test data and perform operation manually.
  dwelling_area_units_test <- dwelling_area_units

  # Get the outputs of the function call
  dwelling_area_units_read <- transform_census(dwelling_area_units)

  # Check that they're equivalent
  expect_identical(dwelling_area_units_test, dwelling_area_units_read)
})

test_that("transform-census drops GIS column correctly", {

  # Retrieve test data and perform operation manually.
  dwelling_area_units_test <- dwelling_area_units
  st_geometry(dwelling_area_units_test) <- NULL

  # Get the outputs of the function call
  dwelling_area_units_read <- transform_census(dwelling_area_units, gis = FALSE)

  # Check that they're equivalent
  expect_equal(dwelling_area_units_test, dwelling_area_units_read)
})

test_that("transform-census import long data correctly with no gis", {

  # Retrieve test data and perform operation manually.
  dwelling_area_units_test <- dwelling_area_units
  st_geometry(dwelling_area_units_test) <- NULL
  dwelling_area_units_test <- tidyr::gather(dwelling_area_units_test, variable, value, -1, -2, -3)

  # Get the outputs of the function call
  dwelling_area_units_read <- transform_census(dwelling_area_units, gis = FALSE, long = TRUE)

  # Check that they're equivalent
  expect_equal(dwelling_area_units_test, dwelling_area_units_read)
})

test_that("transform-census import long data correctly WITH gis", {

  # Retrieve test data and perform operation manually.
  dwelling_regions_test <- dwelling_regions
  dwelling_regions_test <- gather(dwelling_regions_test, variable, value, -1, -2, -3, -geometry)

  # Get the outputs of the function call
  dwelling_regions_read <- transform_census(dwelling_regions, gis = TRUE, long = TRUE)

  # Check that they're equivalent
  expect_equal(data.frame(dwelling_regions_test), data.frame(dwelling_regions_read))
})

test_that("transform-census converts the CRS correctly", {

  # Retrieve test data and perform operation manually.
  dwelling_regions_test <- dwelling_regions
  dwelling_regions_test <- sf::st_transform(dwelling_regions_test, 4326)

  # Get the outputs of the function call
  dwelling_regions_read <- transform_census(dwelling_regions, crs = 4326)

  # Check that they're equivalent
  expect_equal(data.frame(dwelling_regions_test), data.frame(dwelling_regions_read))
})

test_that("transform-census cleans correctly", {

  # Retrieve test data and perform operation manually.
  dwelling_regions_long <- transform_census(dwelling_regions, gis = TRUE, long = TRUE, clean = TRUE)

  # Check that they're equivalent
  expect_equal(ncol(dwelling_regions_long), 8)
})


