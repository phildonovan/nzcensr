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
  dwelling_area_units_read <- transform_census(dwelling_area_units, include_gis = FALSE)

  # Check that they're equivalent
  expect_equal(dwelling_area_units_test, dwelling_area_units_read)
})

test_that("transform-census import long data correctly with no gis", {

  # Retrieve test data and perform operation manually.
  dwelling_area_units_test <- dwelling_area_units
  st_geometry(dwelling_area_units_test) <- NULL
  dwelling_area_units_test <- tidyr::gather(dwelling_area_units_test, variable, value, -1, -2, -3)

  # Get the outputs of the function call
  dwelling_area_units_read <- transform_census(dwelling_area_units, include_gis = FALSE, long = TRUE)

  # Check that they're equivalent
  expect_equal(dwelling_area_units_test, dwelling_area_units_read)
})

test_that("transform-census import long data correctly WITH gis", {

  # Retrieve test data and perform operation manually.
  dwelling_regions_test <- dwelling_regions
  dwelling_regions_test <- gather(dwelling_regions_test, variable, value, -1, -2, -3, -geometry)

  # Get the outputs of the function call
  dwelling_regions_read <- transform_census(dwelling_regions, include_gis = TRUE, long = TRUE)

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
  dwelling_regions_long <- transform_census(dwelling_regions, include_gis = TRUE, long = TRUE, clean = TRUE)

  # Check that they're equivalent
  expect_equal(ncol(dwelling_regions_long), 8)
})

test_that("transform_data replaces the '..C' values correctly", {

  # Replacement value
  replacement_value <- NA_integer_

  # Read in data for test case
  dwelling_au_test <- transform_census(dwelling_area_units, include_gis = FALSE, long = TRUE) %>%
    dplyr::mutate(test_id = row_number())

  # Grab the ..C values
  dwelling_au_test..C <- dplyr::filter(dwelling_au_test, value == "..C") %>%
    dplyr::mutate(value = replacement_value,
                  value = as.integer(value))

  ..C_id <- dplyr::pull(dwelling_au_test..C, test_id)

  # Set up transform return value
  dwelling_au_read <- transform_census(dwelling_area_units, include_gis = FALSE, long = TRUE, replace_confidential_values = replacement_value) %>%
    dplyr::mutate(test_id = row_number())

  # read ..C values
  dwelling_au_read..C <- dplyr::filter(dwelling_au_read, test_id %in% ..C_id)

  # Test equal
  testthat::expect_equal(dwelling_au_test..C, dwelling_au_read..C)

})
