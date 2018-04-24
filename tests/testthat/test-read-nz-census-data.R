context("test-read-nz-census-data.R")
library(sf)
library(tidyverse)
library(nzcensr)


test_that("read-nz-census-data is that same as the data imported", {

  # Retrieve test data and perform operation manually.
  dwelling_area_units_test <- dwelling_area_units

  # Get the outputs of the function call
  dwelling_area_units_read <- read_nz_census_data(dwelling_area_units)

  # Check that they're equivalent
  expect_identical(dwelling_area_units_test, dwelling_area_units_read)
})

test_that("read-nz-census-data drops GIS column correctly", {

  # Retrieve test data and perform operation manually.
  dwelling_area_units_test <- dwelling_area_units
  st_geometry(dwelling_area_units_test) <- NULL

  # Get the outputs of the function call
  dwelling_area_units_read <- read_nz_census_data(dwelling_area_units, include_gis = FALSE)

  # Check that they're equivalent
  expect_equal(dwelling_area_units_test, dwelling_area_units_read)
})

test_that("read-nz-census-data import long data correctly with no gis", {

  # Retrieve test data and perform operation manually.
  dwelling_area_units_test <- dwelling_area_units
  st_geometry(dwelling_area_units_test) <- NULL
  dwelling_area_units_test <- tidyr::gather(dwelling_area_units_test, variable, value, -1, -2, -3)

  # Get the outputs of the function call
  dwelling_area_units_read <- read_nz_census_data(dwelling_area_units, include_gis = FALSE, long = TRUE)

  # Check that they're equivalent
  expect_equal(dwelling_area_units_test, dwelling_area_units_read)
})

test_that("read-nz-census-data import long data correctly WITH gis", {

  # Retrieve test data and perform operation manually.
  dwelling_regions_test <- dwelling_regions
  dwelling_regions_test <- sf::gather.sf(dwelling_regions_test, variable, value, -1, -2, -3, -geometry)

  # Get the outputs of the function call
  dwelling_regions_read <- read_nz_census_data(dwelling_regions, include_gis = TRUE, long = TRUE)

  # Check that they're equivalent
  expect_equal(data.frame(dwelling_regions_test), data.frame(dwelling_regions_read))
})

test_that("read-nz-census-data converts the CRS correctly", {

  # Retrieve test data and perform operation manually.
  dwelling_regions_test <- dwelling_regions
  dwelling_regions_test <- sf::st_transform(dwelling_regions_test, 4326)

  # Get the outputs of the function call
  dwelling_regions_read <- read_nz_census_data(dwelling_regions, crs = 4326)

  # Check that they're equivalent
  expect_equal(data.frame(dwelling_regions_test), data.frame(dwelling_regions_read))
})
