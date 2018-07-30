context("test-filter-by-area.R")

test_that("filter-by-area-expect-errors", {

  # Expect errors!
  expect_error(filter_by_area(dwelling_area_units, "region", "Wellington"))

  dwelling_area_uni <- dwelling_area_units
  expect_error(filter_by_area(dwelling_area_uni, "regions", "Wellington"))

  x_dwelling_area_units <- dwelling_area_units %>% rename(test = Area_Code_and_Description)
  expect_error(filter_by_area(x_dwelling_area_units, "regions", "Wellington"))
})

test_that("filter-by-area-check-northland", {
  northland_area_units <- filter_by_area(dwelling_area_units, "regions", "northland")

  expect_equal(nrow(northland_area_units), 84)
})
