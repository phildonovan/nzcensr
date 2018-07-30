context("test-select-topics.R")

test_that("check select topics returns a tibble with nrows and ncols > 0", {

  # Create some wanted topics
  wanted_topics <- c("dwelling_record_type_for_occupied_dwellings", "fuel_types_used_to_heat_dwellings_(total_responses)(4)_for_occupied_private_dwellings")

  # Use filter_topics without excluding
  result_1 <- select_by_topic(dwelling_area_units, wanted_topics)

  # Use filter_topics wit exclusion
  result_2 <- select_by_topic(dwelling_area_units, wanted_topics, exclude = TRUE)

  expect_true(nrow(result_1) > 0)
  expect_true(ncol(result_1) > 0)

  expect_true(nrow(result_2) > 0)
  expect_true(ncol(result_2) > 0)
})
