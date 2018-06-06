context("test-topic-variable-cleaners.R")

test_that("extract topics works", {

  # Expected output string
  test_string <- c("2001_Census_dwelling_record_type_for_occupied_dwellings_Occupied_Non-private_Dwelling",
                   "2001_Census_dwelling_record_type_for_occupied_dwellings_Hut",
                   "2001_Census_fuel_types_used_to_heat_dwellings_(total_responses)(4)_for_occupied_private_dwellings_Bottled_Gas")

  expected_output_string_1 <- c("dwelling record type for occupied dwellings",
                              "fuel types used to heat dwellings (total responses)(4) for occupied private dwellings")

  expected_output_string_2 <- c("dwelling record type for occupied dwellings",
                                "dwelling record type for occupied dwellings",
                                "fuel types used to heat dwellings (total responses)(4) for occupied private dwellings")

  output_string_1 <- extract_topics(test_string)
  output_string_2 <- extract_topics(test_string, unique_topics = FALSE)

  expect_identical(output_string_1, expected_output_string_1)
  expect_identical(output_string_2, expected_output_string_2)
})

test_that("extract variables works", {

  # Expected output string
  test_string <- c("2001_Census_dwelling_record_type_for_occupied_dwellings_Occupied_Non-private_Dwelling",
                   "2001_Census_fuel_types_used_to_heat_dwellings_(total_responses)(4)_for_occupied_private_dwellings_Bottled_Gas")

  expected_output_string <- c("Occupied Non-private Dwelling",
                              "Bottled Gas")

  output_string <- extract_variables(test_string)

  expect_identical(output_string, expected_output_string)
})
