context("test-nz-census-data.R")
library(nzcensr)
library(tibble)

test_that("nz-census-data works", {
  output <-
    tibble::tribble(
      ~dataset, ~description,
      "dwelling_area_units", "Dwelling data set at the area unit level",
      "dwelling_local_boards", "Dwelling data set at the local board level",
      "dwelling_meshblocks", "Dwelling data set at the meshblock level",
      "dwelling_regions", "Dwelling data set at the regional level",
      "dwelling_tas", "Dwelling data set at the territorial authority level",
      "family_area_units", "Family data set at the area unit level",
      "family_local_boards", "Family data set at the local board level",
      "family_meshblocks", "Family data set at the meshblock level",
      "family_regions", "Family data set at the regional level",
      "family_tas", "Family data set at the territorial authority level",
      "household_area_units", "Household data set at the area unit level",
      "household_local_boards", "Household data set at the local board level",
      "household_meshblocks", "Household data set at the meshblock level",
      "household_regions", "Household data set at the regional level",
      "household_tas", "Household data set at the territorial authority level",
      "individual_part_1_area_units", "Individual (Part 1) data set at the area unit level",
      "individual_part_1_local_boards", "Individual (Part 1) data set at the local board level",
      "individual_part_1_meshblocks", "Individual (Part 1) data set at the meshblock level",
      "individual_part_1_regions", "Individual (Part 1) data set at the regional level",
      "individual_part_1_tas", "Individual (Part 1) data set at the territorial authority level",
      "individual_part_2_area_units", "Individual (Part 2) data set at the area unit level",
      "individual_part_2_local_boards", "Individual (Part 2) data set at the local board level",
      "individual_part_2_meshblocks", "Individual (Part 2) data set at the meshblock level",
      "individual_part_2_regions", "Individual (Part 2) data set at the regional level",
      "individual_part_2_tas", "Individual (Part 2) data set at the territorial authority level",
      "individual_part_3a_area_units", "Individual (Part 3A) data set at the area unit level",
      "individual_part_3a_local_boards", "Individual (Part 3A) data set at the local board level",
      "individual_part_3a_meshblocks", "Individual (Part 3A) data set at the meshblock level",
      "individual_part_3a_regions", "Individual (Part 3A) data set at the regional level",
      "individual_part_3a_tas", "Individual (Part 3A) data set at the territorial authority level",
      "individual_part_3b_area_units", "Individual (Part 3B) data set at the area unit level",
      "individual_part_3b_local_boards", "Individual (Part 3B) data set at the local board level",
      "individual_part_3b_meshblocks", "Individual (Part 3B) data set at the meshblock level",
      "individual_part_3b_regions", "Individual (Part 3B) data set at the regional level",
      "individual_part_3b_tas", "Individual (Part 3B) data set at the territorial authority level"
    )

  expect_equal(nz_census_tables(), output)
})
