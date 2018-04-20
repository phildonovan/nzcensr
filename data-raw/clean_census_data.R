# Clean the census data csvs.

library(tidyverse)
library(sf)
library(readr)
library(stringr)
library(purrr)
library(devtools)

# Get working directory
census_files <- list.files("data-raw/2013_mb_dataset_Total_New_Zealand_CSV/", full.names = TRUE)
census_names <- list.files("data-raw/2013_mb_dataset_Total_New_Zealand_CSV/") %>%
  str_replace("2013-mb-dataset-Total-New-Zealand-", "") %>%
  str_replace(".csv", "")

census_files <- setNames(census_files, census_names)

# Define function for a) separating csv into separate geographic units, b) writing to file.
clean_census_csv <- function(census_file, write_to_file = FALSE){

  # Read file
  census_folder_name <- census_file %>%
    str_replace("data-raw/2013_mb_dataset_Total_New_Zealand_CSV//2013-mb-dataset-Total-New-Zealand-", "") %>%
    str_replace(".csv", "") %>%
    str_to_lower

  message(str_interp("Cleaning the census data for the ${str_to_lower(census_folder_name)} data set."))

  census_file <- read_csv(census_file, col_types = cols())


  # Geographic units and extraction patterns
  extraction_info <- tribble(
    ~geographic_unit, ~pattern, ~gis_file,
    "meshblocks",    "^MB ", NA,
    "area_units",      "^(\\d{6,6})\\b",    NA,
    "wards", "^(\\d{5,5})\\b", NA,
    "tas", "^(\\d{3,3})\\b", NA,
    "regions", "^(\\d{2,2})\\b", NA,
    "local_boards", "^CMB ", NA,
  ) %>%
    split(.$geographic_unit)

  # Extract data
  clean_data <- map(extraction_info, extract_data, census_file = census_file)

  # Write to file(s)
  if (write_to_file == TRUE){
    path <- paste("data-raw/census_2013", census_folder_name, sep = "/")
    if (dir.exists(path) == FALSE) dir.create(path)

    lapply(seq(clean_data), function(census_geography) {
      path <- paste0(path, "/", names(clean_data)[census_geography], ".csv")
      write_csv(clean_data[[census_geography]], path)
    })
  }

  return(clean_data)
}

extract_data <- function(extraction_info, census_file, set_confidential_values = 1){

  # Loop over extraction info, and extract data, and join it to gis info.
  pattern <- extraction_info[["pattern"]]

  # Filter all census data
  data <- dplyr::filter(census_file, str_detect(Area_Code_and_Description, pattern))

  # convert "..C" to new number
  data <- tidyr::gather(data, census_column, count, -1, -2, -3) %>%
    dplyr::mutate(count = dplyr::case_when(
      stringr::str_detect(count, fixed("*")) == TRUE ~ NA_character_,
      # str_detect(count, "..C") == TRUE ~ "1",
      TRUE ~ count)
    ) %>%
    tidyr::spread(census_column, count)

  # Attach geography
  # Insert join here.

  return(data)

}

# Perform extraction
cleaned_census_files <- map(census_files, clean_census_csv)

# Load GIS.
gis <- list(
  meshblocks = st_read("data-raw/2013 Digital Boundaries Generalised Clipped/MB2013_GV_Clipped.tab"),
  area_units = st_read("data-raw/2013 Digital Boundaries Generalised Clipped/AU2013_GV_Clipped.tab"),
  local_boards = st_read("data-raw/2013 Digital Boundaries Generalised Clipped/CB2013_GV_Clipped.tab"),
  wards = st_read("data-raw/2013 Digital Boundaries Generalised Clipped/WARD2013_GV_Clipped.tab"),
  tas = st_read("data-raw/2013 Digital Boundaries Generalised Clipped/TA2013_GV_Clipped.tab"),
  regions = st_read("data-raw/2013 Digital Boundaries Generalised Clipped/REGC2013_GV_Clipped.tab")
)

x <- lapply(seq_along(cleaned_census_files), function(index) {

  layer_name <- names(cleaned_census_files)[index]
  layer_name_clean <- str_replace_all(layer_name, "-", "_") %>%
    str_to_lower()

  layer_list <- cleaned_census_files[[layer_name]]

  parallel::mclapply(seq_along(layer_list), function(layer_index) {
    layer <- layer_list[[layer_index]]
    layer_name <- names(layer_list)[layer_index]

    join_col <- colnames(layer)[2]
    join_col_gis <- colnames(gis[[layer_name]])[1]

    new_name <- paste(layer_name_clean, layer_name, sep = "_")

    new_dataset <- dplyr::left_join(layer, dplyr::select(gis[[layer_name]], 1), by = setNames(join_col_gis, join_col)) %>%
      st_sf
    st_crs(new_dataset) <- 2193

    new_dataset <- dplyr::filter(new_dataset, !is.na(st_dimension(st_geometry(new_dataset))))

    assign(new_name, new_dataset)
    do.call("use_data", list(as.name(new_name), overwrite = TRUE))
  }, mc.cores = 3)
})
