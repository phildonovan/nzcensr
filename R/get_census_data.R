# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

get_available_census_files <- function(){
  #' Get the title of the census data available.
  #'
  #' Quick use function for help a user understand what data is available in the processed directory.
  census_files <- list.files(PROCESSED_DATA_DIR)
  return(census_files)
}

get_available_census_levels <- function(){
  #' Get the available geographic levels for the census data.
  #'
  #' Quick use function for help a user know the different geographic levels present in the data.
  census_files <- list.files(PROCESSED_DATA_DIR)[1]
  census_levels <- list.files(paste(PROCESSED_DATA_DIR, census_files, sep = "/")) %>%
    str_replace(".csv", "")

  return(census_levels)
}


read_nz_census <- function(file_name, geographic_level, include_gis=FALSE){
  #' Read NZ census data
  #'
  #' @description Main read data function which enables a user to specify the type of data they want, and
  #' the geographic level of it, and whether to include a GIS layer.
  #' Presently, if GIS is included, then it returns a list of both the census and gis data.
  #' If no GIS, then it just returns a tibble of the census data.
  #' The census data is presently returned in `long` format.
  #' FUTURE: Add in a flag for returning census data in wide format.

  # Check the inputs are good
  if ((file_name %in% get_available_census_files()) == FALSE) {
    stop(str_interp("File not found. Files must be one of ${paste(get_available_census_files(), collapse = ', ')}"))
  }

  if ((geographic_level %in% get_available_census_levels()) == FALSE) {
    stop(str_interp("File not found. Files must be one of ${paste(get_available_census_levels(), collapse = ', ')}"))

  }
  # Read in data set
  path <- paste(PROCESSED_DATA_DIR, file_name, paste0(geographic_level, ".csv"), sep = "/")
  census_data <- read_csv(path, col_types = cols())

  # Include GIS
  if (include_gis == TRUE){
    gis_files <- list.files(GIS_DATA_DIR)

    if (geographic_level == "area_units") gis_path <- "AU2013_GV_CLIPPED.tab"
    else if (geographic_level == "local_boards") gis_path <- "CB2013_GV_CLIPPED.tab"
    else if (geographic_level == "meshblocks") gis_path <- "MB2013_GV_CLIPPED.tab"
    else if (geographic_level == "regions") gis_path <- "REGC2013_GV_CLIPPED.tab"
    else if (geographic_level == "regions") gis_path <- "REGC2013_GV_CLIPPED.tab"
    else if (geographic_level == "tas") gis_path <- "TA2013_GV_CLIPPED.tab"
    else if (geographic_level == "wards") gis_path <- "WARD2013_GV_CLIPPED.tab"

    path <- paste(GIS_DATA_DIR, gis_path, sep = "/")
    gis_data <- st_read(path) %>%
      st_set_crs(2193)

    return(list(census_data = census_data, gis = gis_data))
  }

  else return(census_data)
}


