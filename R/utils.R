# Several functions which are not exposed to make life easier internally

table_to_long <- function(data){
  #' Convert wide to long
  #'
  #' @description Merely converts a census dataframe from wide to long.
  #'
  #' @param data The data set to be transformed.
  #' @export

  # Set NULL values to remove notes
  variable <- NULL
  value <- NULL
  geometry <- NULL

  if ("geometry" %in% colnames(data)) gather(data, variable, value, -1, -2, -3, -geometry)
  else gather(data, variable, value, -1, -2, -3)

}

clean_census_columns <- function(.data){
  #' Clean variables
  #'
  #' Cleans the variable names and splits them into columns in a long table.
  #'
  #' @param .data The data frame to be cleaned. Must be a long dataframe returned by read_nz_census_data()
  #' @examples
  #' nz_dwelling_regions_long <- transform_census(dwelling_regions,
  #'   include_gis = FALSE,
  #'   long = TRUE,
  #'   replace_confidential_values = NA_integer_)
  #' cleaned_data <- clean_census_columns(nz_dwelling_regions_long)
  #' head(cleaned_data)
  #' @export
  #' @importFrom magrittr "%>%"

  # Set NULL variables to avoid package test 'notes'
  variable <- NULL
  years <- NULL
  topic <- NULL
  year <- NULL
  value <- NULL

  # Extract geography
  if ("sf" %in% class(.data) | "sfc" %in% class(.data)) {
    is_geog <- TRUE
    geog <- sf::st_geometry(.data)
    sf::st_geometry(.data) <- NULL
  } else is_geog <- FALSE

  # Split out year, topic and variable from variable
  .data <- mutate(.data,

                  # Extract the year
                  year = stringr::str_extract(variable, "...."),

                  # Remove it from the column incl the "Census" word from all columns.
                  variable = stringr::str_replace(variable, "20.._Census_", ""),

                  # Create the topic variable by finding the location of the
                  # first _[A-Z] and taking everything in front of it
                  topic = stringr::str_sub(variable,
                                           start = 0,
                                           end = stringr::str_locate(variable, "_[A-Z]")[row_number(), 1]) %>%
                    stringr::str_replace_all("_", " "),

                  # Create the variable as the remaing part of the string after the "_[A-Z]"
                  variable = stringr::str_sub(variable,
                                              start = stringr::str_locate(variable, "_[A-Z]")[row_number(), 2]) %>%
                    stringr::str_replace_all("_", " ")
  )

  # Create new data frame and get all of the column in the desired order.
  .data <- select(.data, 1:3, year, topic, variable, value)

  # Add the geometry column back in if it was present in the first place.
  if (is_geog) {
    sf::st_geometry(.data) <- geog
  }

  return(.data)
}

