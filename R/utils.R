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

  # Extract year and create cleaned variable variable.
  .data <- mutate(.data,

                  # Extract the year
                  year = stringr::str_extract(variable, "...."),

                  # Remove it from the column incl the "Census" word from all columns.
                  variable = stringr::str_replace(variable, "20.._Census_", ""),

                  # Create the topic variable by finding the location of the
                  # first capital letter or digit preceded by an underscore _[A-Z] and taking everything in front of it
                  topic = stringr::str_sub(variable,
                                            start = 0,
                                            end = stringr::str_locate(variable, "_[A-Z]|_[0-9]")[row_number(), 1]) %>%
                    stringr::str_replace_all("_", " "),

                  # Create the variable as the remaing part of the string after the "_[A-Z]"
                  cleaned_variable = stringr::str_sub(variable,
                                                       start = stringr::str_locate(variable, "_[A-Z]|_[0-9]")[row_number(), 2]) %>%
                    stringr::str_replace_all("_", " "),

                  cleaned_variable = ifelse(is.na(cleaned_variable), variable, cleaned_variable)
  )

  # Create new data frame and get all of the column in the desired order.
  .data <- select(.data, 1:3, year, topic, variable = cleaned_variable, value)

  # Add the geometry column back in if it was present in the first place.
  if (is_geog) {
    sf::st_geometry(.data) <- geog
  }

  return(.data)
}

replace_confidential <- function(.data, replacement_value = NA_integer_){
  #' Replace confidential values
  #'
  #' The NZ census commonly uses the notation of '..C' when values are below a
  #' certain threshold that they may reveal private details of certain individuals.
  #' However, it is often required in an analysis to replace these values and
  #' convert the column to an integer (to include ..C they need to be character).
  #'
  #' This function takes the data set, and a replacement value and replaces all of the ..C values.
  #' Mainly for use within the transform_census function.
  #'
  #' @param .data The data set to have its confidential values removed.
  #' @param replacement_value The value to replace the confidential ones with. Defaults to NA_integer.
  #' @export

  # Need to actually convert to function.

  if (!is.numeric(replacement_value)) stop("Replacement value must be a number or NA_integer_")

  do_not_mutate <- c("Area_Code_and_Description", "Code", "Description", "geometry")
  replace_confidential_cols <- colnames(.data)[!(colnames(.data) %in% do_not_mutate)]
  replacement_value <- as.character(replacement_value)

  if(is.na(replacement_value)) {
    replacement_function <- function(col) {suppressWarnings(as.integer(col))}
  } else {
    replacement_function <- function(col) {as.integer(stringr::str_replace(col, "..C", replacement_value))}
  }
  .data <- dplyr::mutate_at(.data, dplyr::vars(replace_confidential_cols), dplyr::funs(replacement_function))
  .data <- sf::st_as_sf(.data)

  return(.data)
}

extract_topics <- function(raw_topics, unique_topics = TRUE){
  #' Extract topic
  #'
  #' Extracts the topics for a raw census column from a vector strings.
  #'
  #' @param raw_topics A vector of raw topics.
  #' @param unique_topics Whether the results should be unique or not. Defaults to TRUE.
  #'
  #' @return A vector of topics

  # Get unique values
  raw_topics <- unique(raw_topics)

  # Extract years and then strip away year
  no_year <- stringr::str_replace(raw_topics, "^20.._Census_", "")

  # Locate the first capital letter.
  capital_letter_locations <- stringr::str_locate(no_year, "_[A-Z]|_[0-9]")[,1]

  # Generate sequence to iterate over
  sequence <- seq_along(no_year)
  cleaned_topics <- sapply(sequence, function(x){
    stringr::str_sub(no_year[x], start = 0, end = capital_letter_locations[x] - 1) %>%
      stringr::str_replace_all("_", " ")
  })

  if (unique_topics == TRUE) cleaned_topics <- unique(cleaned_topics)

  return(cleaned_topics)
}

extract_variables <- function(raw_variables){
  #' Extract variable
  #'
  #' Extracts the variables for a raw census column from a vector strings.
  #'
  #'
  #' @param raw_variables A vector of census columns containing the raw variables.
  #'
  #' @return A vector of variables

  # Get unique values
  raw_variables <- unique(raw_variables)

  # Strip away year
  no_year <- stringr::str_replace(raw_variables, "^20.._Census_", "")

  # Locate the first capital letter.
  capital_letter_locations <- stringr::str_locate(no_year, "_[A-Z]|_[0-9]")[,2]

  # Generate sequence to iterate over
  sequence <- seq_along(no_year)
  sapply(sequence, function(x){
    stringr::str_sub(no_year[x],
                     start = capital_letter_locations[x]) %>%
      stringr::str_replace_all("_", " ")
  })
}

select_census_topics <- function(.data, topics, exclude = FALSE){
  #' Filter the census topics in a census dataframe
  #'
  #' This is a helper function for quickly and easily selecting or excluding topics in a nz census data set.
  #'
  #' @param .data The data frame from which the topics should be selected/excluded from.
  #' @param topics The census topics to be selected or excluded.
  #' @param exclude Whether the topics should be selected or excluded.
  #'
  #' @return A dataframe
  #' @export

  # Protect regular expression characters
  topics_protected <- str_replace_all(topics, "(\\W)", "\\\\\\1")
  topics_regex <- paste(topics_protected, collapse = "|")

  # Detect the columns that match the vectors given.
  all_columns <- colnames(.data)
  topic_column_match <- str_detect(all_columns, topics_regex)

  # Flip the logical if the excluding.
  if (exclude == TRUE) topic_column_match <- !topic_column_match

  # Check that the topics are actually in the dataframe
  if (any(topic_column_match) == FALSE) stop("No columns match the topics you've given me in the data set.")

  # Get the column names that match (including the description columns e.g. Description, Code, geometry etc.)
  descriptive_columns <- all_columns[!str_detect(all_columns, "Census")]
  matching_columns <- all_columns[topic_column_match]
  wanted_columns <- c(descriptive_columns, matching_columns)

  # Select and return data frame.
  select(.data, one_of(wanted_columns))
}
