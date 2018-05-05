# Several functions which are not exposed to make life easier internally

table_to_long <- function(data){
  #' Convert wide to long
  #'
  #' @description Merely converts a census dataframe from wide to long.
  #'
  #' @param data The data set to be transformed.
  #' @importFrom dplyr everything
  #' @importFrom tidyr gather

  if ("geometry" %in% colnames(data)) gather.sf(data, variable, value, -1, -2, -3, -geometry)
  else tidyr::gather(data, variable, value, -1, -2, -3)

}

clean_census_columns <- function(data){
  #' Clean variables
  #'
  #' Cleans the variable names and splits them into columns in a long table.
  #'
  #' @param data The data frame to be cleaned. Must be a long dataframe returned by read_nz_census_data()
  #' @examples
  #' nz_dwelling_regions_long <- read_nz_census_data(dwelling_regions,
  #'   include_gis = FALSE,
  #'   long = TRUE,
  #'   replace_confidential_values = NA_integer_)
  #' cleaned_data <- clean_census_columns(nz_dwelling_regions_long)
  #' head(cleaned_data)
  #' @export
  #' @importFrom stringr str_replace str_replace_all
  #' @importFrom dplyr pull mutate select everything
  #' @importFrom magrittr "%>%"
  #' @importFrom sf st_geometry

  # Extract geography
  if ("sf" %in% class(data) | "sfc" %in% class(data)) {
    is_geog <- TRUE
    geog <- sf::st_geometry(data)
    sf::st_geometry(data) <- NULL
  } else is_geog <- FALSE

  # Extract variable columns
  variables <- dplyr::pull(data, variable)

  # Extract years
  years <- stringr::str_extract(variables, "....")

  # Extract "topics" and "variables"
  census_topics_variables <- variables %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_replace("20.. Census ", "") %>%
    stringr::str_split(" ") %>%
    lapply(split_topics_variables)

  census_topics_variables <- do.call("rbind", census_topics_variables) %>%
    dplyr::mutate(year = years) %>%
    dplyr::select(year, 1:3)

  cleaned_data <- cbind(data %>% dplyr::select(-variable), census_topics_variables) %>%
    dplyr::select(1:3, year, topic, variable, dplyr::everything())

  if (is_geog) {
    sf::st_geometry(cleaned_data) <- geog
  }

  return(cleaned_data)
}

split_topics_variables <- function(topic_variable){
  #' Split the topics and variables
  #'
  #' Splits the variavble column of the long data into years, topics and variables.
  #' Only intended for use inside of clean_census_variables()
  #'
  #' @param topic_variable The column with the topic and variable stuck together e.g. ("variable" column from long output)
  #' @importFrom stringr str_detect

  topic <- topic_variable[!stringr::str_detect(topic_variable, "[A-Z]")] %>% paste(collapse = " ")
  variable <- topic_variable[stringr::str_detect(topic_variable, "[A-Z]")] %>% paste(collapse = " ")

  return(data.frame(topic = topic, variable = variable))

  }
