# Several functions which are not exposed to make life easier internally

table_to_long <- function(data_set){
  #' Convert wide to long
  #'
  #' @description Merely converts a census dataframe from wide to long.
  #' @importFrom tidyr gather

  if ("geometry" %in% colnames(data_set)) sf::gather.sf(data_set, variable, value, -1, -2, -3, -geometry)
  else tidyr::gather(data_set, variable, value, -1, -2, -3)

}

clean_census_columns <- function(long_df){
  #' Clean variables
  #'
  #' Cleans the variable names and splits them into columns in a long table.
  #'
  #' @param long_df The data frame to be cleaned. Must be a long dataframe returned by read_nz_census_data()
  #' @examples
  #' nz_dwelling_regions_long <- read_nz_census_data(dwelling_regions, include_gis = FALSE, long = TRUE, replace_confidential_values = NA_integer_)
  #' cleaned_data <- clean_census_columns(nz_dwelling_regions_long)
  #' head(cleaned_data)
  #' @export
  #' @importFrom stringr str_replace str_replace_all
  #' @importFrom dplyr pull mutate select

  # Extract variable columns
  variables <- dplyr::pull(long_df, variable)

  # Extract years
  years <- stringr::str_extract(variables, "....")

  # Extract "topics" and "variables"
  census_topics_variables <- variables %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_replace("20.. Census ", "") %>%
    stringr::str_split(" ") %>%
    lapply(split_topics)

  census_topics_variables <- do.call("rbind", census_topics_variables) %>%
    dplyr::mutate(year = years) %>%
    dplyr::select(year, 1:3)

  cleaned_data <- cbind(long_df %>% dplyr::select(-variable), census_topics_variables) %>%
    dplyr::select(1:3, year, topic, variable, everything())

  return(cleaned_data)


}

split_topics_variables <- function(topic_variable){
  #' Split the topics and variables
  #'
  #' Splits the variavble column of the long data into years, topics and variables.
  #' Only intended for use inside of clean_census_variables()
  #' @importFrom stringr str_detect

  topic <- topic_variable[!stringr::str_detect(topic_variable, "[A-Z]")] %>% paste(collapse = " ")
  variable <- topic_variable[stringr::str_detect(topic_variable, "[A-Z]")] %>% paste(collapse = " ")

  return(data.frame(topic = topic, variable = variable))

  }
