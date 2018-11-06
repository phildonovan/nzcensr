# A few functions to retrieve census data with
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

nz_census_tables <- function(table_name = "", variables = FALSE){
  #' Returns a table of census data available
  #'
  #' @description Shows what data sets are available and what they're called. For an
  #' in-depth query of a data set specify the table and variable.
  #'
  #' @param table_name The name of the table that for a description. Must be a string e.g. "dwelling_area_units". 
  #' @param variables Whether to show variables of a table or not. Table must be specified when checking out a specific variable.
  #'
  #' @return A table describing the data set(s).
  #'
  #' @importFrom tibble tribble
  #' @importFrom stringr str_detect str_replace_all
  #' @export
  
  table_description <-
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
  
  # Check if a table name is specified, else return all the tables.
  if (table_name != "") {
    
    # Check if the string is in the table_description table
    if (table_name %in% table_description[["dataset"]]) {
      
      # Get the table from the table name string
      table <- eval(parse(text=table_name))
      
      # Return table topics
      table_headers <- colnames(table)
      table_topics <- table_headers[stringr::str_detect(table_headers, "Census")]
      table_topics_clean <- extract_topics(table_topics) %>%
        stringr::str_replace_all(" ", "_")
      
      if (variables == TRUE) {
        
        table_topics_clean <- extract_topics(table_topics, unique_topics = FALSE) %>%
          stringr::str_replace_all(" ", "_")
        table_variables_clean <- extract_variables(table_topics)
        
        table <- 
          as_tibble(
            list(topic = table_topics_clean,
                 variable = table_variables_clean)) %>%
          distinct(topic, variable)
        
      } else {
        table <- as_tibble(list(topics = table_topics_clean))
      }
    }  else {
      stop("Table not in the census. Please check the tables available with nz_census_tables()")
    }
    
  } else {
    
    # Return the table
    table <- table_description
  }
  
  return(table)
  
}

transform_census <- function(.data, gis = TRUE,
                             crs = 2193, long = FALSE, clean = FALSE){
  #' Transform nz census data
  #'
  #' @description Performs various common manipulation tasks on the census data. Mainly meant for easy transformations
  #' such as whether to include the GIS data, what its CRS system should be (defaults to 2193 -- NZTM) and whether it
  #' should come in the wide (default) or long format.
  #'
  #' @param .data The data set to be passed.
  #' @param gis If the data set should include the gis column (defaults to TRUE).
  #' @param crs The desired Coordinate Reference System of the data set (defaults to 2193). Only
  #' important if the data includes a geometry column.
  #' @param long Whether the data should be returned in the long format or not.
  #' @param clean True/False on whether to separate the original census column headers into year, 'topics' and 'variables'.
  #'
  #' @return It returns either a tibble or a simple features dataframe.
  #'
  #' @export
  
  # Drop geometry column
  if (gis == FALSE) sf::st_geometry(.data) <- NULL
  
  # Perform CRS transformation
  if (gis == TRUE & crs != 2193) .data <- sf::st_transform(.data, crs)
  
  # Convert to long
  if (long == TRUE) .data <- table_to_long(.data)
  
  # Clean columns
  if (clean == TRUE & long == TRUE) .data <- clean_census_columns(.data)
  else if (clean == TRUE & long == FALSE) stop("To clean the data, it must be in the long format. Specifiy long = TRUE in the call.")
  
  return(.data)
}

