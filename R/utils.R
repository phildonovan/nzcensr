# Several functions which are not exposed to make life easier internally

table_to_long <- function(data_set){
  #' Convert wide to long
  #'
  #' @description Merely converts a census dataframe from wide to long.
  #' @importFrom tidyr gather

  if ("geometry" %in% colnames(data_set)) sf::gather.sf(data_set, variable, value, -1, -2, -3, -geometry)
  else tidyr::gather(data_set, variable, value, -1, -2, -3)

}
