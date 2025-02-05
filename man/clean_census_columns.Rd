% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{clean_census_columns}
\alias{clean_census_columns}
\title{Clean variables}
\usage{
clean_census_columns(.data)
}
\arguments{
\item{.data}{The data frame to be cleaned. Must be a long dataframe returned by read_nz_census_data()}
}
\description{
Cleans the variable names and splits them into columns in a long table.
}
\examples{
nz_dwelling_regions_long <- transform_census(dwelling_regions,
  gis = FALSE,
  long = TRUE)
cleaned_data <- clean_census_columns(nz_dwelling_regions_long)
head(cleaned_data)
}
