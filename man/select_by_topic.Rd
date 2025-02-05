% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{select_by_topic}
\alias{select_by_topic}
\title{Filter the census topics in a census dataframe}
\usage{
select_by_topic(.data, topics, exclude = FALSE)
}
\arguments{
\item{.data}{The data frame from which the topics should be selected/excluded from.}

\item{topics}{A vector of census topics to be selected or excluded e.g.
(c("number_of_rooms_for_occupied_private_dwellings", "number_of_bedrooms_for_occupied_private_dwellings")).}

\item{exclude}{Whether the topics should be selected or excluded.}
}
\value{
A dataframe
}
\description{
This is a helper function for quickly and easily selecting or excluding topics on a wide nzcensr data set.
}
\details{
Really just a wrapper around a str_detect("pattern_1|pattern_2") selection.
}
\examples{
select_by_topic(dwelling_area_units, c("fuel_types", "private_dwelling"))

}
