# nzcensr
[![Travis-CI Build Status](https://travis-ci.org/phildonovan/nzcensr.svg?branch=master)](https://travis-ci.org/phildonovan/nzcensr)
 [![Coverage status](https://codecov.io/gh/phildonovan/nzcensr/branch/master/graph/badge.svg)](https://codecov.io/github/phildonovan/nzcensr?branch=master)


<a href="http://www.spatialanalytics.co.nz/packages/nzcensr/"><img align="right" src="https://user-images.githubusercontent.com/1796295/39080457-148a54be-4583-11e8-936b-99cfb36f936e.png" height="200" width="200" /></a>

A library largely composed of tables representing the [New Zealand Census](https://www.stats.govt.nz/) and a helper functions to manipulate them.
The data can be either a normal dataframe (tibble) or a simple features data table (GIS).

Check out the vignette and package help at [spatialanalytics.co.nz](https://www.spatialanalytics.co.nz/packages/nzcensr/).

## Installation

Please be aware that this is a rather large package due to the amount of data contained within it, and therefore takes a little while to download. 

To install the package:

    devtools::install_github("phildonovan/nzcensr")
    
# Get going

Three main functions for use:

    # Load libraries
    library(nzcensr)
    library(tidyverse)
    library(sf)
    
    # Select topic, filter to Hamilton, transform
    hamilton_city_council_ethnic_groups <- select_by_topic() %>% 
      filter_by_area() %>% 
      transform_census(long = TRUE, clean = TRUE, replace_confidential_values = 1)

## Author

  * Phil Donovan
  
