## Introduction

[![Travis-CI Build Status](https://travis-ci.org/phildonovan/nzcensr.svg?branch=master)](https://travis-ci.org/phildonovan/nzcensr)

A library largely composed of tables representing the [New Zealand Census](https://www.stats.govt.nz/) and a few helper functions to access it.
The data can be either a normal dataframe (tibble) or a simple features data table (GIS).

NOTE: This is my first package so please let me know what I am doing wrong. 

## Installation

To install the package:

    devtools::install_github(phildonovan/nzcensr)

## Usage

Upon loading, the `nzcensr` package brings a series of tables if the censes: dwelling, family, household, individual part 1, individual part 2, individual part 3a, and individual part3b. 
These tables are all available at the five different geographical levels: meshblocks, area units, local boards, territorial authorities and regions. 
All of these tables are only loaded when called ('lazy loading').

At present, it only has two functions:

  1. `nz_census_data()` and
  2. `read_nz_census_data()`.
  
By default, `nz_census_data()` provides information on the tables made available by `nzcensr`. 
If a table is specified, then it returns all of the columns from table specified e.g. `nzcensr(dwelling_area_units)`.
Meawhile the `read_nz_census_data()` makes it easy to get the data in the shape the user wants such as whether it has the geometry column included, what to replace confidential values with, the Coordinate Reference System (defaults to New Zealand Transverse Mercator)  and / or whether it is in wide (default) or long format.

For a demonstration of its use, please check out the vignette []().

## Author

  * Phil Donovan
  
## TODO

Things that need to be done:

  * Add in testing,
  * badges,
  * strictly GIS tables (now census data),
  * improve vignette,
  * more functionality.
