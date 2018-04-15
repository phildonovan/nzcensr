# nzcensr

A library largely composed of tables representing the nz census and a few helper functions to access it.
The data can be either a normal fataframe (tibble) or a simple features data table (GIS).
Ideally, the data could also be imported as the older *sp* ojects as well, for those to like hte old ways.  

## Installation

To install, you should have at least *simple features* installed. 
Further, if you wish to import *sp* objects instead, then you obviously need to have installed that library as well. 

To install the package:

    devtools::install_github(phildonovan/nzcensr)
    
Of course, for installing from github you will need devtools as well.

## Author

  * Phil Donovan
