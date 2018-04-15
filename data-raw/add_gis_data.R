library(sf)

list.files("data-raw/2013 Digital Boundaries Generalised Clipped/", pattern = "map")

# Read and add data
meshblocks <- st_read("data-raw/2013 Digital Boundaries Generalised Clipped/MB2013_GV_Clipped.tab")
devtools::use_data(meshblocks)

area_units <- st_read("data-raw/2013 Digital Boundaries Generalised Clipped/AU2013_GV_Clipped.tab")
devtools::use_data(area_units)

local_boards <- st_read("data-raw/2013 Digital Boundaries Generalised Clipped/CB2013_GV_Clipped.tab")
devtools::use_data(local_boards)

wards <- st_read("data-raw/2013 Digital Boundaries Generalised Clipped/WARD2013_GV_Clipped.tab")
devtools::use_data(wards)

tas <- st_read("data-raw/2013 Digital Boundaries Generalised Clipped/TA2013_GV_Clipped.tab")
devtools::use_data(tas)

regions <- st_read("data-raw/2013 Digital Boundaries Generalised Clipped/REGC2013_GV_Clipped.tab")
devtools::use_data(regions)
