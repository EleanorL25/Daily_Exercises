library(sf)
library(terra)
library(mapview)
remotes::install_github("mikejohnson51/AOI")
library(AOI)

sf::sf_extSoftVersion()
#>           GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H 
#>       "3.13.0"        "3.8.5"        "9.5.1"         "true"         "true" 
#>           PROJ 
#>        "9.5.1"

terra::gdal()
#> [1] "3.8.5"