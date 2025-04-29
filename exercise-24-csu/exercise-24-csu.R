library(here)
library(sf)
library(AOI)
library(dplyr)
library(ggplot2)
uscities<-readr::read_csv(here("exercise-24-csu/simplemaps_uscities_basicv1.90/uscities.csv"))
(uscities_sf <- st_as_sf(uscities, coords = c("lng", "lat"), crs = 4326))

aoi_get(state = "CO", county = "Larimer")

cities_larimer<-uscities_sf %>%
  filter(county_name=="Larimer")

larimer_counties<-ggplot(cities_larimer)+
  geom_sf()
larimer_counties
