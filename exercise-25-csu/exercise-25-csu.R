library(here)
library(sf)
library(AOI)
library(tigris)
library(ggplot2)
library(dplyr)

majorrivers<-read_sf(here("exercise-25-csu/majorrivers_0_0/MajorRivers.shp"))
Mississippi_river <- subset(majorrivers, SYSTEM == "Mississippi")
print(Mississippi_river)

conus_counties <- AOI::aoi_get(state = "conus")
all_counties <- tigris::counties(cb = TRUE, year = 2023)
conus_counties <- st_transform(conus_counties, st_crs(all_counties))
counties <- sf::st_filter(all_counties, conus_counties)

Mississippi_river <- st_transform(Mississippi_river, st_crs(all_counties))
mississippi_counties <- st_filter(all_counties, Mississippi_river, .predicate = st_intersects)

Mississippi_river_plot<-ggplot()+
  geom_sf(data = mississippi_counties, fill = "lightblue", color = "blue", size = 0.2)+
  geom_sf(data = Mississippi_river, color = "darkblue", size = 0.6)+
  theme_linedraw() +
  labs(title = "Counties Intersecting the Mississippi River System",
       size = "land area",
       fill = "Miles")
Mississippi_river_plot


uscities<-readr::read_csv(here("exercise-24-csu/simplemaps_uscities_basicv1.90/uscities.csv"))
(uscities_sf <- st_as_sf(uscities, coords = c("lng", "lat"), crs = 4326))
mississippi_counties <- st_transform(mississippi_counties, st_crs(uscities_sf))
mississippi_cities <- st_join(uscities_sf, mississippi_counties, join = st_within)
str(mississippi_cities)
mississippi_cities <- mississippi_cities %>% filter(!is.na(NAME))
head(mississippi_cities)




library(here)
library(sf)
library(AOI)
library(tigris)
library(ggplot2)

# Read the shapefile for major rivers
majorrivers <- read_sf(here("exercise-25-csu/majorrivers_0_0/MajorRivers.shp"))

# Filter to only include the Mississippi River
Mississippi_river <- subset(majorrivers, SYSTEM == "Mississippi")
print(Mississippi_river)
# Get county boundaries for CONUS
conus_counties <- AOI::aoi_get(state = "conus")
# Read all counties
all_counties <- tigris::counties(cb = TRUE, year = 2023)

# Ensure CRS alignment with CONUS counties
conus_counties <- st_transform(conus_counties, st_crs(all_counties))

# Use st_filter to intersect the counties with the Mississippi River system
counties <- sf::st_filter(all_counties, conus_counties)

# Transform the Mississippi River to the same CRS as counties
Mississippi_river <- st_transform(Mississippi_river, st_crs(all_counties))

# Filter counties that intersect with the Mississippi River system
mississippi_counties <- st_filter(all_counties, Mississippi_river, .predicate = st_intersects)
# Create the plot
Mississippi_river_plot <- ggplot() +
  geom_sf(data = mississippi_counties, fill = "lightblue", color = "blue", size = 0.2) +
  geom_sf(data = Mississippi_river, color = "darkblue", size = 0.6) +
  theme_linedraw() +
  labs(title = "Counties Intersecting the Mississippi River System",
       size = "land area",
       fill = "Miles")

# Show the plot
Mississippi_river_plot

# Read in the city data
uscities <- readr::read_csv(here("exercise-24-csu/simplemaps_uscities_basicv1.90/uscities.csv"))

# Convert city data to sf object
uscities_sf <- st_as_sf(uscities, coords = c("lng", "lat"), crs = 4326)

# Transform city data CRS to match counties CRS
uscities_sf <- st_transform(uscities_sf, st_crs(mississippi_counties))
# Perform spatial join to get cities in Mississippi River counties
mississippi_cities <- st_join(uscities_sf, mississippi_counties, join = st_within)

# Merge with the counties data
mississippi_counties <- left_join(mississippi_counties, urban_population_by_county, by = "NAME")
# Create the plot with population data

# Drop geometry to avoid list column issues
mississippi_cities_nogeom <- st_drop_geometry(mississippi_cities)

# Check which entries have a matched county name (from the joined counties)
table(is.na(mississippi_cities_nogeom$NAME))  # This checks how many are NA

# Filter out cities that didn’t intersect any county
mississippi_cities <- mississippi_cities[!is.na(mississippi_cities$NAME), ]

# Calculate the total urban population per county
urban_population_by_county <- mississippi_cities %>%
  group_by(NAME) %>%
  summarise(total_population = sum(population)) 


Mississippi_river_plot2 <- ggplot() +
  geom_sf(data = mississippi_counties, aes(fill = ALAND), color = "blue", size = 0.2) +
  geom_sf(data = Mississippi_river, color = "darkblue", size = 0.3) +
  scale_fill_viridis_c(option = "C", na.value = "gray", name = "population") +
  theme_linedraw() +
  labs(title = "Counties Intersecting the Mississippi River System with Urban Population")

# Show the plot
Mississippi_river_plot2

