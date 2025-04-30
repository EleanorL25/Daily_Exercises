library(here)
library(sf)
library(AOI)
library(dplyr)
library(ggplot2)
library(ggrepel)
uscities<-readr::read_csv(here("exercise-24-csu/simplemaps_uscities_basicv1.90/uscities.csv"))
(uscities_sf <- st_as_sf(uscities, coords = c("lng", "lat"), crs = 4326))

Larimer_boundary<-aoi_get(state = "CO", county = "Larimer")

cities_larimer<-uscities_sf %>%
  filter(county_name=="Larimer")

larimer_counties<-ggplot()+
  geom_sf(data=Larimer_boundary, aes(fill = land_area/1e10))+
  geom_sf(data=cities_larimer,aes(size = population/1e5))+
  theme_linedraw() +
  labs(title = "Larimer County: Cities Included",
       size = "Population \n(100,000)",
       fill = "Acres \n(billions)")
larimer_counties


# Tag top 3 most populous cities
cities_larimer <- cities_larimer %>%
  arrange(desc(population)) %>%
  mutate(top_city = row_number() <= 3)


Larimer_plot_big <- ggplot() +
  geom_sf(data = Larimer_boundary, aes(fill = land_area / 1e10)) +
  geom_sf(data = cities_larimer, aes(size = population / 1e5, color = top_city)) +
  theme_linedraw() +
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "black"),
    labels = c("FALSE" = "Other Cities", "TRUE" = "Top 3 Cities"),
    name = "City Rank"
  ) +
  labs(
    title = "Larimer County: Cities Included",
    size = "Population \n(100,000)",
    fill = "Acres \n(billions)"
  )

Larimer_plot_big

three_largest_cities <- cities_larimer %>%
  filter(top_city == TRUE)

Larimer_plot_big <- ggplot() +
  geom_sf(data = Larimer_boundary, aes(fill = land_area / 1e10)) +
  geom_sf(data = cities_larimer, aes(size = population / 1e5, color = top_city)) +
  geom_label_repel(
    data = three_largest_cities,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    color = "black",
    box.padding = 0.5
  ) +
  theme_linedraw() +
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "black"),
    labels = c("FALSE" = "Other Cities", "TRUE" = "Top 3 Cities"),
    name = "City Rank"
  ) +
  labs(
    title = "Larimer County: Cities Included",
    size = "Population \n(100,000)",
    fill = "Acres \n(billions)"
  )

Larimer_plot_big
