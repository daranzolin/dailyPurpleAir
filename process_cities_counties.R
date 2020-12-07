library(sf)
library(tigris)
library(bayareamapping) # remotes::install_github("ir-sfsu/bayareamapping)
library(dplyr)

data("cities")
data("water")

# subset counties
counties <- counties(state = "CA")
counties <- st_as_sf(counties) %>% 
  filter(NAME %in% c("Alameda", 
                     "Contra Costa",
                     "Santa Clara",
                     "San Mateo",
                     "San Francisco",
                     "Santa Cruz",
                     "Napa",
                     "Sonoma",
                     "Marin",
                     "Solano",
                     "Monterey",
                     "Sacramento")) %>% 
  st_transform(crs = 3310)

# subset largest bodies of water
water_areas <- st_area(water) 
bay_and_delta_inds <- tail(order(water_areas), 3)
bay_and_delta <- water %>% 
  slice(bay_and_delta_inds) %>% 
  st_transform(crs = 3310)

# calc city centroids
city_centroids <- st_centroid(cities) %>% 
  st_transform(crs = 3310)

# write out
st_write(counties, "data/counties.shp")
st_write(city_centroids, "data/cities.shp")
st_write(bay_and_delta, "data/bay_and_delta.shp")
