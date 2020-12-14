library(sf)
library(tigris)
library(bayareamapping) # remotes::install_github("ir-sfsu/bayareamapping)
library(dplyr)
library(geojsonio)

data("cities")
data("water")

# subset counties
counties <- counties(state = "CA") %>% 
  st_as_sf() %>% 
  st_transform(crs = 3310) %>% 
  filter(NAME %in% c("Alameda", 
                     "Contra Costa",
                     "Santa Clara",
                     "San Mateo",
                     "San Francisco",
                     "Santa Cruz",
                     "Napa",
                     "Sonoma",
                     "Solano",
                     "Marin")
  ) %>% 
  transmute(
    NAME,
    AREA = as.numeric(st_area(.) / 1000000)
  ) %>% 
  mutate(SAMPLE = floor(sqrt(AREA)))

# ba_counties_dissolved <- counties %>%
#   st_transform(crs = 4326) %>% 
#   st_transform(crs = 3310) %>% 
#   # st_buffer(10000) %>% #10km
#   summarize() 

# subset largest bodies of water
# water_areas <- st_area(water) 
# bay_and_delta_inds <- tail(order(water_areas), 3)
# bay_and_delta <- water %>% slice(bay_and_delta_inds) 

# calc city centroids
city_centroids <- st_centroid(cities) %>% st_transform(crs = 3310)

# write out
st_write(counties, "data/counties.shp")
geojson_write(city_centroids, file = "data/cities.geojson")
# st_write(ba_counties_dissolved_and_buffered_10km, "data/ba_counties_dissolved_and_buffered_10km.shp")
# st_write(bay_and_delta, "data/bay_and_delta.shp")
