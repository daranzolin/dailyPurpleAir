library(dplyr)
library(tidyr)
library(jsonlite)
library(sf)
library(lawn)
library(geojsonio)
library(stars)

counties <- st_read("data/counties.shp")
cities <- st_read("data/cities.shp")
bay_and_delta <- st_read("data/bay_and_delta.shp")

pa <- fromJSON("https://www.purpleair.com/json")

bb <- st_bbox(counties)

padf <- pa[["results"]] %>% 
  filter(
    AGE == 0, 
    DEVICE_LOCATIONTYPE == "outside"
    ) %>% 
  select(Lat, Lon, PM2_5Value) %>%
  drop_na() 

pasf <- padf %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = st_crs(bb)) %>% 
  st_crop(., bb) %>% 
  mutate(
    PM2_5Value = as.numeric(PM2_5Value),
    row_id = row_number()
    ) %>% 
  st_join(counties,
          join = st_intersects,
          left = TRUE)

max_sample_size <- count(pasf, NAME) %>% 
  pull(n) %>% 
  min()

pa_sampled <- pasf %>% 
  group_by(NAME) %>% 
  sample_n(max_sample_size) %>% 
  ungroup()
# pa_sampled <- pasf %>% 
#   add_count(NAME, name = "obs") %>% 
#   group_by(NAME) %>%
#   mutate(sample_size = ifelse(obs < 50, obs, 50)) %>% 
#   sample_n(sample_size, replace = FALSE) %>% 
#   ungroup()

ptsgeo <- geojson_json(pa_sampled)
ptslwn <- as.feature(unclass(ptsgeo))
tin <- lawn_tin(ptslwn)
tin_avg <- lawn_average(tin, ptslwn, in_field = "PM2_5Value")
tin_sf <- geojson_sf(structure(tin_avg, class = "geo_list")) %>% 
  st_transform(crs = 3310) %>% 
  st_make_valid() %>% 
  st_buffer(0)

bay_and_delta <- st_transform(bay_and_delta, crs = 3310)

crp <- st_crop(r, bay_and_delta, crop = FALSE)
r_masked <- r * ifelse(is.na(crp[[1]]), 1, NA)

sf_poly <- st_as_sf(r_masked, merge = TRUE)
centroids_out <- st_centroid(sf_poly)
st_write(centroids_out, "data/centroids_out.shp", overwrite = TRUE)
# valid_centroids <- sf_poly %>% 
#   filter(average < 8) %>% 
#   st_centroid() 
# 
# cupertino <- cities %>% 
#   filter(city == "CUPERTINO") %>% 
#   st_transform(crs = 3310)
# 
# st_distance(cupertino, valid_centroids, by_element = TRUE) -> distances
# closest_ind <- which.min(distances)
# valid_centroids %>% 
#   slice(closest_ind) %>% 
#   bind_rows(cupertino) %>% 
#   mapview()
