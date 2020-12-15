library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(jsonlite)
library(sf)
library(lawn)
library(geojsonio)
library(stars)
library(httr)
library(con2aqi)

counties <- st_read("data/counties.shp")
counties_dissolved <- summarize(counties) %>% st_transform(crs = 3310)
# bay_and_delta <- st_read("data/bay_and_delta.shp")

url <- "https://api.purpleair.com/v1/sensors"
url <- modify_url(
  url, 
  query = list(
    location_type = 0, 
    fields = "latitude,longitude,location_type,pm2.5"
    )
  )
r <- GET(url, add_headers(`X-API-Key` = "394DB3CB-3DDA-11EB-9893-42010A8001E8"))
# r$status_code
pajson <- fromJSON(content(r, "text")) 
pa_data <- set_names(as_tibble(pajson$data), pajson$fields) %>% 
  drop_na() %>% 
  select(latitude, longitude, pm2.5)

pasf <- pa_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 3310) %>% 
  st_crop(., counties_dissolved) %>% 
  st_join(counties, join = st_intersects, left = TRUE) %>% 
  add_count(NAME, name = "COUNTY_SAMPLES")

sample_info <- pasf %>% 
  st_drop_geometry() %>% 
  distinct(NAME, SAMPLE, COUNTY_SAMPLES) %>% 
  drop_na()

county_samples <- list()
for (i in 1:nrow(sample_info)) {
  county <- sample_info$NAME[i]
  sample_size <- sample_info$SAMPLE[i]
  county_samples[[i]] <- pasf %>% 
    filter(NAME == county) %>% 
    sample_n(sample_size) %>% 
    select(pm2.5) %>% 
    st_transform(crs = 4326)
}

sf_to_TIN_to_sf <- function(x) {
  ptsgeo <- geojson_json(x)
  ptslwn <- as.feature(unclass(ptsgeo))
  tin <- lawn_tin(ptslwn)
  tin_avg <- lawn_average(tin, ptslwn, in_field = "pm2.5")
  tin_out <- geojson_sf(structure(tin_avg, class = "geo_list")) %>% 
    st_make_valid() %>% 
    st_buffer(0)
  return(tin_out)
}

county_TINs <- county_samples %>% 
  map_dfr(sf_to_TIN_to_sf) %>% 
  mutate(AQI = con2aqi("pm25", average)) %>% 
  st_transform(crs = 3310)

centroids <- st_centroid(county_TINs)
geojson_write(centroids, file = "data/TIN_centroids.geojson", overwrite = TRUE)
# geojson_write(county_TINs, file = "data/county_TINs.geojson", overwrite = TRUE)
# bay_and_delta <- st_transform(bay_and_delta, crs = 3310)
# r <- st_as_stars(county_TINs)
# crp <- st_crop(r, bay_and_delta, crop = FALSE)
# r_masked <- r * ifelse(is.na(crp[[1]]), 1, NA)
# sf_poly <- st_as_sf(r_masked, merge = TRUE)
# centroids_out <- st_centroid(sf_poly)
# st_write(centroids_out, "data/centroids_out.shp", overwrite = TRUE)
# valid_TINs <- county_TINs %>%
#   filter(AQI < 20) 
# cupertino <- cities %>%
#   filter(city == "CUPERTINO") %>%
#   st_transform(crs = 3310)
# # 
# st_distance(cupertino, county_TINs, by_element = TRUE) -> distances
# closest_ind <- which.min(distances)
# valid_centroids %>% 
#   slice(closest_ind) %>% 
#   bind_rows(cupertino) %>% 
#   mapview()
