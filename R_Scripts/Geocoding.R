
library(tidyverse)
library(tidygeocoder)
library(sf)

# Load your data
cafo <- read_csv("./data/WMS_CAFO_Details_Ext.csv")

# Combine address fields into one string
cafo <- cafo %>%
  mutate(full_address = paste(LOCATION_ADDRESS, LOCATION_ADDRESS_CITY_STATE_ZIP, sep = ", "))

# Geocode using OpenStreetMap (free)
cafo_geocoded <- cafo %>%
  geocode(
    address = full_address,
    method = "arcgis",      # or "arcgis" if you prefer ESRI’s service
    lat = latitude,
    long = longitude
  )

cafo_clean <- cafo_geocoded %>%
  filter(!is.na(latitude) & !is.na(longitude))

cafo_sf <- st_as_sf(
  cafo_clean,
  coords = c("longitude", "latitude"),  # names must match your columns
  crs = 4326,  # WGS84 geographic coordinate system
  remove = FALSE  # keep original lon/lat columns
)

# Shorten column names before writing
names(cafo_sf) <- make.names(substr(names(cafo_sf), 1, 10), unique = TRUE)


st_write(cafo_sf, "./data/PA_Cafos_Geocoded.shp",
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

test <- st_read("./data/PA_Cafos_Geocoded.shp")

plot(st_geometry(cafo_sf))

st_write(cafo_sf, "./data/PA_Cafos_Geocoded.gpkg", delete_layer = TRUE)
