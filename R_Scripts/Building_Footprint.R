library(arrow)
install_arrow()
library(sf)
library(dplyr)
library(tigris)
library(rdeck) # pak::pak("rdeck")
library(geoarrow)
options(tigris_use_cache = TRUE)

install.packages("pmtiles", repos = c('https://walkerke.r-universe.dev', 'https://cloud.r-project.org'))

buildings <- open_dataset("s3://overturemaps-us-west-2/release/2025-11-19.0/theme=buildings")

library(pmtiles)

#nrow(buildings)


# -------------------------------------------------------------------------

ES_Counties <- c(
  "Cecil",
  "Kent",
  "Queen Anne's",
  'Talbot',
  "Caroline",
  "Dorchester",
  "Wicomico",
  "Somerset",
  "Worccester"
)

ES_Test_County <- c("Caroline")




sf_bbox <- counties(state = "MD", cb = TRUE, resolution = "20m") |> 
  filter(NAME %in% ES_Test_County) |> 
  st_bbox() |> 
  as.vector()


sf_buildings <- buildings |>
  filter(bbox$xmin > sf_bbox[1],
         bbox$ymin > sf_bbox[2],
         bbox$xmax < sf_bbox[3],
         bbox$ymax < sf_bbox[4]) |>
  select(id, geometry, height) |> 
  collect() |>
  st_as_sf(crs = 4326) |> 
  mutate(height = ifelse(is.na(height), 8, height))




sf_bbox <- counties(state = "MD", cb = TRUE, resolution = "20m") |>
  filter(NAME %in% ES_Test_County) |>
  st_bbox()

sf_buildings <- buildings |>
  filter(
    bbox$xmin >= sf_bbox["xmin"],
    bbox$ymin >= sf_bbox["ymin"],
    bbox$xmax <= sf_bbox["xmax"],
    bbox$ymax <= sf_bbox["ymax"]
  ) |>
  select(id, geometry, height) |>
  collect() |>
  st_as_sf(crs = 4326) |>
  mutate(height = ifelse(is.na(height), 8, height))



# -------------------------------------------------------------------------


rdeck(map_style = mapbox_light(), 
      initial_view_state = view_state(
        center = c(38.890307, -75.881584),
        zoom = 11.3,
        bearing = -60,
        pitch = 76
      )) |> 
  add_polygon_layer(
    data = sf_buildings, 
    name = "Caroline County",
    get_polygon = geometry, 
    get_elevation = height, 
    get_fill_color = scale_color_linear(
      col = height,
      palette = viridisLite::inferno(100, direction = -1)
    ),
    extruded = TRUE, 
    opacity = 0.5)

