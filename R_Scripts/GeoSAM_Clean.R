library(geosam)
library(sf)
library(dplyr)
library(purrr)


Sys.getenv("HF_TOKEN")
Sys.setenv(MAPBOX_PUBLIC_TOKEN = "pk.eyJ1IjoiYXNoZW5hbCIsImEiOiJjbWptem5kY2Ixd2kyM2ZvbGg4cWd0YzZjIn0.N2liDshRbc11XmUKNWD3yA")

#geosam_install()

geosam_status()

# Interactive Geosam platform:
#result <- sam_explore(source = "mapbox")


# -------------------------------------------------------------------------

county <- st_read("./data/Uzma_Data/Counties/Caroline_County_Detailed/Caroline_County_Detailed.shp") %>% st_transform(., 3857)

grid <- st_make_grid(
  county,
  cellsize = 2000,   # meters
  square = TRUE
) |>
  st_sf() |>
  st_intersection(county)


# -------------------------------------------------------------------------

for (i in seq_len(nrow(grid))) {
  message("Processing chunk: ", i, " / ", nrow(grid))
  
  res <- tryCatch({
    sam_detect(
      bbox      = grid[i, ],
      text      = "ponds",
      source    = "mapbox",
      zoom      = 15,
      threshold = 0.3,
      chunked   = TRUE
    )
  }, error = function(e) {
    message(" -> error on chunk ", i, " : ", e$message)
    return(NULL)
  })
  
  if (!is.null(res) && length(res) > 0) {
    res_sf <- tryCatch(sam_as_sf(res), error = function(e) NULL)
    
    if (!is.null(res_sf) && nrow(res_sf) > 0) {
      res_sf <- mutate(res_sf, chunk_id = i)
      
      saveRDS(
        res_sf,
        file = sprintf("./Results_GeoSAM/2.1_1.26.26/geosam_chunk_%03d.rds", i)
      )
      
      message(" -> saved chunk ", i)
    } else {
      message(" -> chunk ", i, " has no sf polygons")
    }
  } else {
    message(" -> chunk ", i, " returned NULL or empty")
  }
}

files <- list.files(
  "././Results_GeoSAM/2.1_1.26.26",
  pattern = "geosam_chunk_.*\\.rds$",
  full.names = TRUE
)

cafos_raw <- map_dfr(files, readRDS)

  #library(mapview)
  #mapview(cafos_raw, col.regions = "red")

files <- list.files(
  "././Results_GeoSAM/2.1_1.26.26",
  pattern = "geosam_chunk_.*\\.rds$",
  full.names = TRUE)

cafos_raw <- do.call(rbind, lapply(files, readRDS))
st_write(
  cafos_raw,
  "././Results_GeoSAM/2.1_1.26.26/Caroline_County_Candidates.gpkg")

