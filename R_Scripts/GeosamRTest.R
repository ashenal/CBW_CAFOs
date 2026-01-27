# remotes::install_github("walkerke/geosam")

library(geosam)
library(sf)
library(dplyr)
library(purrr)

Sys.getenv("HF_TOKEN")
Sys.setenv(MAPBOX_PUBLIC_TOKEN = "pk.eyJ1IjoiYXNoZW5hbCIsImEiOiJjbWptem5kY2Ixd2kyM2ZvbGg4cWd0YzZjIn0.N2liDshRbc11XmUKNWD3yA")

geosam_install()

geosam_status()

# Interactive Geosam platform
result <- sam_explore(source = "mapbox")


# -------------------------------------------------------------------------

county <- st_read("./data/Uzma_Data/Counties/Caroline_County_Detailed/Caroline_County_Detailed.shp") %>% st_transform(., 3857)

Cafos <- sam_detect(
  bbox = CarolineCounty,
  text = "grouped silver warehouses",
  source = "mapbox",
  zoom = 13,
  chunked = TRUE
)

plot(Cafos)

sam_view(Cafos)



# Chunked from chatgpt ----------------------------------------------------

grid <- st_make_grid(
  county,
  cellsize = 2000,   # meters
  square = TRUE
) |>
  st_sf() |>
  st_intersection(county)

cafos <- map_dfr(
  seq_len(nrow(grid)),
  purrr::possibly(
    function(i) {
      
      res <- sam_detect(
        bbox = grid[i, ],
        text = "long narrow farm buildings with light colored metal roofs"
        source = "mapbox",
        zoom = 15,
        
        chunked = FALSE
      )
      
      if (is.null(res)) return(NULL)
      
      res |> mutate(chunk_id = i)
    },
    otherwise = NULL
  )
)

class(cafos)
nrow(cafos)



# -------------------------------------------------------------------------

library(purrr)
library(dplyr)
library(sf)

county <- st_read("./data/Uzma_Data/Counties/Caroline_County_Detailed/Caroline_County_Detailed.shp") %>% st_transform(., 3857)

grid <- st_make_grid(
  county,
  cellsize = 2000,   # meters
  square = TRUE
) |>
  st_sf() |>
  st_intersection(county)

results_list <- map(
  seq_len(min(20, nrow(grid))),
  purrr::possibly(
    function(i) {
      
      res <- sam_detect(
        bbox = grid[i, ],
        text = "grouped silver warehouses",
        source = "mapbox",
        zoom = 15,
        chunked = FALSE
      )
      
      if (is.null(res) || nrow(res) == 0) return(NULL)
      
      mutate(res, chunk_id = i)
    },
    otherwise = NULL
  )
)

# Drop NULLs
results_list <- purrr::compact(results_list)

# Bind sf objects safely
cafos <- do.call(rbind, results_list)


# -------------------------------------------------------------------------

library(geosam)
library(dplyr)

county <- st_read("./data/Uzma_Data/Counties/Caroline_County_Detailed/Caroline_County_Detailed.shp") %>% st_transform(., 3857)

grid <- st_make_grid(
  county,
  cellsize = 2000,   # meters
  square = TRUE
) |>
  st_sf() |>
  st_intersection(county)

dir.create("./results1.14", showWarnings = FALSE)

for (i in seq_len(nrow(grid))) {   # loop over ALL grid cells
  message("Processing chunk: ", i, " / ", nrow(grid))
  
  res <- tryCatch({
    sam_detect(
      bbox   = grid[i, ],
      text   = "long narrow farm buildings with light colored metal roofs",
      source  = "mapbox",
      zoom    = 15,
      threshold = 0.4
    )
  }, error = function(e) { 
    message(" -> error on chunk ", i, " : ", e$message)
    return(NULL)
  })
  
  message("res class: ", paste(class(res), collapse = ", "))
  message("res length: ", length(res))
  
  if (!is.null(res) && length(res) > 0) {
    res_sf <- tryCatch(sam_as_sf(res), error = function(e) NULL)
    if (!is.null(res_sf) && nrow(res_sf) > 0) {
      res_sf <- res_sf %>% mutate(chunk_id = i)
      saveRDS(res_sf, file = sprintf("./results1.14/geosam_chunk_%03d.rds", i))
      message(" -> saved chunk ", i)
    } else {
      message(" -> chunk ", i, " has no sf polygons")
    }
  } else {
    message(" -> chunk ", i, " returned NULL or empty")
  }
}



# list all saved chunks (first 20)
files <- list.files("./results", pattern = "geosam_chunk_.*\\.rds$", full.names = TRUE)

cafos <- map_dfr(files, readRDS)
cafos

st_write(cafos, "./data/Geosam_Results/12-31_CarolineCounty1/Results.shp")

library(ggplot2)
ggplot() +
  geom_sf(data = county, fill = "white", color = "black") +
  geom_sf(data = cafos, aes(fill = factor(chunk_id)), alpha = 0.5, color = "red") +
  theme_minimal()




# TEST 1.14 ---------------------------------------------------------------


for (i in seq_len(nrow(grid))) {
  message("Processing chunk: ", i, " / ", nrow(grid))
  
  res <- tryCatch({
    sam_detect(
      bbox      = grid[i, ],
      text      = "long narrow farm buildings with light colored metal roofs",
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
        file = sprintf("./Results_GeoSAM/1.3_1.26.26/geosam_chunk_%03d.rds", i)
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
  "././Results_GeoSAM/1.3_1.26.26",
  pattern = "geosam_chunk_.*\\.rds$",
  full.names = TRUE
)

cafos_raw <- map_dfr(files, readRDS)

library(mapview)
library(sf)

mapview(cafos_raw, col.regions = "red")

files <- list.files(
  "././Results_GeoSAM/1.3_1.26.26",
  pattern = "geosam_chunk_.*\\.rds$",
  full.names = TRUE
)

cafos_raw <- do.call(rbind, lapply(files, readRDS))
st_write(
  cafos_raw,
  "././Results_GeoSAM/1.3_1.26.26/Caroline_County_Candidates.gpkg"
)
