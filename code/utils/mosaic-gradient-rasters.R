library(terra)
library(tidyverse)

sa_calcs_rds <- readRDS("C:/Users/patrick/Documents/bobwhite-data/RAP_2019Tree_Sa_Chunks_230420.rds")

sa_calcs_rds <- sa_calcs_rds %>% transpose()
str(sa_calcs_rds)

is_ok <- sa_calcs_rds$error %>% map_lgl(is_null)
sa_calcs_rds_ok <- sa_calcs_rds[is_ok]

crop_and_mask <- function(poly) {
  cropped_raster <- terra::crop(r, vect(poly))
  masked_raster <- terra::mask(cropped_raster, vect(poly))
  return(masked_raster)
}

test <- rast(sa_calcs_rds_ok$result[[50]])

polygonize_and_mask <- function(raster){
  rast_orig <- terra::rast(raster)
  rast <- terra::rast(raster)
  values(rast)[!is.na(values(rast))] <- 1
  pr <- as.polygons(rast)
  pr_buffer <- buffer(pr, -5000)
  cropped_rast <- terra::crop(rast, pr_buffer)
  masked_rast <- terra::mask(rast_orig, pr_buffer)
  return(masked_rast)
}

### Get the results and the errors compressed into two lists 
foo <- sa_calcs_rds %>% simplify_all()

### Get just the first list containing the results 
results <- foo[[1]]

results_trimmed <- results %>%
  map(polygonize_and_mask)

### Convert to raster collection
rc <- sprc(results_trimmed)
m <- mosaic(rc, fun="min")

m_clammped <- clamp(m, 0, 100)

writeRaster(m_clammped, "C:/Users/patrick/Documents/bobwhite-data/RAP_2019Tree_Sa_mosaic.tif", overwrite=T)

