library(terra)
library(tidyverse)

sa_calcs_rds <- readRDS("C:/Users/patrick/Documents/bobwhite-data/RAP_SHR_Sa_Chunks_230504.rds")

sa_calcs_rds <- sa_calcs_rds %>% transpose()

is_ok <- sa_calcs_rds$error %>% map_lgl(is_null)
sa_calcs_rds_ok <- sa_calcs_rds[is_ok]

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

convert_to_rast <- function(r){
  name <- names(r)
  convert <- rast(r)
  names(r) <- name
  return(convert)
}

### Convert to raster collection
converted <- results %>%
  map(convert_to_rast)

rc <- sprc(converted)
m <- mosaic(rc, fun="mean")

m_clamped <- clamp(m, 0, 100)

writeRaster(m_clamped, "C:/Users/patrick/Documents/bobwhite-data/RAP_Tree_Sa_mosaic270m.tif", overwrite=T)

