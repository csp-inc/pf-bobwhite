## ---------------------------
##
## Script name: geodiv-parallel-calculations.R
##
## Purpose of script: This script is used to generate a raster image describing the 'surface roughness' of continuous proportional land cover as an input for the bobwhite species distribution model. It first takes a vector file describing the spatial extent of the model, divides it into 50 roughly equal sized polygons, buffers each polygon by 5km (to reduce potential edge effects from the subsequent moving window calculations), and then, using the purrr package, maps a function to crop and mask the raster to each polygon. Then another function is mapped over the list to calculate surface roughness (or any other gradient metric available in the geodiv R package) for each raster chunk.Finally, the rasters are mosaicked back together using the 'min' option to mitigate potential edge effects that may result in Inf values. Importantly, the gradient metrics in this script are meant to be calculated in parallel. The script was developed to run on a Google VM with 64GB of RAM -- this is a very memory intensive operation and takes about an hour and half to two hours to complete.   
##
## Author: Patrick T. Freeman
##
## Date Created: 2023-03-17
## Date last updated: 2023-03-17
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  
library(terra)
library(geodiv)
library(sf)
library(dismo)
library(smoothr)
library(tidyverse)
library(purrr)

### Source the split polygons function
source("C:/Users/patrick/Documents/GitHub/pf-bobwhite/code/utils/split-polygons-function.R")

# Load the model states and process to remove small polygons associated with outlying islands 

### Load model states polygon and union it  
model_states <- st_read("C:/Users/patrick/Documents/bobwhite-data/bobwhite_model_states.gpkg") %>% 
  st_transform(., crs=5070) %>% 
  st_union()

### Remove small outlying islands that are unlikely to be of interest. 
area_thresh <- units::set_units(5000, km^2)
model_states_noislands <- drop_crumbs(model_states, threshold = area_thresh) 

### Split the polygons into a set number of polygons
#p <- split_poly(model_states_noislands, 50) 

#### Load the standard split: 
p <- read_rds("C:/Users/patrick/Documents/bobwhite-data/RAP_polygon_50chunks.rds")

# Load raster and polygon data
rap_raster <- rast("C:/Users/patrick/Documents/bobwhite-data/RAP-1621-mean-unsmoothed270m.tif")

states_vect <- vect(model_states_noislands)

rap_raster_masked <- crop(mask(rap_raster, states_vect), states_vect)

### REmove rap_raster to free memory
rm(rap_raster)

### Fix a band name for shrub cover
names(rap_raster_masked)[2] <- "RAP_SHR_1621_mean"


### Get desired layer and convert back to raster 
r <- rap_raster_masked$RAP_BGR_1621_mean

#### Aggregate R to coarser resolution to make sure this is working as intended
#ra <- aggregate(r, fact=20, fun=max)

# Create a function to crop and mask the raster to each polygon
crop_and_mask <- function(poly){
  ### Buffer the polygon by 5 km
  poly_buff <- st_buffer(poly, 5000)
  ### Crop and mask the raster to the buffered polygon
  cropped_raster <- terra::crop(r, vect(poly_buff))
  masked_raster <- terra::mask(cropped_raster, vect(poly_buff))
  return(masked_raster)
}

# Use the map function to apply the function to each polygon in the sf object
masked_rasters <- map(p$geometry, crop_and_mask)

# View the masked rasters to check names are appropriate
masked_rasters[[1]]

### Set up for gradient analysis  
gradient_analysis <- function(r){
  raster_name <- names(r)
  output_raster <- texture_image(raster(r), 
  size=5000,
  window="circle",
  in_meters=T,
  metric='sa',
  parallel=T)
return(output_raster)
}

test_run <- masked_rasters
test_check <- sprc(test_run)
test_m <- mosaic(test_check, fun="min")
plot(test_m)
rm(test_run, test_check, test_m)


### Perform the gradient analysis 
gradient_rasters <- masked_rasters %>%
  map(safely(gradient_analysis))

### Write to RDS if necessary 
write_rds(gradient_rasters, "C:/Users/patrick/Documents/bobwhite-data/RAP_BGR_Sa_Chunks_230508.rds")

sa_calcs_rds <- gradient_rasters %>% transpose()

is_ok <- sa_calcs_rds$error %>% map_lgl(is_null)
sa_calcs_rds_ok <- sa_calcs_rds[is_ok]
foo <- sa_calcs_rds %>% simplify_all()

### Get just the first list containing the results 
results <- foo[[1]]

convert_to_rast <- function(r){
  name <- names(r)
  convert <- rast(r)
  names(r) <- name
  return(convert)
}
test_check <- results %>% 
  map(convert_to_rast, .progress=T)
test_rc <- sprc(test_check)
test_m <- mosaic(test_rc, fun="min")
plot(test_m)

m_clamped <- clamp(test_m, 0, 100)

writeRaster(m_clamped, "C:/Users/patrick/Documents/bobwhite-data/RAP_BGR_Sa_mosaic270m.tif", overwrite=T)

