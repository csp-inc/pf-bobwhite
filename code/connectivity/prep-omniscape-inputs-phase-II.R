library(sf)
library(terra)
library(tidyverse)

# ---------- COLLECT INPUTS ---------------
# NOBO study area
aoi <- st_read("connectivity-data/bobwhite-grid-union.gpkg") %>% 
  st_buffer(-10000)
# Habitat suitability surface
hab <- rast("connectivity-data/output-data_bobwhite-habitat-suitability-final.tiff") %>% 
  clamp(upper = 502) %>%
  terra::mask(aoi)
log_hab <- log(hab+1)
# # Get ARS regions 
regions <- st_read("connectivity-data/ars_regions_5070.gpkg") %>% 
     st_transform(st_crs(aoi)) %>% 
     st_intersection(aoi) %>% 
     mutate(Area_km2 = as.numeric(st_area(.))/1e6)

# Inputs for conus and regional analysis
source_input <- log_hab
res_input <- hab

# ------------- FUNCTIONS --------------------
# Rescale between 0 and 1
rescale01 <- function(r){
  mm <- minmax(r)
  rout <- (r - mm[1])/(mm[2] - mm[1])
  return(rout)
}

# Define neg exponential resistance function (from Keeley et al. 2016 Landscape Ecol)
keeley_rescale <- function(x, c) 100-99*((1-exp(-c*x))/(1-exp(-c)))

# Rescale habitat to resistance
resScale <- function(h, c){
  h01 <- rescale01(h)
  # Apply neg exponential conversion
  out <- keeley_rescale(h01, c)
  return(out)
}

# Rescale habitat to resistance by region
resScaleRegion <- function(h, c, region = regions){
  # Loop through regions, clip raster to region, and apply 0-1 rescale by region
  rastList = list()
  for(i in 1:nrow(region)){
    r_temp <- terra::mask(h, vect(region[i,]))
    h01_temp <- rescale01(r_temp)
    rastList[[i]] <- h01_temp
  }
  # Merge regional rasters
  h01 <- rastList %>% terra::sprc() %>% terra::merge()
  
  # Apply neg exponential conversion
  out <- keeley_rescale(h01, c)
  return(out)
}

# Convert to source strength from hab suitability value by region
sourceRegion <- function(r, region = regions) {
  # Rescale raster to 0-1 by region
  rastList = list()
  for(i in 1:nrow(region)){
    r_temp <- terra::mask(r, vect(region[i,]))
    h01_temp <- rescale01(r_temp)
    rastList[[i]] <- h01_temp
  }
  # Merge regional rasters
  r <- rastList %>% terra::sprc() %>% terra::merge()
  
  return(r)
}

# ------------- PREP OUTPUTS --------------------
# Set output folder name
# Possible naming convention: {model type}-{resistance scaling}
out_dir = "omni-ne8-log-region-phase-II" # ADJUST BASED ON MODEL TYPE

# Make output folder
if(dir.exists(paste0("connectivity-data/omniscape-inputs/", out_dir)) == FALSE){
  dir.create(paste0("connectivity-data/omniscape-inputs/", out_dir))
}

# Create source layer by region

### Southeast 
ss_name = "southeast" # set name
ss <- sourceRegion(source_input) # apply regional source rescaling 
southeast_v <- vect(regions %>%
  filter(ARSregion == "southeast")) # filter to create vector of region
ss_southeast <- crop(mask(ss, southeast_v), southeast_v) # mask and crop the source strength raster to the region
ss_rast_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", "source-", ss_name, ".tif") ## set out path
terra::writeRaster(ss_southeast, filename = ss_rast_path, overwrite = TRUE) # write out regional source strength layer


# Resistance from habitat suitability
res <- resScaleRegion(h = res_input, c = 8, region = regions)
res_southeast <- crop(mask(res, southeast_v), southeast_v)
res_name = "resistance-ne8-southeast.tif" # ** ADJUST BASED ON MODEL STRUCTURE
res_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", res_name)
terra::writeRaster(res_southeast, filename = res_path, overwrite = TRUE)

### plains 
ss_name = "plains" # set name
ss <- sourceRegion(source_input) # apply regional source rescaling 
plains_v <- vect(regions %>%
                      filter(ARSregion == "plains")) # filter to create vector of region
ss_plains <- crop(mask(ss, plains_v), plains_v) # mask and crop the source strength raster to the region
ss_rast_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", "source-", ss_name, ".tif") ## set out path
terra::writeRaster(ss_plains, filename = ss_rast_path, overwrite = TRUE) # write out regional source strength layer


# Resistance from habitat suitability
res <- resScaleRegion(h = res_input, c = 8, region = regions)
res_plains <- crop(mask(res, plains_v), plains_v)
res_name = "resistance-ne8-plains.tif" # ** ADJUST BASED ON MODEL STRUCTURE
res_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", res_name)
terra::writeRaster(res_plains, filename = res_path, overwrite = TRUE)

### midwest
ss_name = "midwest" # set name
ss <- sourceRegion(source_input) # apply regional source rescaling 
midwest_v <- vect(regions %>%
                   filter(ARSregion == "midwest")) # filter to create vector of region
ss_midwest <- crop(mask(ss, midwest_v), midwest_v) # mask and crop the source strength raster to the region
ss_rast_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", "source-", ss_name, ".tif") ## set out path
terra::writeRaster(ss_midwest, filename = ss_rast_path, overwrite = TRUE) # write out regional source strength layer


# Resistance from habitat suitability
res <- resScaleRegion(h = res_input, c = 8, region = regions)
res_midwest <- crop(mask(res, midwest_v), midwest_v)
res_name = "resistance-ne8-midwest.tif" # ** ADJUST BASED ON MODEL STRUCTURE
res_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", res_name)
terra::writeRaster(res_midwest, filename = res_path, overwrite = TRUE)

### northeast
ss_name = "northeast" # set name
ss <- sourceRegion(source_input) # apply regional source rescaling 
northeast_v <- vect(regions %>%
                    filter(ARSregion == "northeast")) # filter to create vector of region
ss_northeast <- crop(mask(ss, northeast_v), northeast_v) # mask and crop the source strength raster to the region
ss_rast_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", "source-", ss_name, ".tif") ## set out path
terra::writeRaster(ss_northeast, filename = ss_rast_path, overwrite = TRUE) # write out regional source strength layer


# Resistance from habitat suitability
res <- resScaleRegion(h = res_input, c = 8, region = regions)
res_northeast <- crop(mask(res, northeast_v), northeast_v)
res_name = "resistance-ne8-northeast.tif" # ** ADJUST BASED ON MODEL STRUCTURE
res_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", res_name)
terra::writeRaster(res_northeast, filename = res_path, overwrite = TRUE)


### rangewide
ss_name = "rangewide" # set name
ss <- sourceRegion(source_input, region=aoi) # apply scaling using entire AOI of model 
ss_rast_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", "source-", ss_name, ".tif") ## set out path
terra::writeRaster(ss, filename = ss_rast_path, overwrite = TRUE) # write out regional source strength layer


# Resistance from habitat suitability
res <- resScaleRegion(h = res_input, c = 8, region = aoi)
res_name = "resistance-ne8-rangewide.tif" # ** ADJUST BASED ON MODEL STRUCTURE
res_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", res_name)
terra::writeRaster(res, filename = res_path, overwrite = TRUE)

