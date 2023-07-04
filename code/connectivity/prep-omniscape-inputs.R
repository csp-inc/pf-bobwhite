library(sf)
library(terra)
library(tidyverse)

# ---------- COLLECT INPUTS ---------------
# NOBO study area
aoi <- st_read("connectivity-data/bobwhite-grid-union.gpkg") %>% 
  st_buffer(-10000)
# Habitat suitability surface
hab <- rast("connectivity-data/bobwhite-sdm-final-mlra-5k.tiff") %>% 
  clamp(upper = 495) %>% 
  terra::mask(aoi)
# Get MLRA regions
mlra <- st_read("connectivity-data/MLRA_52_2022/MLRA_52.shp") %>% 
  st_transform(st_crs(aoi)) %>% 
  st_intersection(aoi) %>% 
  mutate(Area_km2 = as.numeric(st_area(.))/1e6)


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
resScaleRegion <- function(h, c, region = mlra){
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

# Sample random points based on hab suitability value by region
sourceRegion <- function(r, region = mlra) {
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
out_dir = "omni-ne8" # ADJUST BASED ON MODEL TYPE

# Make output folder
if(dir.exists(paste0("connectivity-data/omniscape-inputs/", out_dir)) == FALSE){
  dir.create(paste0("connectivity-data/omniscape-inputs/", out_dir))
}

# Random points as sources
ss_name = "source" # ** ADJUST BASED ON SETTINGS IN CALL TO get_points()
# ss <- get_points(r = hab, n = 500, cut = 0.01, reassign = 0.01, mode = 'pairwise')
ss <- sourceRegion(r = hab)
ss_rast_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", ss_name, ".tif")
terra::writeRaster(ss[[1]], filename = ss_rast_path, overwrite = TRUE)


# Resistance from habitat suitability
res <- resScaleRegion(h = hab, c = 8)
res_name = "resistance-ne8.tif" # ** ADJUST BASED ON SETTINGS IN CALL TO get_points()
res_path = paste0("connectivity-data/omniscape-inputs/", out_dir, "/", res_name)
terra::writeRaster(res, filename = res_path, overwrite = TRUE)
