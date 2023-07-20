## ---------------------------
##
## Script name: snow-depth-days-covariate-construction.R
##
## Purpose of script:Generating a rasterized surface of # of snow days with snow depth >2.5 cm based on SNODAS data layers  
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
library(tidyverse)
library(sf)
library(smoothr)

nc_list <- list.files("/Users/patrickfreeman-csp/Desktop/SNODAS_DATA_DIR/nc_2020", pattern="*.nc", full.names = T)

#import all raster files in folder using lapply
allrasters <- lapply(nc_list, rast)

# Create reclassification raster such that each raster is reclassified to 0 if the depth is <250mm (2.5 cm) and 1 if 250 or greater 
m <- c(0, 249, 0,  
       250, 100000, 1)
m <- matrix(m, ncol=3, byrow = TRUE)

### Function for reclassifying raster
reclass_function <- function(x){
  terra::ifel(x>249, 1, 0)
}

### Reclassify all rasters in list 
reclassed <- lapply(allrasters, reclass_function)

### Bind all rasters together as multi-layer 
reclassed_stack <- rast(reclassed)

### Sum rasters to get number of days with snow depth >2.5cm 
t <- sum(reclassed_stack)

### Write to file 
writeRaster(t, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/snodas_days_snowdepth_2020_WGS84.tif")
# 
# rast_to_plot <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/snodas_days_snowdepth_2017_WGS84.tif")
# 
# ggplot() +
#   geom_spatraster(data = rast_to_plot, aes(fill = sum)) +
#   # You can use coord_sf
#   coord_sf(crs = 5070) +
#   scale_fill_hypso_c() + 
#   labs(title="SNODAS: Days with snow depth >2.5 cm in 2017")


### Calculating the mean number of days with snow >2.5cm across 2016-2021
sno_files <- list.files("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/SNODAS", pattern="*.tif", full.names=T)
#import all raster files in folder using lapply
allrasters <- lapply(sno_files, rast)

### Just rasters from 2018-2021
allrasters_1821 <- allrasters[3:6]

### Stack rasters from 2018-2021/all years 
allrasters_stacked <- rast(allrasters)
allrasters_1821_stacked <- rast(allrasters_1821)

### Calculate mean across raster stack
allrasters_mean <- mean(allrasters_stacked, na.rm=T)
allrasters_1821_mean <- mean(allrasters_1821_stacked, na.rm=T)


### Prep the mask 

### Load model states polygon and union it  
model_states <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/bobwhite_model_states.gpkg") %>% 
  st_transform(., crs=5070) %>% 
  st_union()

### Remove small outlying islands that are unlikely to be of interest. 
area_thresh <- units::set_units(5000, km^2)
p_dropped <- drop_crumbs(model_states, threshold = area_thresh) %>%
  st_cast(., "POLYGON") %>%
  st_transform(., crs=st_crs(allrasters_1821_mean))

allrasters_mean_masked <- terra::mask(allrasters_mean, vect(p_dropped))
allrasters_1821_mean_masked <- terra::mask(allrasters_1821_mean, vect(p_dropped))


allrasters_mean_cropped <- terra::crop(allrasters_mean_masked, vect(p_dropped))
allrasters_1821_mean_cropped <- terra::crop(allrasters_1821_mean_masked, vect(p_dropped))


writeRaster(allrasters_mean_cropped, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/SNODAS/snodas-snowdays-mean1621-wgs84.tif")

writeRaster(allrasters_1821_mean_cropped, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/SNODAS/snodas-snowdays-mean1821-wgs84.tif")
                                      