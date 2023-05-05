## ---------------------------
##
## Script name: mtbs-rasterization.R
##
## Purpose of script: Rasterize MTBS burn perimeters for the derivation of of a burned covariate 
##
## Author: Patrick T. Freeman
##
## Date Created: 2023-05-02
## Date last updated: 2023-05-02
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  


library(sf)
library(smoothr)
library(raster)
library(terra)
library(tidyterra)
library(fasterize)
library(lubridate)
library(tidyverse)
library(purrr)

#### Load the 5km sampling grid used for bobwhite model 
grid_5km <- read_sf("/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/pf_5km_sampling_grid.gpkg")


### Load MTBS burn perimeter data within the range states we're using for the model 
mtbs <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data/00_covariates/mtbs_range_states.gpkg") %>%
  st_transform(., crs=5070)

### Get the ignition date as year and also create a counter column for binarization in raster creation 
mtbs_date <- mtbs %>%
  mutate(year = year(Ig_Date)) %>%
  dplyr::filter(year >= 2006 & year <= 2021) %>%
  dplyr::mutate(counter = 1)


# Intersect the two, which cuts all the polygons by each grid boundary:
mtbs.int <- st_intersection(mtbs_date, grid_5km)

### Aggregate
mtbs_count <- aggregate(Event_ID ~ grid_id_5km, data = mtbs.int, FUN = length) %>%
  dplyr::rename(burn.count.0621 = Event_ID)

### Join back to full grid 
grid_burncount <- full_join(grid_5km, mtbs_count, by="grid_id_5km") %>%
  replace_na(list(burn.count.0621 = 0))

r <- raster(grid_5km, res = 5000)

### Rasterize the polygons using fasterize -- no 'mean' function available so chose max
r_burncount <- rast(fasterize(grid_burncount, r, field = "burn.count.0621", fun="max"))
names(r_burncount) <- "burncount_0621"

ggplot() + geom_spatraster(data=r_burncount, aes(fill=burncount_0621)) + scale_fill_hypso_c() + coord_sf() + theme_minimal() + labs(title="Burn count 2006-2021 at 5km resolution", fill="Number of burn perimeters")




### Load model states polygon and union it  
model_states <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/bobwhite_model_states.gpkg") %>% 
  st_transform(., crs=5070) %>% 
  st_union()

### Remove small outlying islands that are unlikely to be of interest. 
area_thresh <- units::set_units(5000, km^2)
p_dropped <- drop_crumbs(model_states, threshold = area_thresh) %>%
  st_cast(., "POLYGON") %>%
  as_Spatial()


### Create template raster with same extent as the states at 270m resolution 
r <- raster(p_dropped, res = 270)

#### Now split the mtbs dataset into individual years 
mtbs_split <- split(mtbs_date, f=as.factor(mtbs_date$year))

### Write a function to rasterize each year's data 
rasterize_burn_perimeters <- function(poly) {
  p <- poly
  year <- unique(p$year)
  rast <- rast(fasterize(poly, r, field="counter", fun="max"))
  names(rast) <- year
  # Return a list with both vect and surf_rough objects
  return(rast)
}

### Map function over list with purrr
out_rasters <- mtbs_split %>%
  map(rasterize_burn_perimeters)

### Limit to last 15 years (2007-2021)
out_rasters_1721 <- out_rasters[2:16]

### Stack rasters
stack <- terra::rast(out_rasters_1721)

### Sum rasters to get burn frequency over the last 15 years for each pixel
burn_freq <- sum(stack, na.rm=T) 

### Mask the raster again
p_dropped_mask <- drop_crumbs(model_states, threshold = area_thresh) %>%
  st_cast(., "MULTIPOLYGON")

burn_freq_masked <- terra::mask(burn_freq, vect(p_dropped_mask))
burn_freq_masked <- terra::mask(ifel(is.na(burn_freq_masked), 0, burn_freq_masked), vect(p_dropped_mask))

writeRaster(burn_freq_masked, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/MTBS_burnfrequency_0721_270m.tif")

