library(sf)
library(tidyverse)
library(lubridate)
library(fasterize)
library(raster)
library(terra)

dtrange <- st_read("/Volumes/GoogleDrive/My Drive/00_CSP_Projects/01_BLM-PVA/01_spatial-layer-development/08_basic_tort_shapefiles/DTrange/dtrange.shp") %>%
  st_transform(., crs=5070)



### Load MTBS burn perimeter data within the range states we're using for the model 
mtbs <- st_read("/Users/patrickfreeman-csp/Downloads/mtbs_perimeter_data/mtbs_perims_DD.shp") %>%
  st_transform(., crs=5070)

### Get the ignition date as year and also create a counter column for binarization in raster creation 
mtbs_date <- mtbs %>%
  mutate(year = year(Ig_Date)) %>%
  #dplyr::filter(year >= 2006 & year <= 2021) %>%
  dplyr::mutate(counter = 1)

# Intersect the two, which cuts all the polygons by each grid boundary:
mtbs.int <- st_intersection(mtbs_date, dtrange)

### Make a template raster 
r <- raster(dtrange, res = 5000)

### SPlit into list 
mtbs_date_list <- split(mtbs.int, f=mtbs.int$year)

length(mtbs_date_list)

### Create empty list 
yearly.rasters <- list() 


for(i in 1:length(mtbs_date_list)){
  
  mtbs_year <- mtbs_date_list[[i]] %>%
    st_cast(., "MULTIPOLYGON") ## cast to MULTIPOLYGON --- check if that's correct or you lose some polygons 
  
  mtbs_template_raster <- r
  
  ### Rasterize polygons for that year using the template raster 
  test <- rast(fasterize(mtbs_year, mtbs_template_raster, field = "year", fun="max"))
  names(test) <- unique(mtbs_year$year) # name with year 
  
  ### Append raster to list 
  yearly.rasters[[i]] <- test
}

### stack rasters 
yearly.rasters.stack <- rast(yearly.rasters)

### Create year of analysis raster 
temp_2020 <- r
values(temp_2020) <- 2020

### Retain the max value of the raster stack
rmax <- app(yearly.rasters.stack, max, na.rm=T)

### PErform the calculation to find time since last burned 
test_calc <- (rast(temp_2020$layer))-(rmax$max)

### Replace values as necessary if interested in discretizing 
test_calc_discrete <- ifel(test_calc==1, 2, 1)
test_calc_discrete <- ifel(is.na(test_calc_discrete), 0, test_calc_discrete)
