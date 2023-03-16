## ---------------------------
##
## Script name: average-farm-size-raster-construction.R
##
## Purpose of script:Generating a rasterized surface of average farm size across all counties in the study area. Average farm size was sourced from USDA QuickStats using the tidyUSDA R package. 
##
## Author: Patrick T. Freeman
##
## Date Created: 2023-03-16
## Date last updated: 2023-03-16
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  


library(tidyUSDA)
library(sf)
library(terra)
library(raster) ## both terra and raster are needed because of dependencies in the fasterize workflow
library(fasterize)
library(tidyverse)
library(tidyterra)
#vignette("using_tidyusda") ### Open vignette if desired 

### Load the USDA QuickStats API key 
key <- '2B0BB9F8-D4C0-37AF-AC43-AFAC8ED8B18C'

### Get some general info about the parameters 
tidyUSDA::allCategory %>% head()
tidyUSDA::allGeogLevel %>% head()

# Get acres per operation by county (AKA average farm size) in 2017 - the last time a country-level census was completed - using the tidyUSA getQuickstat function
acres.per.operation <- tidyUSDA::getQuickstat(
  #sector='ECONOMICS',
  #group='FARMS & LAND & ASSETS',
  #commodity='FARM OPERATIONS',
  #category='AREA OPERATED',
  domain='TOTAL',
  county=NULL,
  key = key,
  program = 'CENSUS',
  data_item = 'FARM OPERATIONS - AREA OPERATED, MEASURED IN ACRES / OPERATION',
  geographic_level = 'COUNTY',
  year = '2017',
  state = NULL,
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = F)

# Plot this data for each state (this takes a long time)
#tidyUSDA::plotUSDA(df = acres.per.operation)

### Load sampling grid for model to use to clip the county dataset 
model_states <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/bobwhite_model_states.gpkg") %>%
  st_union()

### Reproject the county-level average farm size vector to EPSG 5070
acres.per.operation <- acres.per.operation %>% 
  st_transform(., crs=st_crs(model_states))

### Clip to study area 
farm_size_model_area <- st_intersection(acres.per.operation, model_states) %>%
  st_transform(crs=5070) %>%
  st_cast(., "POLYGON") ### have to cast to polygon for this to work 

### Create template raster with same extent as polygons at 5km resolution 
r <- raster(farm_size_model_area, res = 5000)

### Rasterize the polygons using fasterize -- no 'mean' function available so chose max
r <- rast(fasterize(farm_size_model_area, r, field = "Value", fun="max"))


### Check alignment with model sampling grid 
grid5km <- st_read("/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/pf_5km_sampling_grid.gpkg")

def <- ggplot() +
  geom_spatraster(data = r) + 
  geom_sf(data=grid5km, fill="NA")
### Looks generally ok but there are some issues/non-overlapping areas.
