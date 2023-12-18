## ---------------------------
##
## Script name: prep-connectivity-validation-data.R
##
## Purpose of script: Construct a dataset of eBird detections that will be used to assess connectivity model layers based on resistance surfaces with different values of c. 
## Author: Patrick T. Freeman
##
## Date Created: 2023-12-14
## Date last updated: 2023-12-14
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  
library(sf)
library(tidyverse)

### Load eBird dataset used to fit SDM
ebird <- read_csv("/Users/patrickfreeman-csp/Downloads/final_ebird.csv") %>%
  st_as_sf(coords=(c("longitude", "latitude")), crs=4326) %>%
  st_transform(., crs=5070)

### Get sampling grid, union it, and apply negative buffer 
grid_union <- st_read("/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/pf_5km_sampling_grid.gpkg") %>%
  st_transform(., crs=5070) %>%
  st_union() %>% 
  st_buffer(., dist=-10000) 

### Filter the points to just those within the grid (some are on the west coast inexplicably), filter to just include detections, and sample 14000 points randomly 

set.seed(1234)
ebird_detection_samples <- ebird %>%
  dplyr::filter(species_observed==T) %>%
  st_filter(., grid_union)  %>%
  sample_n(., 13000, replace=F)

### Load the 5km grid cells 
grid_disagg <- st_read("/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/pf_5km_sampling_grid.gpkg") %>%
  st_transform(., crs=5070)

### Filter dataset to reain only one record per grid cell 
ebird_detection_samples_grid <- st_join(ebird_detection_samples, grid_disagg, join=st_within) %>%
  group_by(factor(grid_id_5km)) %>%
  dplyr::slice(., 1)

st_write(ebird_detection_samples_grid, "/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/connectivity-model-ebird-detections.gpkg")


