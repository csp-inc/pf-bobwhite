## ---------------------------
##
## Script name: grid-summarize-ebird-data.R
##
## Purpose of script: For each 5km x 5km grid cell in the polygon - tabulate the following metrics for Sprih Harsh
## 1. nsite - an integer value for the number of eBird point-level sampling locations where each location retains information detection covariates like distance traveled, duration in minutes, etc. 
## 2. cell - a vector listing the ID numbers of point-level sampling locations 
## 3. Yi - a vector with a length equal to the number of eBird points in grid i with a detection 
## Author: Patrick T. Freeman
##
## Date Created: 2023-03-07
## Date last updated: 2023-03-07
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  

library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)


# Import eBird and grid datasets ------------------------------------------

ebird <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/ebird-data/final_ebird.csv") %>%
  st_as_sf(coords=(c("longitude", "latitude")), crs=4326) %>%
  st_transform(., crs=5070)
  
grid_5km <- st_read("/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/pf_5km_sampling_grid.gpkg")

### Get states in study area for plotting 
states <- ne_states(country="United States of America", returnclass="sf")
states_sub <- states %>%
  dplyr::filter(region %in% c("Midwest", "Northeast", "South", "West")) %>%
  dplyr::filter(!name %in% c("Alaska", "Hawaii", "California", "Washington", "Oregon", "Nevada", "Idaho", "Arizona", "Utah", "Montana", "North Dakota", "Minnesota", "New York", "Connecticut", "Rhode Island", "Massachusetts", "Maine", "Vermont", "New Hampshire", "Wyoming")) %>%
  st_transform(crs=5070)

# find points within grid cells 
sites_in_grid <- st_join(ebird, grid_5km, join = st_within)

View(head(sites_in_grid))

# Looks like some return NA -- could be that the points are fall outside of the grid cells - pull these to examine 
na_points <- sites_in_grid %>%
  dplyr::filter(is.na(grid_id_10km))
nrow(na_points)

### Oh this dataset contains checklists from all 50 states 
ggplot(states) + geom_sf() + geom_sf(data=na_points, aes(color=species_observed)) + scale_color_viridis_d() + coord_sf()

# Remove points outside of study area -------------------------------------

sites_in_grid_study <- sites_in_grid %>%
  dplyr::filter(!is.na(grid_id_10km))
### How many sampling points remaining? 
nrow(sites_in_grid_study)
### 1089559 - lots of points 
### How many detections/non-detections
ftable(sites_in_grid_study$species_observed)
### 400,746 non-detections, 688,813 detections - not bad! 

### Plot the detection/non-detection points on top of the study sites 
ggplot(states_sub) + geom_sf() + geom_sf(data=sites_in_grid_study, aes(color=species_observed), size=0.5) + scale_color_viridis_d(alpha=0.25) + coord_sf()
