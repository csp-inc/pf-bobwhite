## ---------------------------
##
## Script name: ebird-spatial-subsampling.R
##
## Purpose of script:This script implements the spatial bias and class imbalance
# corrections described in Robinson et al. (2017, Div. Dist.)
# in which non-detections are spatially sub-sampled and all
# detections are retained (i.e., no sub-sampling of detections).
##
## Author: Patrick T. Freeman and Justin Suraci
##
## Date Created: 2023-02-28
## Date last updated: 
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  

### Load packages using pacman package
pacman::p_load(tidyverse, sf, spData, gridExtra, lubridate, scam, PresenceAbsence, rnaturalearth, rnaturalearthdata)


# GET DATA AND FUNCTIONS
# Get species range map for creating sampling grid
rmap <- st_read('/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/bobwhite-range-maps/northern-bobwhite-iucn-range/norbo_union_range.shp') %>% 
  st_transform(5070) %>%
  st_buffer(., 7000) ## approximate dispersal distance 

states <- ne_states(country="United States of America", returnclass="sf")
states_sub <- states %>%
  dplyr::filter(region %in% c("Midwest", "Northeast", "South", "West")) %>%
  dplyr::filter(!name %in% c("Alaska", "Hawaii", "California", "Washington", "Oregon", "Nevada", "Idaho", "Arizona", "Utah", "Montana", "North Dakota", "Maine", "Vermont", "New Hampshire") )

irrelevant_states <- c("US-AK", "US-AZ", "US-CA", "US-HI", "US-ID", "US-ME", "US-MT", "US-ND", "US-NH", "US-NV", "US-OR", "US-UT", "US-VT", "US-WA")

### Load the zero-filled eBird data prepared by Sprih Harsh of UGA and filter to remove records in irrelevant states to reduce size of the dataset 
ebird <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data/ebird/ebd_whole_US16_22.csv") %>% 
  st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326)) %>% 
  st_transform(5070) %>%
  filter(!state_code %in% irrelevant_states)

# Some functions for preparing sampling grid and sampling within grid
makeGrid <- function(poly, spacing){
  gTemp <- st_make_grid(poly, square = T, cellsize = c(spacing, spacing)) %>% 
    st_sf() # make grid using range polygon as bounding box
  int <- st_intersects(gTemp, poly, sparse = TRUE) # clean up by only keeping grid cells that intersect range
  keep <- lengths(int) > 0
  grid <- gTemp[keep,] %>% 
    mutate(cellNum = 1:nrow(.))
  return(grid)
}

cellSample <- function(points, grid, n = 1){
  int <- st_intersects(points, grid, sparse = FALSE)
  cellNum <- unlist(apply(int, 1, which))
  points$cellNum <- cellNum
  pt_sample <- points %>% 
    group_by(cellNum) %>% 
    sample_n(size = n) %>% 
    ungroup() 
  return(pt_sample)
}


#-------------------------------
#-------------------------------
# SPATIAL SUB SAMPLING

# ** IMPORTANT ** Set spatial grid size for subsampling
gridSize = 25000

# Make sampling grid, based on 
norbo_grid <- makeGrid(rmap, gridSize)

# Divide data into detections and non-detections
# Keep all detections, Keep one detection per grid cell
brd_pres <- ebird %>% filter(species_observed)
brd_abs_sample <- ebird %>% filter(species_observed==F) 

### Filter to brd pres and brd_abs_sample data to the norbo range polygon 
brd_pres_filt <- st_filter(brd_pres, rmap)
brd_abs_sample_filt <- st_filter(brd_abs_sample, rmap)

brd_abs_final_sample <- brd_abs_sample_filt %>% 
  cellSample(norbo_grid) %>% 
  select(-cellNum)



### Get a summary table of what states there were bobwhite detections in the eBird dataset
state_table <- (as.data.frame(with(brd_pres, ftable(sort(state_code)))))


brd_ss <- rbind(brd_pres, brd_abs_sample)