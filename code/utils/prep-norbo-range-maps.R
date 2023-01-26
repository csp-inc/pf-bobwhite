library(sf)
library(tidyverse)
library(spData)

usa <- spData::us_states %>% st_union() %>% st_transform(4326)

conus <- st_read(dsn = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data/tl_2022_us_state/', layer = 'tl_2022_us_state') %>% 
  st_transform(4326) %>% filter(!STUSPS %in% c('AK', 'PR', 'VI', 'MP','GU', 'AS', 'HI')) %>% 
  st_union()


# Unioned range maps
norbo <- st_read(dsn = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data/bobwhite-range-maps/IUCN-Redlist-Range-Maps/', 
                layer = 'iucn_bobwhite_range') %>% 
  st_union() %>% 
  st_intersection(usa) %>% 
  st_write(dsn = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/bobwhite-range-maps/northern-bobwhite-iucn-range', 
           layer = 'norbo_union_range', driver = 'ESRI Shapefile', delete_layer = TRUE)

# Buffered range maps based on literature-based dispersal distances (https://trace.tennessee.edu/nqsp/vol8/iss1/56/)
norbo <- st_read(dsn = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data/bobwhite-range-maps/IUCN-Redlist-Range-Maps/', 
                 layer = 'iucn_bobwhite_range') %>% 
  st_union() %>% 
  st_buffer(dist = 7000) %>% 
  st_simplify(dTolerance = 1000) %>% 
  st_intersection(conus) %>% 
  st_write(dsn = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/bobwhite-range-maps/', 
           layer = 'norbo_buff_7km', driver = 'ESRI Shapefile', delete_layer = TRUE)