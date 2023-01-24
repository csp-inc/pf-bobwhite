library(sf)
library(tidyverse)
library(spData)

usa <- spData::us_states %>% st_union() %>% st_transform(4326)
conus <- st_read(dsn = 'data/tiger-states-2021/', layer = 'tl_2021_us_state') %>% 
  st_transform(4326) %>% filter(!STUSPS %in% c('AK', 'PR', 'VI', 'MP','GU', 'AS', 'HI')) %>% 
  st_union()

# Unioned range maps
ambd <- st_read(dsn = 'data/focal-species-range-maps/black-duck-iucn-range', 
                layer = 'data_0') %>% 
  st_union() %>% 
  st_intersection(usa) %>% 
  st_write(dsn = 'data/focal-species-range-maps/black-duck-iucn-range', 
           layer = 'ambd_union_range', driver = 'ESRI Shapefile', delete_layer = TRUE)

sagr <- st_read(dsn = 'data/focal-species-range-maps/sage-grouse-iucn-range', 
                layer = 'data_0') %>% 
  st_union() %>% 
  st_intersection(usa) %>% 
  st_write(dsn = 'data/focal-species-range-maps/sage-grouse-iucn-range', 
           layer = 'sagr_union_range', driver = 'ESRI Shapefile', delete_layer = TRUE)

bobo <- st_read(dsn = 'data/focal-species-range-maps/bobolink-iucn-range', 
                layer = 'data_0') %>% 
  st_union() %>% 
  st_intersection(usa) %>% 
  st_write(dsn = 'data/focal-species-range-maps/bobolink-iucn-range', 
           layer = 'bobo_union_range', driver = 'ESRI Shapefile', delete_layer = TRUE)

# Unioned, buffered range maps
sagr_buff <- st_read(dsn = 'data/focal-species-range-maps/sage-grouse-iucn-range', 
                     layer = 'data_0') %>% 
  st_union() %>% 
  st_buffer(dist = 120000) %>% 
  st_simplify(dTolerance = 20000) %>% 
  st_intersection(usa) %>% 
  st_write(dsn = 'data/focal-species-range-maps/sage-grouse-iucn-range', 
           layer = 'sagr_buff_120km', driver = 'ESRI Shapefile', delete_layer = TRUE)

ambd <- st_read(dsn = 'data/focal-species-range-maps/black-duck-iucn-range', 
                layer = 'data_0') %>% 
  st_union() %>% 
  st_buffer(dist = 250000) %>% 
  st_simplify(dTolerance = 30000) %>% 
  st_intersection(conus) %>% 
  st_write(dsn = 'data/focal-species-range-maps/black-duck-iucn-range/', 
           layer = 'ambd_buff_250km', driver = 'ESRI Shapefile', delete_layer = TRUE)