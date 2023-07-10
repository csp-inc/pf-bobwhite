library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

### Get states 
states <- ne_states(country='United States of America', returnclass = 'sf')

### Create region state vectors 
southeast <- c('Arkansas','Tennessee','North Carolina','Louisiana','Mississippi','Alabama','Georgia', 'South Carolina','Florida')

plains <- c('South Dakota','Nebraska','Colorado','Kansas','New Mexico','Texas','Oklahoma')

midwest <- c('Minnesota','Wisconsin','Michigan','Iowa','Illinois','Indiana','Ohio','Missouri','Kentucky')

northeast <- c('New Jersey','Pennsylvania','West Virginia','Virginia','Maryland','Delaware')

### Assign regions to states 
states_region_assigned <- states %>%
  dplyr::mutate(ARSregion = 
                  case_when(name %in% southeast ~ 'southeast',
                            name %in% plains ~ 'plains',
                            name %in% midwest ~ 'midwest',
                            name %in% northeast ~ 'northeast')) %>%
  dplyr::filter(!is.na(ARSregion))

### Plot to check
plot(st_geometry(states_region_assigned))

### Union the regions 
ars_regions <- states_region_assigned %>%
  group_by(ARSregion) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_transform(., crs=5070)

### plot to check 
plot(st_geometry(ars_regions))      

### Write to file 
st_write(ars_regions, "connectivity-data/ars_regions_5070.gpkg")
               