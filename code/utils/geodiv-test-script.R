library(raster)
library(terra)
library(tidyverse)
library(sf)
library(tidyterra)
library(geodiv)

rap <- rast("C:/Users/patrick/Documents/bobwhite-data/RAP_VegCover_2019_250m_masked.tif")

model_states <- st_read("C:/Users/patrick/Documents/bobwhite-data/bobwhite_model_states.gpkg") %>% 
  st_transform(., crs=5070) %>%
  st_buffer(., 5000) %>% 
  dplyr::mutate(area = st_area(.)) %>%
  dplyr::arrange(., desc(area)) %>%
  dplyr::slice(., 1)

rap_cropped <- crop(rap, model_states)


#Use the ‘remove_plane’ function of geodiv to remove any trend, if present. This function searches polynomials of orders 0 - 3 to determine which is has the lowest error relative to the surface values. To fit a surface with a user-specified polynomial order, you may use the function ‘fitplane.’ Here, the result is a polynomial surface of order 0, so only the mean surface would be removed.

# Remove a polynomial trend.
# Tree cover (even with 32 GB RAM still not big enough - consider chunking raster into pieces then mosaicing?)
rapTre_rem <- remove_plane(raster::raster(rap_cropped$TRE))


#### Polynomial trend removed: 
rapTre_rem <-terra::rast('C:/Users/patrick/Documents/bobwhite-data/RAP_VegCover_2019_250m_masked_planeremove.tif')
final_states <- vect(st_read("C:/Users/patrick/Documents/bobwhite-data/bobwhite_model_states_noislands.gpkg"))
rapTre_rem_masked <- crop(mask(rapTre_rem, final_states), final_states)

### Aggregate to 1km resolution (for proof of concept)
aggregated <- terra::aggregate(rapTre_rem_masked, fact = 4)


# Texture image creation using 'focal_metrics' function.
# 5 km square moving window
window <- matrix(1, nrow = 5, ncol = 5)
system.time(
  output_raster <- texture_image(raster(rap_cropped$TRE), 
                                 size=5,
                                 window="square",
                                 metric='sa',
                                 parallel=T)
)
print(output_raster)