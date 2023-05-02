
library(terra)
library(purrr)

# Load raster and polygon data
r <- rapTre_rem_masked$RAP_VegCover_2019_250m_masked_planeremove
p <- aoi_split

# Create a function to crop and mask the raster to each polygon
crop_and_mask <- function(poly) {
  cropped_raster <- terra::crop(r, vect(poly))
  masked_raster <- terra::mask(cropped_raster, vect(poly))
  return(masked_raster)
}

# Use the map function to apply the function to each polygon in the sf object
masked_rasters <- map(p$geometry, crop_and_mask)

# View the masked rasters
masked_rasters

### Set up for gradient analysis  


gradient_analysis <- function(r) {
  output_raster <- texture_image(raster(r$RAP_VegCover_2019_250m_masked_planeremove), 
  size=21,
  window="square",
  metric='sa',
  parallel=T)
return(output_raster)
}

gradient_rasters <- masked_rasters %>%
  map(safely(gradient_analysis))

##### WORKING #####

grid <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/grid_5km.shp")

rast <- rast("/Volumes/GoogleDrive/My Drive/RAP_exports/RAP_VegCover_2019_250m.tif")
rast

centroids <- grid %>%
  st_centroid(.) %>%
  st_buffer(., 5000) 

calc_gradient_metrics <- function(poly) {
  vect <- vect(poly)
  cropped_raster <- terra::crop(rast$TRE, vect)
  masked_raster <- terra::mask(cropped_raster, vect)
  surf_rough <- sa(raster(masked_raster))
  
  # Create a new column with the surf_rough variable
  poly$surf_rough <- surf_rough
  
  # Return a list with both vect and surf_rough objects
  return(list(vect = vect, surf_rough = surf_rough))
}

centroid_sample <- centroids[2000:2010,]

### Apply the functino to every feature in the sf object 
centroid_sample <- centroid_sample %>% 
  mutate(surf_rough = map_dbl(st_geometry(.), ~calc_gradient_metrics(.x)$surf_rough))

