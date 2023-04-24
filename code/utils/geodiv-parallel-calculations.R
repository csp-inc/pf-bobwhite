
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
