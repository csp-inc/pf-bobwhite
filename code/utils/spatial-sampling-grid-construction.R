## ---------------------------
##
## Script name: spatial-sampling-grid-construction.R
##
## Purpose of script:Generating regular spatial grid for sampling and summarizing eBird data at 10km x 10km and 5km x 5km resolution. The goal was to ensure that each 5km cell has id information that links it to its appropriate 10km cell. This grid is limited to the 30 state area that is being incorporated into the species distribution model. 
##
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
library(terra)

### Define a function to plot a raster with cell numbers on top (taken from https://www.pmassicotte.com/posts/2022-04-28-changing-spatial-resolution-of-a-raster-with-terra/)
plot_raster <- function(r) {
  plot(r, axes = FALSE, legend = FALSE)
  plot(as.polygons(r, dissolve = FALSE, trunc = FALSE), add = TRUE)
  text(r, digits = 2)
}


# Import 10km polygon grid ------------------------------------------------

grid_10 <- st_read("/Users/patrickfreeman-csp/Downloads/grid 10kmx10km/study_area_grid10km.shp") %>%
  dplyr::mutate(grid10_id = row_number()) %>%
  st_transform(crs=5070) ### convert to equal area projected coordinate system 

# Rasterize 10km grid  ------------------------------------------------


### Create a raster with the same extent at Sprih's polygon grid with values ranging from to ncell in raster
r_10km <- rast(grid_10, res=(10000), vals=c(1:78848))

# Convert 10km grid to 5km grid  ------------------------------------------------

### Disaggregate the 10km x 10km raster by a factor of 2 (for each 10km grid, there should be 4 5kmx5km grids)
r_5km <-disagg(r_10km, fact = 2)

### Mask to the original spatial boundary of Sprih's grid 
r_5km_mask <- terra::mask(r_5km, vect(grid_10))
r_10km_mask <- terra::mask(r_10km, vect(grid_10))

# Plot test area to confirm behavior  ------------------------------------------------

### Grab a test extent to visualize if this is performing correctly
test_extent <- terra::ext(-128608.561909533, -101493.524509481, 848747.917033691, 878667.958302713)
### Crop the raster above and plot it 
test_crop <- terra::crop(r_5km_mask, test_extent)
plot_raster(test_crop)

### As it stands, each 4-cell quadrant has the same value 
test_crop_poly <- as.polygons(test_crop, dissolve=F) %>%
  st_as_sf() %>%
  dplyr::rename(grid_id_10km = lyr.1) %>%
  dplyr::mutate(grid_id_5km = row_number(.))

# Convert masked 5km and 10km rasters to polygons  ------------------------------------------------

grid_5km_poly <- as.polygons(r_5km_mask, dissolve=F)
grid_10km_poly <- as.polygons(r_10km_mask, dissolve=F)


### Add columns to identify the 10km grid cell and 5km grid cell that each cell belongs to
grid_5km_poly_sf <- grid_5km_poly %>%
  st_as_sf() %>%
  dplyr::rename(grid_id_10km = lyr.1) %>%
  dplyr::mutate(grid_id_5km = row_number(.))

grid_10km_poly_sf <- grid_10km_poly %>%
  st_as_sf() %>%
  dplyr::rename(grid_id_10km = lyr.1)

### View to check 
grid_5km_poly_sf %>% dplyr::arrange(., desc(grid_id_10km)) %>% View()

# Write polygon files to geopackage  ------------------------------------------------

st_write(grid_5km_poly_sf, "data/pf_5km_sampling_grid.gpkg")
st_write(grid_10km_poly_sf, "data/pf_10km_sampling_grid.gpkg")

