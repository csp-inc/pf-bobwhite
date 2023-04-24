library(landscapemetrics)
library(terra)
library(tidyterra)
library(raster)
library(fasterize)
library(sf)
library(tidyverse)
library(cowplot)
library(tictoc)

test_rast <- rast("/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/nlcd-playground.tif")

forest_only <- ifel(test_rast$landcover == 1, 1, 0)

ext <- ext(1360592.37510584, 1363425.22861981, 646365.571549534, 650031.617273497)
forest_crop <- crop(forest_only, ext)

grid <- st_read("/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/pf_5km_sampling_grid.gpkg")

test_grid <- crop(vect(grid), rast(forest_only))

test_grid2 <- st_as_sf(test_grid) 

plot(test_rast)
plot(st_geometry(test_grid2), add=T)

cropped_rast <- crop(test_rast, vect(test_grid2))
plot(cropped_rast)
ncell(cropped_rast)

### Get centroids of each of the grid cells 
test_centroid <- st_centroid(test_grid2)

check_landscape(test_rast)
tic("sample_lsm starting")
### Sample landscape metrics around each centroid with a 6000m radius (to facilitate overlap)
test_out <- sample_lsm(test_rast, 
                       y = test_centroid, 
                       plot_id=test_centroid$grid_id_5km,
                       shape="circle",
                       what = "lsm_l_contag", 
                       size=6000, 
                       return_raster=T, 
                       all_classes=F)
toc("sample_lsm complete")

test_out <- test_out %>%
  dplyr::rename(grid_id_5km=plot_id)

### Get just forest class  edge density 
forest_ed <- test_out %>%
  dplyr::filter(class==1) %>%
  dplyr::rename(grid_id_5km=plot_id)  %>%
  dplyr::select(value, grid_id_5km)

### Join data back to the centroid dataset by the id of the 5km grid cell
out_join <- full_join(test_centroid, test_out, by="grid_id_5km")

### now join back to the POLYGON version of the ID prior to rasterization
out_polygon_join <- out_join  %>%
  st_drop_geometry() %>%
  full_join(., grid, by="grid_id_5km") %>%
  st_as_sf()

### Create a template raster based on the extent of the polygon layer and with a resolution of 5000 sq m per pixel
r <- raster(out_polygon_join, res = 5000)

### Rasterize the polygon using the fasterize function -- using value 'max' for now...perhaps there is a better way to do this? 
r2 <- rast(fasterize(out_polygon_join, r, field = "value", fun="max"))

r3 <- crop(r2, test_rast)
has.colors(test_rast)

### Plotting 
cols <- c("forestgreen", "goldenrod", "brown", "yellow", "khaki4", "red", "darkblue")
ggplot() + geom_spatraster(data=r3) + 
  scale_fill_hypso_c()
