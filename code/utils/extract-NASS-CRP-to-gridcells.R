library(terra)

### Load average farm size raster
farm_size <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/NASS_CRP/nass_2017_average_farm_size.tif")

### Load CRP acreage raster 
crp_acreage <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/NASS_CRP/proportion-county-land-crp-2020.tif")

### Load 5km sampling grid 
grid <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/grid_5km.shp")

### Convert to point by getting centroid of every grid 
grid_centroids <- grid %>%
  st_centroid(.)

### Extract values to points 
grid_farm_size <- extract(farm_size, vect(grid_centroids), bind=T)
grid_farm_size <- as.data.frame(grid_farm_size)
#write_csv(grid_farm_size, "/Volumes/GoogleDrive/My Drive/bobwhite-covs/bobwhite-5kmgrid-CSP-NASS-2017-avg-farm-size-extraction_230425.csv")

grid_crp <- extract(crp_acreage, vect(grid_centroids), bind=T)
grid_crp <- as.data.frame(grid_crp)
#write_csv(grid_crp, "/Volumes/GoogleDrive/My Drive/bobwhite-covs/bobwhite-5kmgrid-CSP-CRP-2020-proportion-county-land_230425.csv")


