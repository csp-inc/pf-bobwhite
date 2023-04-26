library(terra)

snodas_18 <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/SNODAS/snodas_days_snowdepth_2018_WGS84.tif")
names(snodas_18) <- c("snodays_18")
snodas_19 <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/SNODAS/snodas_days_snowdepth_2019_WGS84.tif")
names(snodas_19) <- c("snodays_19")
snodas_21 <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/SNODAS/snodas_days_snowdepth_2021_WGS84.tif")
names(snodas_21) <- c("snodays_21")

snodas_list <- list(snodas_18, snodas_19, snodas_21)
r_c <- rast(snodas_list) 

### Load 5km sampling grid 
grid <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/grid_5km.shp")

### Convert to point by getting centroid of every grid 
grid_centroids <- grid %>%
  st_centroid(.) %>%
  st_transform(., crs=4326)


### Extract values to points 
grid_snodays <- extract(r_c, vect(grid_centroids), bind=T)
grid_snodays <- as.data.frame(grid_snodays)

write_csv(grid_snodays, "/Volumes/GoogleDrive/My Drive/bobwhite-covs/bobwhite-5kmgrid-CSP-SNODAS-snowdays-230425.csv")


