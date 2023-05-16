library(terra)
library(sf)
library(tidyverse)
library(purrr)
library(janitor)

#### Write function to perform extraction when provided a raster 
extractVals <- function(raster){
  pts <- grid5k_centroids
  rast_crs <-st_crs(raster)
  pts_proj <- pts %>%
    st_transform(., crs=rast_crs)
  pts_vect <- vect(pts_proj)
  extraction <- terra::extract(raster, pts_vect, bind=T)
  return(extraction)
}


### Load the 5km sampling grid 
grid5k <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/grid_5km.shp")

### Get centroids from sampling grid 
grid5k_centroids <- st_centroid(grid5k)

### Load covariate rasters 
avg_farm_size <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/NASS_CRP/nass_2017_average_farm_size.tif")
names(avg_farm_size) <- "avg_farm_size_2017"

prop_county_crp <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/NASS_CRP/proportion-county-land-crp-2020.tif")
names(prop_county_crp) <- "prop_county_crp_2020"

snodas <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/SNODAS/snodas-snowdays-mean1621-wgs84.tif")
names(snodas) <- "snowdays_gt1pt5cm_1621"

farm_size_extract <- extractVals(avg_farm_size)
prop_county_crp_extract <- extractVals(prop_county_crp)
snodas_extract <- extractVals(snodas)


### Assemble and extract the gradient metrics

gradient_rast_files <- list.files("/Users/patrickfreeman-csp/Downloads/RAP_Gradient_Rasters/", pattern=".tif", full.names=T)
#import all raster files in folder using lapply
gradient_rasts <- lapply(gradient_rast_files , rast)

getLayerNames <- function(r){
  r_name <- paste0(names(r),"_surfaceroughness")
  return(r_name)
}

gradient_names <- gradient_rasts %>%
  map(getLayerNames)

gradient_rast_stack <- rast(gradient_rasts)
names(gradient_rast_stack) <- gradient_names

rap_gradient_extract <- extractVals(gradient_rast_stack)

#### Extract Climate data 
climate <- rast("/Volumes/GoogleDrive/My Drive/GEE-exports/climate-1621-mean-smoothed270m.tif")
climate_extract <- extractVals(climate)


### Burn Frequency 
burn_freq <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/MTBS_burnfrequency_0721_270m.tif")
burnfreq_extract <- extractVals(burn_freq) %>%
  dplyr::rename(mtbs_burn_freq_0721 = sum)

### Get the GEE-based extractions for LUI and RAP proportional cover and Ag proportional cover
lui_extract <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/02-outputs/bobwhite-5kmgrid-CSP-LUI-extraction-230424.csv") %>%
  rename(Ag_LUI_5km = Ag_5km,
         Energy_LUI_5km = Energy_5km,
         Transport_LUI_5km = Transport_5km,
         Urban_LUI_5km = Urban_5km)

agpcov_extract <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/02-outputs/AgPCov_5km_1619.csv") %>%
  dplyr::select(grid_id_5k, NLCD_1619_mean_rowcropPcov)

rap_extract <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/02-outputs/RAP_5km_smooth.csv") %>%
dplyr::select(-c(`.geo`, `system:index`))

### Full join all of the extracts together from this batch of covariates
extract_out <- as_tibble(full_join(as.data.frame(rap_gradient_extract), as.data.frame(climate_extract), by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., as.data.frame(snodas_extract), by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., as.data.frame(prop_county_crp_extract), by=c("fid", "grid_id_10", "grid_id_5k"))) %>%
  full_join(., as.data.frame(farm_size_extract), by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., as.data.frame(burnfreq_extract), by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., lui_extract, by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., agpcov_extract, by="grid_id_5k") %>%
  full_join(., rap_extract, by="grid_id_5k")
  
### Remove any rows that have any variable with no data...
extract_out_complete_cases <- extract_out %>%
  drop_na()

### Join back to grid for visualization checks and clean up 
grid5k_cov_join <- left_join(extract_out_complete_cases, grid5k, by="grid_id_5k") %>%
  dplyr::select(-fid.y, grid_id_10.y) %>%
  dplyr::rename(fid = fid.x,
                grid_id_10 = grid_id_10.x) %>% 
  st_as_sf()

st_write(grid5k_cov_join, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/02-outputs/grid_5km_covariatejoin.gpkg")
