## ---------------------------
##
## Script name: bobwhite-covariate-grid-assembly.R
##
## Purpose of script: A script to fully assemble the covariates for the bobwhite SDM for each 5km grid cell. This is a piecemeal approach that combines exxtractions directly from raters and compiling pre-extracted csvs (exported from GEE) to contend with issues related to file size. Likely will revisit if time allows. 
##
## Author: Patrick T. Freeman
##
## Date Created: 2023-05-16
## Date last updated: 2023-05-18
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  


library(terra)
library(sf)
library(tidyverse)
library(purrr)
library(janitor)
library(smoothr)

#### Write function to perform extraction when provided a raster - transforms input vector to match raster CRS
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
### Replace NA values with 0 for now
farm_size_extract <- extractVals(avg_farm_size)


prop_county_crp <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/NASS_CRP/proportion-county-land-crp-2020.tif")
names(prop_county_crp) <- "prop_county_crp_2020"
### Replace NA values with 0 for now
prop_county_crp_extract <- extractVals(prop_county_crp)


snodas <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/SNODAS/snodas-snowdays-mean1621-wgs84.tif")
names(snodas) <- "snowdays_gt2pt5cm_1621"
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
gradient_rast_stack <- ifel(gradient_rast_stack == 100, NA, gradient_rast_stack)
names(gradient_rast_stack) <- gradient_names

rap_gradient_extract <- extractVals(gradient_rast_stack)

rap_gradient_extract_clean <- as.data.frame(rap_gradient_extract) %>% 
  mutate(dubious_flag=
         case_when(
           RAP_AFG_1621_mean_surfaceroughness>60 | 
             RAP_BGR_1621_mean_surfaceroughness >60 | 
             RAP_PFG_1621_mean_surfaceroughness >60 | 
             RAP_SHR_1621_mean_surfaceroughness >60 | 
             RAP_TRE_1621_mean_surfaceroughness >60 ~ "dubious roughness value",
           TRUE ~ "not dubious roughness value"))



#### Extract Climate data 
climate <- rast("/Users/patrickfreeman-csp/Downloads/climate-1621-mean-smoothed270m.tif")
climate_extract <- extractVals(climate)


### Burn Frequency 
burn_freq <- rast("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/MTBS_burnfrequency_0721_270m.tif")
burnfreq_extract <- extractVals(burn_freq) %>%
  as.data.frame() %>% 
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


### Finally extract terrain covariates
terrain_extract <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/02-outputs/terrain_5km_smooth.csv") %>%
  dplyr::select(-c(`.geo`, `system:index`))


### Full join all of the extracts together from this batch of covariates
extract_out <- as_tibble(full_join(as.data.frame(rap_gradient_extract_clean), as.data.frame(climate_extract), by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., as.data.frame(snodas_extract), by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., as.data.frame(prop_county_crp_extract), by=c("fid", "grid_id_10", "grid_id_5k"))) %>%
  full_join(., as.data.frame(farm_size_extract), by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., as.data.frame(burnfreq_extract), by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., lui_extract, by=c("fid", "grid_id_10", "grid_id_5k")) %>%
  full_join(., agpcov_extract, by="grid_id_5k") %>%
  full_join(., rap_extract, by="grid_id_5k") %>%
  full_join(., terrain_extract, by="grid_id_5k")


### Join back to grid for visualization checks - full version with NA values
grid5k_cov_join_wNA <- full_join(grid5k, extract_out, by=c("grid_id_5k", "grid_id_10", "fid")) %>%
  dplyr::mutate(fid = as.integer(fid)) %>%
  dplyr::select(-fid)


#### Load the grid that was developed by intersecting a negative 10km buffer with the original 5km grid
neg_grid <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/grid5k_neg10kmbuffer.gpkg")
### Get grid IDs to retain 
neg_grid_retain <- unique(neg_grid$grid_id_5k)

### Were some small artifacts to remove further
postbuff_removal <- c(63172, 64322, 64328, 56238, 56239, 68919, 68920, 179573, 80, 102, 99)

### And also some additional ones to keep 
postbuff_keeps <- c(40011, 41181, 41183, 42353, 42354, 42355, 42356, 43523, 43526, 43525)

### Now create a final vector of grid cell IDs to keep
neg_grid_retain_1 <- c(neg_grid_retain, postbuff_keeps)
neg_grid_retain_df <- as.data.frame(neg_grid_retain_1)
neg_grid_retain_df_final <- neg_grid_retain_df %>%
  dplyr::filter(!neg_grid_retain_1 %in% postbuff_removal)


### Label grid cells as within our outside of 10km buffer 
final_grid_withcovs_wNA <- grid5k_cov_join_wNA %>%
  dplyr::mutate(in_out_10kmbuff = case_when(
    grid_id_5k %in% neg_grid_retain_df_final$neg_grid_retain_1 ~ "inside",
    !grid_id_5k %in% neg_grid_retain_df_final$neg_grid_retain_1 ~ "outside"
  )) 

### Write to file as gpkg
st_write(final_grid_withcovs_wNA , "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/02-outputs/covariate_assembly/grid_5km_covariatejoinwNA_neg10kmbufferlabel.gpkg")

### Write to file as csv
final_grid_withcovs_wNA %>%
  st_drop_geometry() %>%
  write_csv(., "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/02-outputs/covariate_assembly/grid_5km_covariatejoinwNA_neg10kmbufferlabel.csv")

