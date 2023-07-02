library(sf)
library(terra)
library(tidyverse)

# ---------- COLLECT INPUTS ---------------
# Habitat suitability surface
hab <- rast("connectivity-data/bobwhite-sdm-final-mlra-5k.tiff") %>% 
  clamp(upper = 495)
# NOBO study area
aoi <- st_read("connectivity-data/bobwhite-grid-union.gpkg")

# ------------- FUNCTIONS --------------------
# Rescale between 0 and 1
rescale01 <- function(r){
  mm <- minmax(r)
  rout <- (r - mm[1])/(mm[2] - mm[1])
  return(rout)
}

# Define neg exponential resistance function (from Keeley et al. 2016 Landscape Ecol)
keeley_rescale <- function(x, c) 100-99*((1-exp(-c*x))/(1-exp(-c)))
resScale <- function(h, c){
  h01 <- rescale01(h)
  # Apply neg exponential conversion
  out <- keeley_rescale(h01, c)
  return(out)
}

# Sample random points based on hab suitability value
# mode = "advanced" codes each point with source strength value 
# mode = "pairwise" codes each point with a unique identifier
get_points <- function(r, n, cut = 0, reassign = 0, mode = "advanced") {
  # Rescale raster to 0-1, define cut-off, and recode all NA to 0
  r <- rescale01(r)
  r[r<cut]<-reassign
  r[is.na(r)] <- 0
  
  
  # Sample cells based on raster value
  cells <- sample(1:ncell(r), n, prob=r[], replace=FALSE)
  
  # Create points from sampled cells
  centers <- xyFromCell(r, cells)
  points <- centers %>%
    as.data.frame() %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(r))
  
  points$id <- 1:nrow(points)
  points$ss <- terra::extract(r, vect(points))[,2]
  out_txt <- points %>% dplyr::select(id, ss) %>% st_drop_geometry() %>% as.matrix() 
  
  # Apply values to points based on 'mode' argument
  if(mode == "pairwise") points$val <- points$id
  if(mode == "advanced") points$val <- points$ss
  
  # Rasterize points using original grid
  out_rast <- terra::rasterize(vect(points), r, field = "val", background = NA)
  return(list(out_rast, out_txt))
}

# ------------- PREP OUTPUTS --------------------
# Set output folder name
# Possible naming convention: {model type}-{resistance scaling}-{n points}
# "pw" = pairwise; "ota" = one-to-all
out_dir = "ota-ne8-n500" # ADJUST BASED ON MODEL TYPE

# Make output folder
if(dir.exists(paste0("connectivity-data/circuitscape-inputs/", out_dir)) == FALSE){
  dir.create(paste0("connectivity-data/circuitscape-inputs/", out_dir))
}

# Random points as sources
ss_name = "source-ota-n500-c01" # ** ADJUST BASED ON SETTINGS IN CALL TO get_points()
ss <- get_points(r = hab, n = 500, cut = 0.01, reassign = 0.01, mode = 'pairwise')
ss_rast_path = paste0("connectivity-data/circuitscape-inputs/", out_dir, "/", ss_name, ".tif")
ss_txt_path = paste0("connectivity-data/circuitscape-inputs/", out_dir, "/", ss_name, ".txt")
terra::writeRaster(ss[[1]], filename = ss_rast_path, overwrite = TRUE)
write.table(ss[[2]], file = ss_txt_path, sep = " ", row.names = FALSE, col.names = FALSE)

# Resistance from habitat suitability
res <- resScale(h = hab, c = 8)
res_name = "resistance-ne8.tif" # ** ADJUST BASED ON SETTINGS IN CALL TO get_points()
res_path = paste0("connectivity-data/circuitscape-inputs/", out_dir, "/", res_name)
terra::writeRaster(res, filename = res_path, overwrite = TRUE)

plot(res)
plot(ss[[1]], add = T, col = 'grey30')
