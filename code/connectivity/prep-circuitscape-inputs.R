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
  
  # Apply values to points based on 'mode' argument
  if(mode == "advanced") points$val <- terra::extract(r, vect(points))[,2]
  if(mode == "pairwise") points$val <- 1:nrow(points)
  
  # Rasterize points using original grid
  out <- terra::rasterize(vect(points), r, field = "val", background = NA)
  return(out)
}

# ------------- PREP OUTPUTS --------------------

# Random points as sources
ss <- get_points(hab, 50, cut = 0.01, reassign = 0.01, mode = 'pairwise')
terra::writeRaster(ss, filename = "connectivity-data/circuitscape-inputs/pw-ne8/source-pairwise-n50-c01.tif", overwrite = TRUE)

# Resistance from habitat suitability
res <- resScale(hab, 8)
terra::writeRaster(res, filename = "connectivity-data/circuitscape-inputs/pw-ne8/resistance-ne8.tif", overwrite = TRUE)


par(mfrow = c(1,2))
res <- resScale(hab, 4)
plot(hab)
plot(res)
plot(ss, add = T, col = 'grey20')
