library(terra)
library(sf)
library(tidyverse)

test <- rast("/Volumes/GoogleDrive/My Drive/bobwhite-covs/bobwhite-nlcd2019_30m-0000065536-0000065536.tif")

#draw(x="extent", col="red", lwd=2, id=FALSE, n=1000)
extent <- terra::ext(1355922.05955335, 1393325.65756824, 639896.501240695, 659582.605459057) 

test2 <- terra::crop(test, extent)

### Reclassify raster

### Forest = 
# 41 - Deciduous forest: areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. More than 75% of the tree species shed foliage simultaneously in response to seasonal change.
# 42 - Evergreen forest: areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. More than 75% of the tree species maintain their leaves all year. Canopy is never without green foliage.
# 43 - Mixed forest: areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. Neither deciduous nor evergreen species are greater than 75% of total tree cover.
# 90 - Woody wetlands: areas where forest or shrubland vegetation accounts for greater than 20% of vegetative cover and the soil or substrate is periodically saturated with or covered with water.
forest <- cbind(c(41,42,43,90), 1)

### Grassland 
# 71 - Grassland/herbaceous: areas dominated by gramanoid or herbaceous vegetation, generally greater than 80% of total vegetation. These areas are not subject to intensive management such as tilling, but can be utilized for grazing.
grassland <- cbind(c(71), 2)

### Shrub
#52 - Shrub/scrub: areas dominated by shrubs less than 5 meters tall with shrub canopy typically greater than 20% of total vegetation. This class includes true shrubs, young trees in an early successional stage, or trees stunted from environmental conditions.
shrub <- cbind(c(52), 3)

### Pasture
#71- Pasture/hay: areas of grasses, legumes, or grass-legume mixtures planted for livestock grazing or the production of seed or hay crops, typically on a perennial cycle. Pasture/hay vegetation accounts for greater than 20% of total vegetation.
pasture <- cbind(c(71), 4)

### Rowcrop/Cultivated 
# 82 - Cultivated crops: areas used for the production of annual crops, such as corn, soybeans, vegetables, tobacco, and cotton, and also perennial woody crops such as orchards and vineyards. Crop vegetation accounts for greater than 20% of total vegetation. This class also includes all land being actively tilled.
rowcrop <- cbind(c(82),5)

### Development
# 21 - 24 
dev <- cbind(c(21, 22, 23, 24), 6)

### Water
water <- cbind(c(11), 7)

### Bind the entire reclassification matrix
reclass_mat <- rbind(forest, grassland, shrub, pasture, rowcrop, dev, water)

### Reclassify the raster 
rcx1 <- classify(test2, reclass_mat, others=NA)

### Make the sampling grid template 
my_grid_geom = st_make_grid(st_as_sfc(st_bbox(rcx1)), cellsize = 5000)
my_grid_template = st_sf(geom = my_grid_geom)
### Assign a grid cell ID 
my_grid_template$plot_id = seq_len(nrow(my_grid_template))

### Plot to check 
plot(rcx1)
plot(st_geometry(my_grid_template), add = TRUE)

### Calculating landscape metrics for each polygon 
my_metric1 = sample_lsm(rcx1, my_grid_template,
                        level = "class", metric = "pland", progress=T)

### Join results back to sampling grid 
my_grid1 = left_join(my_grid_template, my_metric1, by = "plot_id")

### For-loop for calculating class adjacencies within each grid cell 
grid_out <- list()

for(i in 1:nrow(my_grid_template)){
  
  gridcell <- my_grid_template[i,]
  rast_crop <- crop(rcx1, gridcell)
  
  adj_mat <- get_adjacencies(rast_crop, neighbourhood = 8, what = "unlike", upper = FALSE)
  
  print(paste0("There are ",nrow(adj_mat$layer_1)," rows in the adj mat"))
  print(paste0("There are ",ncol(adj_mat$layer_1)," columns in the adj mat"))
  
  ### This will only work appropriately if the matrix is ALWAYS 6 x 6 and in the same order of landclass combinations 
  ### Forest (1) to grassland (2)
  f=c("1")
  g=c("2")
  
  f2g_mat_out <- adj_mat$layer_1[rownames(adj_mat$layer_1)%in%g,colnames(adj_mat$layer_1)%in%f]
  
  if(length(f2g_mat_out)==0)
    f2g_mat_out <- NA

  gridcell$f2g <- f2g_mat_out
  
  ### Forest (1) to pasture (4)
  f=c("1")
  p=c("4")
  
  f2p_mat_out <- adj_mat$layer_1[rownames(adj_mat$layer_1)%in%p,colnames(adj_mat$layer_1)%in%f]
  
  if(length(f2p_mat_out)==0)
    f2p_mat_out <- NA
  
  gridcell$f2p <- f2p_mat_out
  
  ### Forest (1) to rowcrop (5)
  f=c("1")
  rc=c("5")
  
  f2rc_mat_out <- adj_mat$layer_1[rownames(adj_mat$layer_1)%in%rc,colnames(adj_mat$layer_1)%in%f]
  
  if(length(f2rc_mat_out)==0)
    f2rc_mat_out <- NA
  
  gridcell$f2rc <- f2rc_mat_out
  
  ### Grassland (2) to rowcrop(5)
  g=c("2")
  rc=c("5")
  
  g2rc_mat_out <- adj_mat$layer_1[rownames(adj_mat$layer_1)%in%rc,colnames(adj_mat$layer_1)%in%g]
  
  if(length(g2rc_mat_out)==0)
    g2rc_mat_out <- NA
  
  gridcell$g2rc <- g2rc_mat_out
  
  grid_out[[i]] <- gridcell 
}

grid_out_df <- bind_rows(grid_out)

tm_shape(grid_out_df) +
  tm_polygons("f2g", style = "cont", title = "Forest:Grassland edge") +
  #tm_facets(by = "class", free.coords = FALSE) + 
  tm_layout( 
            title= 'Forest:grassland edge (m)')
  


