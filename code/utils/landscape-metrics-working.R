library(tictoc)
library(landscapemetrics)
library(terra)

test <- rast("/Users/patrickfreeman-csp/Documents/GitHub/pf-bobwhite/data/nlcd-playground.tif")

plot(test)

forest_only <- ifel(test$landcover == 1, 1, 0)

ext <- ext(1360592.37510584, 1363425.22861981, 646365.571549534, 650031.617273497)

tic("cropping")
forest_crop <- crop(forest_only, ext)
toc()


tic("window_lsm")
result <- window_lsm(forest_only, window = moving_window, what = c("lsm_l_ed"), progress=T)
toc()

### Edge density units:Meters per hectare

