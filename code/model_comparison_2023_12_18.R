## Bobwhite connectivity model comparisons
## Author: Sarah McTague
## Date of last update: 12/19/2023

## Set working directory and load the packages.
options(stringsAsFactors = FALSE)
library(tidyverse)
library(sf)
library(raster)
library(dismo)

## Load the geopackage of e-bird detections. 
detections <- st_read("Data/connectivity-model-ebird-detections.gpkg")
detections <- as_Spatial(detections)
plot(detections)

## Load the tif files for each connectivity model. 
conn_4 <- raster("Data/conn_4.tif")
conn_8 <- raster("Data/conn_8.tif")
conn_16 <- raster("Data/conn_16.tif")
conn_32 <- raster("Data/conn_32.tif")
# plot(conn_4)
# plot(conn_8)
# plot(conn_16)
# plot(conn_32)

## Stack the rasters.
stack <- stack(conn_4, conn_8, conn_16, conn_32)

## Create random locations. 
available <- sampleRandom(stack, size=4932, cells=TRUE, sp=TRUE)
plot(available)

## Extract values for each point across all four rasters.
rasValue_avail <- extract(stack, available) ## On available points first.
average_available <- colMeans(rasValue_avail,  na.rm = TRUE)

rasValue_detect <- extract(stack, detections) ## On detection points.
average_detected <- colMeans(rasValue_detect, na.rm = TRUE)
print(average_available)
print(average_detected)

