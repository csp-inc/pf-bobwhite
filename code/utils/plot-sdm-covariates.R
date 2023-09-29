library(tidyterra)
library(ggplot2)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

states <- ne_states(country="United States of America", returnclass="sf")
states_sub <- states %>%
  dplyr::filter(region %in% c("Midwest", "Northeast", "South", "West")) %>%
  dplyr::filter(!name %in% c("Alaska", "Hawaii", "California", "Washington", "Oregon", "Nevada", "Idaho", "Arizona", "Utah", "Montana", "North Dakota", "Minnesota", "New York", "Connecticut", "Rhode Island", "Massachusetts", "Maine", "Vermont", "New Hampshire", "Wyoming")) %>%
  st_transform(crs=5070) %>%
  st_union() %>%
  st_buffer(., -10000)

tree_cover <- crop(rast("/Volumes/GoogleDrive/My Drive/GEE-exports/RAPTREE-1821-mean-smoothed270m.tif"), vect(states_sub), mask=T)

pfg_cover <- crop(rast("/Volumes/GoogleDrive/My Drive/GEE-exports/RAP-1821-mean-smoothed-pfg270m.tif"), vect(states_sub), mask=T)

surface_roughness <- crop(rast("/Users/patrickfreeman-csp/Downloads/02-output-data_00_covariate-data_RAP_gradient_layers_RAP_1821_RAP_1821TRE_Sa_mosaic270m.tif"), vect(states_sub), mask=T)

climate <- crop(rast("/Users/patrickfreeman-csp/Downloads/02-output-data_00_covariate-data_DAYMET_CLIMATE_climate-1821-mean-smoothed1000m.tif"), vect(states_sub), mask=T)

snow_days <- crop(rast("/Users/patrickfreeman-csp/Downloads/02-output-data_00_covariate-data_SNODAS_snodas-snowdays-mean1821-wgs84.tif"), vect(st_transform(states_sub, crs=4326)), mask=T)

avg_farm_size <- crop(rast("/Users/patrickfreeman-csp/Downloads/02-output-data_00_covariate-data_NASS_CRP_nass_2017_average_farm_size.tif"), vect(states_sub), mask=T)

tree_pl <- ggplot() + 
  geom_spatraster(data=tree_cover) + 
  scale_fill_whitebox_c(
    palette="viridi"
  ) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(fill="Proportional Tree Cover") + 
  guides(fill = guide_colorbar(
    direction = "horizontal",
    keyheight = .5,
    keywidth = 2,
    label.position = "bottom",
    nrow = 1,
    title = "Proportional Tree Cover",
    title.hjust = 0.5,
    title.position="top",
    barwidth=25))


sa_pl <- ggplot() + 
  geom_spatraster(data=surface_roughness) + 
  scale_fill_whitebox_c(
    palette="gn_yl",
    limits=c(0, 50),
    direction=-1
  ) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(fill="Surface Roughness (tree cover)") + 
  guides(fill = guide_colorbar(
    direction = "horizontal",
    keyheight = .5,
    keywidth = 2,
    label.position = "bottom",
    nrow = 1,
    title = "Surface Roughness (tree cover)",
    title.hjust = 0.5,
    title.position="top",
    barwidth=25))


pfg_pl <- ggplot() + 
  geom_spatraster(data=pfg_cover) + 
  scale_fill_whitebox_c(
    palette="viridi"
  ) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(fill="Proportional Perennial Forb/Grass Cover") + 
  guides(fill = guide_colorbar(
    direction = "horizontal",
    keyheight = .5,
    keywidth = 2,
    label.position = "bottom",
    nrow = 1,
    title = "Proportional Perennial Forb/Grass Cover",
    title.hjust = 0.5,
    title.position="top",
    barwidth=25))

temp_pl <- ggplot() + 
  geom_spatraster(data=climate$tmax_1821_mean_5km) + 
  scale_fill_whitebox_c(
    palette="muted",
    direction=1
  ) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(fill="Mean Daily Max Temperature (Celsius)") + 
  guides(fill = guide_colorbar(
    direction = "horizontal",
    keyheight = .5,
    keywidth = 2,
    label.position = "bottom",
    nrow = 1,
    title = "Mean Daily Max Temperature (Celcius)",
    title.hjust = 0.5,
    title.position="top",
    barwidth=25))

afs_pl <- ggplot() + 
  geom_spatraster(data=log(avg_farm_size)) + 
  scale_fill_continuous_sequential(
    palette="Inferno",
    rev=T,
    alpha=0.75,
    na.value=NA
    ) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(fill="Average Farm Size (log(acres))") + 
  guides(fill = guide_colorbar(
    direction = "horizontal",
    keyheight = .5,
    keywidth = 2,
    label.position = "bottom",
    nrow = 1,
    title = "Average Farm Size (log(acres))",
    title.hjust = 0.5,
    title.position="top",
    barwidth=25))



snowdays_pl <- ggplot() + 
  geom_spatraster(data=snow_days) + 
  scale_fill_continuous_sequential(
    palette="Dark Mint",
    rev=T,
    alpha=0.75,
    na.value=NA
  ) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(fill="Average # Days with Snow Depth >2.5cm") + 
  guides(fill = guide_colorbar(
    direction = "horizontal",
    keyheight = .5,
    keywidth = 2,
    label.position = "bottom",
    nrow = 1,
    title = "Average # Days with Snow Depth >2.5cm",
    title.hjust = 0.5,
    title.position="top",
    barwidth=25))

(temp_pl + snowdays_pl + pfg_pl) / (tree_pl + sa_pl + afs_pl)


