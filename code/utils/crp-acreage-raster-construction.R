## ---------------------------
##
## Script name: crp-acreage-raster-construction.R
##
## Purpose of script:Generating a rasterized surface of acreage in bobwhite-relevant Conservation Reserve Program (CRP) enrollment based on acreage records from the Farm Services Agency.
##
## Author: Patrick T. Freeman
##
## Date Created: 2023-03-17
## Date last updated: 2023-07-20
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  


library(MazamaSpatialUtils)
library(sf)
library(raster)
library(terra)
library(fasterize)
library(tidyverse)
library(units)


### Load county-level spatial data 
setSpatialDataDir("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data")
MazamaSpatialUtils::convertUSCensusCounties()
loadSpatialData("USCensusCounties_02")

### read CRP data table for 2017 and 2020
df_17 <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data/00_covariates/CRP_COUNTY_PRACTICE_2017_simplified.csv")

df_20 <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data/00_covariates/CRP_COUNTY_PRACTICE_Jan2020_simplified.csv")

df_17$COUNTY <- str_to_title(df_17$COUNTY, locale = "en")
df_20$COUNTY <- str_to_title(df_20$COUNTY, locale = "en")

df_17$countyFIPS <- df_17$FIPS
df_20$countyFIPS <- df_20$FIPS


### CRP Codes relevant to bobwhite
NOBOcodes <- c("CP2","CP4", "CP8","CP12","CP15","CP21","CP22","CP25",
               "CP29","CP30","CP33","CP36","CP38","CP42","CP43")

df17_long <- df_17 %>%
  pivot_longer(cols= starts_with("CP"), names_to="Practice",values_to="Acres")

df20_long <- df_20 %>%
  pivot_longer(cols= starts_with("CP"), names_to="Practice",values_to="Acres")

state_abv <- c("ALABAMA", "ARKANSAS", "COLORADO", "DELAWARE", "FLORIDA", "GEORGIA", 
               "ILLINOIS", "INDIANA", "IOWA", "KANSAS","KENTUCKY",
               "LOUISIANA", "MARYLAND", "MICHIGAN", "MISSISSIPPI", "MISSOURI", "NEBRASKA", 
               "NEW JERSEY", "NEW MEXICO", "NORTH CAROLINA", "OHIO", "OKLAHOMA", 
               "PENNSYLVANIA", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "VIRGINIA",
               "WEST VIRGINIA", "WISCONSIN")

state_title <- str_to_title(state_abv)

NOBOstates <- c("AL", "AR", "CO", "DE","FL", "GA",
                "IL", "IN", "IA", "KS","KY",
                "LA","MD", "MI","MS", "MO", "NE", 
                "NJ", "NM", "NC", "OH", "OK", "PA", "SC","SD", "TN","TX","VA","WV", "WI")

state_df <- as.data.frame(cbind(NOBOstates, state_abv))
colnames(state_df) <- c("stateCode","STATE")

df17_nobo <- df17_long %>% 
  filter(Practice %in% NOBOcodes,STATE %in% state_abv) %>%
  group_by(STATE,COUNTY, countyFIPS) %>%
  summarise(SumCRP = sum(Acres, na.rm=T)) %>%
  left_join(state_df)

df20_nobo <- df20_long %>% 
  filter(Practice %in% NOBOcodes,STATE %in% state_abv) %>%
  group_by(STATE,COUNTY, countyFIPS) %>%
  summarise(SumCRP = sum(Acres, na.rm=T)) %>%
  left_join(state_df)

df17_nobo$countyFIPS <- ifelse(df17_nobo$STATE == "COLORADO",paste("0",df17_nobo$countyFIPS, sep =""),df17_nobo$countyFIPS)
df20_nobo$countyFIPS <- ifelse(df20_nobo$STATE == "COLORADO",paste("0",df20_nobo$countyFIPS, sep =""),df20_nobo$countyFIPS)


df20_nobo$countyFIPS <- ifelse(df20_nobo$STATE == "ALABAMA",paste("0",df20_nobo$countyFIPS, sep =""),df20_nobo$countyFIPS)
df20_nobo$countyFIPS <- ifelse(df20_nobo$STATE == "ALABAMA",paste("0",df20_nobo$countyFIPS, sep =""),df20_nobo$countyFIPS)


df17_nobo$countyFIPS <- ifelse(df17_nobo$STATE == "ARKANSAS",paste("0",df17_nobo$countyFIPS, sep =""),df17_nobo$countyFIPS)
df20_nobo$countyFIPS <- ifelse(df20_nobo$STATE == "ARKANSAS",paste("0",df20_nobo$countyFIPS, sep =""),df20_nobo$countyFIPS)


### Calculate area of all counties 
USCensusCounties_02$area <- drop_units(st_area(USCensusCounties_02))/4047

USCensusCounties_02 <- as.data.frame(USCensusCounties_02) 


df17_nobo_sp <- full_join(df17_nobo, USCensusCounties_02, by=c("countyFIPS", "stateCode")) %>%
  mutate(PercentCRP = (SumCRP/area)*100) %>%
  st_as_sf() %>%
  replace_na(list(PercentCRP = 0)) %>%
  dplyr::filter(stateName %in% state_title)

df20_nobo_sp <- full_join(df20_nobo, USCensusCounties_02, by=c("countyFIPS", "stateCode")) %>%
  mutate(PercentCRP = (SumCRP/area)*100) %>%
  st_as_sf() %>%
  replace_na(list(PercentCRP = 0)) %>%
  dplyr::filter(stateName %in% state_title)

ggplot(data = df20_nobo_sp) +
  geom_sf() +
  geom_sf(data = df20_nobo_sp, aes(fill = PercentCRP)) +
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = seq(0,40,1),
                       trans = "identity") +  
  guides(fill=guide_legend(title="Percent CRP")) +
  ggtitle("Percent of each state in CRP as of 2020") +
  theme(panel.grid.major=element_blank(),strip.background = element_blank(),
        strip.text.x = element_text(colour="black",size=12),
        strip.text.y = element_text(colour="black",size=12),
        panel.grid.minor=element_blank(),
        #panel.background = element_rect(fill = "white"),
        axis.line=element_line("black"),
        axis.ticks=element_line("black"),
        axis.text.x = element_text(colour="black",size=12),
        axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        legend.title = element_text(colour="black",size=12),
        legend.text = element_text(colour="black",size=12),
        axis.text.y = element_text(colour="black",size=12), legend.position = c(0.9, 0.1))



### Load sampling grid for model to use to clip the county dataset 
model_states <- st_read("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/bobwhite_model_states.gpkg") %>%
  st_union()


### Reproject the county-level average farm size vector to EPSG 5070
df17_nobo_sp <- df17_nobo_sp %>% 
  st_transform(., crs=st_crs(model_states))
df20_nobo_sp <- df20_nobo_sp %>% 
  st_transform(., crs=st_crs(model_states))

### Clip to study area 
prop_crp17_model_area <- st_intersection(df17_nobo_sp, model_states) %>%
  st_transform(crs=5070) %>%
  st_cast(., "POLYGON") ### have to cast to polygon for this to work 

prop_crp20_model_area <- st_intersection(df20_nobo_sp, model_states) %>%
  st_transform(crs=5070) %>%
  st_cast(., "POLYGON") ### have to cast to polygon for this to work 

# ggplot(data = prop_crp_model_area) +
#   geom_sf() +
#   geom_sf(data = prop_crp_model_area, aes(fill = PercentCRP)) +
#   scale_fill_gradientn(colours = rev(rainbow(7)),
#                        breaks = seq(0,40,1),
#                        trans = "identity") +  
#   guides(fill=guide_legend(title="Percent CRP")) +
#   ggtitle("Percent of each state in CRP as of Jan. 2020") +
#   theme(panel.grid.major=element_blank(),strip.background = element_blank(),
#         strip.text.x = element_text(colour="black",size=12),
#         strip.text.y = element_text(colour="black",size=12),
#         panel.grid.minor=element_blank(),
#         #panel.background = element_rect(fill = "white"),
#         axis.line=element_line("black"),
#         axis.ticks=element_line("black"),
#         axis.text.x = element_text(colour="black",size=12),
#         axis.title.y=element_text(colour="black",size=12),
#         axis.title.x=element_text(colour="black",size=12),
#         legend.title = element_text(colour="black",size=12),
#         legend.text = element_text(colour="black",size=12),
#         axis.text.y = element_text(colour="black",size=12), legend.position = c(0.9, 0.1))


### Create template raster with same extent as polygons at 5km resolution 
r <- raster(prop_crp17_model_area, res = 5000)

### Rasterize the polygons using fasterize -- no 'mean' function available so chose max
r17 <- rast(fasterize(prop_crp17_model_area, r, field = "PercentCRP", fun="max"))

r20 <- rast(fasterize(prop_crp20_model_area, r, field = "PercentCRP", fun="max"))

(def <- ggplot() +
    geom_spatraster(data = r17) + 
    labs(title="Percent of county area enrolled in Bobwhite+ CRP (2017)",
         subtitle="Not normalized by county area"))

writeRaster(r17, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/NASS_CRP/proportion-county-land-crp-2017.tif", overwrite=T)

writeRaster(r20, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/00_covariates/NASS_CRP/proportion-county-land-crp-2020.tif", overwrite=T)

### Check difference between years 
diff_raster <- r20-r17
