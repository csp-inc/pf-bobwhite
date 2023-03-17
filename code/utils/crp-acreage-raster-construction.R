## ---------------------------
##
## Script name: crp-acreage-raster-construction.R
##
## Purpose of script:Generating a rasterized surface of acreage in bobwhite-relevant Conservation Reserve Program (CRP) enrollment based on acreage records from the Farm Services Agency.
##
## Author: Patrick T. Freeman
##
## Date Created: 2023-03-17
## Date last updated: 2023-03-17
##
## Email contact: patrick[at]csp-inc.org
##
## ---------------------------
##
## Notes: 
##  


library(MazamaSpatialUtils)
library(sf)
library(tidyverse)
library(units)


### Load county-level spatial data 
setSpatialDataDir("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data")
MazamaSpatialUtils::convertUSCensusCounties()
loadSpatialData("USCensusCounties_02")

### read CRP data table for 2020 
df <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/00-raw-data/00_covariates/CRP_COUNTY_PRACTICE_simplified.csv")
glimpse(df)

df$COUNTY <- str_to_title(df$COUNTY, locale = "en")
df$countyFIPS <- df$FIPS

### CRP Codes relevant to bobwhite
NOBOcodes <- c("CP2","CP4", "CP8","CP12","CP15","CP21","CP22","CP25",
               "CP29","CP30","CP33","CP36","CP38","CP42","CP43")

df_long <- df %>%
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

df_nobo <- df_long %>% 
  filter(Practice %in% NOBOcodes,STATE %in% state_abv) %>%
  group_by(STATE,COUNTY, countyFIPS) %>%
  summarise(SumCRP = sum(Acres, na.rm=T)) %>%
  left_join(state_df)

df_nobo$countyFIPS <- ifelse(df_nobo$STATE == "COLORADO",paste("0",df_nobo$countyFIPS, sep =""),df_nobo$countyFIPS)

df_nobo$countyFIPS <- ifelse(df_nobo$STATE == "ALABAMA",paste("0",df_nobo$countyFIPS, sep =""),df_nobo$countyFIPS)
df_nobo$countyFIPS <- ifelse(df_nobo$STATE == "ARKANSAS",paste("0",df_nobo$countyFIPS, sep =""),df_nobo$countyFIPS)


### Calculate area of all counties 
USCensusCounties_02$area <- drop_units(st_area(USCensusCounties_02))/4047

USCensusCounties_02 <- as.data.frame(USCensusCounties_02) 


df_nobo_sp <- full_join(df_nobo, USCensusCounties_02, by=c("countyFIPS", "stateCode")) %>%
  mutate(PercentCRP = (SumCRP/area)*100) %>%
  st_as_sf() %>%
  replace_na(list(PercentCRP = 0)) %>%
  dplyr::filter(stateName %in% state_title)

ggplot(data = df_nobo_sp) +
  geom_sf() +
  geom_sf(data = df_nobo_sp, aes(fill = PercentCRP)) +
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = seq(0,40,1),
                       trans = "identity") +  
  guides(fill=guide_legend(title="Percent CRP")) +
  ggtitle("Percent of each state in CRP as of Jan. 2020") +
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

