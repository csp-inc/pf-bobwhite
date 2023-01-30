library(auk)
library(tidyverse)
library(lubridate)
library(sf)

# Read species detection data ("ebd") and the sampling effort dataset into auk
ebd <- auk_ebd("data/ebird/ebd_US_norbob_relDec-2022/ebd_US_norbob_relDec-2022.txt", 
               file_sampling = "data/ebird/ebd_sampling_relDec-2022/ebd_sampling_relDec-2022.txt")

# Filter based on survey protocol and to only include complete check lists
ebd_filters <- ebd %>% 
  auk_country(country = 'United States') %>% 
  auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
  auk_complete()

# Run filtering
f_ebd <- file.path("data/ebird/ebd_filtered/ebd_US_norbob_us_filtered.txt")
f_sampling <- file.path("data/ebird/ebd_filtered/ebd_checklists_us_filtered.txt")

# only run if the files don't already exist
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}

# reimport and "zero-fill" to get detected/not-detected dataset for all three species
ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse = TRUE)

# Clean and convert some variables
# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- lubridate::hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

ebd_zf <- ebd_zf %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling", 
                                 0, effort_distance_km),
    # convert time to decimal hours since midnight
    start_time_dec = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )

# Additional filtering to deal with variation in effort/detectability
ebd_zf_filtered <- ebd_zf %>% 
  filter(
    duration_minutes <= 5 * 60,
    effort_distance_km <= 5,
    number_observers <= 10)

# Select columns and write
ebird <- ebd_zf_filtered %>% 
  select(checklist_id, observer_id, sampling_event_identifier,
         scientific_name,
         observation_count, species_observed, 
         state_code, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         time_observations_started, start_time_dec,
         duration_minutes, effort_distance_km,
         number_observers)
 write_csv(ebird, "data/ebird/ebd_norbo_zf_final_20230127.csv", na = "")

# Remove intermediate datasets
rm(ebd_zf_filtered)
rm(ebd_zf)

#----------------------------
# Create species-level datasets and apply spatial filter based on range maps

# Get species range polygons
norbo_range <- st_read(dsn = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1oJ6TJDhezsMmFqpRtiCxuKU5wNSq4CF6/PF Bobwhite/04_Methods_Analysis/01-processed-data/bobwhite-range-maps/',
                      layer = 'norbo_buff_7km')

# Get spatial versions of ebird dataset
ebird <- st_as_sf(ebird, coords = c('longitude','latitude')) 
st_crs(ebird) <- st_crs(ambd_range)

# function to filter based on species name and range polygon
spFilter <- function(data, poly, sp){
  data <- data %>% filter(scientific_name == sp)
  int <- st_intersects(data, poly, sparse = TRUE)
  keep <- lengths(int) > 0
  out <- data[keep,]
  return(out)
}

norbo_data <- spFilter(ebird, ambd_range, "Colinus virginianus")

# Save spatial data
save(list = c('norbo_data', 
     file = 'data/ebird/ebd_norbo_final_20230127.rda')

# Save non-spatial data
norbo_data %>% mutate(longitude = st_coordinates(.)[,1],
                     latitude = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  write_csv("data/ebird/ebd_norbo_final_20230127.csv", na = "")

#----------------
# Save shapefiles and non-spatial data truncated by date
ambd_yr <- ambd_data %>% mutate(longitude = st_coordinates(.)[,1],
                                latitude = st_coordinates(.)[,2]) %>% 
  filter(year >= 2014, year <= 2018) 

ambd_yr %>% st_write(dsn = 'data/ebird/ambd-2014-2018-shp', layer = "ambd-2014-2018",
                     driver = 'ESRI Shapefile', append = FALSE)

ambd_yr %>% st_drop_geometry() %>% write_csv("data/ebird/ebd_ambd_2014_2018.csv", na = "")


