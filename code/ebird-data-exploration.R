library(tidyverse)
library(sf)
library(spData)
library(gridExtra)
library(lubridate)

states = spData::us_states %>% st_transform(4326)
bird = read.csv('data/ebird/ebd_sagr_final_20220106.csv', header = T) %>% 
  st_as_sf(coords = c('longitude','latitude')) %>% 
  mutate(month = month(observation_date))
st_crs(bird) <- st_crs(4326)

pres <- bird %>% filter(observation_count>0)
abs <- bird %>% filter(observation_count==0) %>% 
  sample_n(3000)
bird_samp_eq <- rbind(pres, abs)
bird_samp <- bird %>% sample_n(10000) %>% 
  mutate(month = month(observation_date), year = year(observation_date))

plot(st_geometry(states))
plot(st_geometry(pres %>% filter(year>2020)), add = T, pch = 16, cex = 0.3, col = 'red')
plot(st_geometry(abs), add = T, pch = ".")

bs_col <- ifelse(bird_samp$species_observed, 'red','black')
bs_cex <- ifelse(bird_samp$species_observed, 0.3,0.1)
plot(st_geometry(bird_samp), add = T, pch = 16, col = bs_col, cex = bs_cex)


# summarize data by hourly bins
breaks <- 0:24
labels <- breaks[-length(breaks)] + diff(breaks) / 2
bird_tod <- bird_samp %>% 
  mutate(tod_bins = cut(start_time_dec, 
                        breaks = breaks, 
                        labels = labels,
                        include.lowest = TRUE),
         tod_bins = as.numeric(as.character(tod_bins))) %>% 
  group_by(tod_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_tod_hist <- ggplot(bird_tod) +
  aes(x = tod_bins, y = n_checklists) +
  geom_segment(aes(xend = tod_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Hours since midnight",
       y = "# checklists",
       title = "Distribution of observation start times")

# frequency of detection
g_tod_freq <- ggplot(bird_tod %>% filter(n_checklists > 100)) +
  aes(x = tod_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hours since midnight",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_tod_hist, g_tod_freq)

#-------------------------
# summarize data by 30 minute bins
breaks <- round(seq(1, 12, by = 1))
labels <- breaks[-length(breaks)] + diff(breaks) / 2
ebird_dur <- bird_samp %>% 
  mutate(dur_bins = cut(month, 
                        breaks = breaks, 
                        labels = labels,
                        include.lowest = FALSE),
         dur_bins = as.numeric(as.character(dur_bins))) %>% 
  group_by(dur_bins) %>% 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_dur_hist <- ggplot(ebird_dur) +
  aes(x = dur_bins, y = n_checklists) +
  geom_segment(aes(xend = dur_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  # scale_y_continuous(labels = scales::comma) +
  labs(x = "Checklist duration (hours)",
       y = "# checklists",
       title = "Distribution of checklist durations")

# frequency of detection
g_dur_freq <- ggplot(ebird_dur %>% filter(n_checklists > 100)) +
  aes(x = dur_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  # scale_y_continuous(labels = scales::percent) +
  labs(x = "Checklist duration (hours)",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_dur_hist, g_dur_freq)