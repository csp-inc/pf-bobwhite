library(tidyverse)
library(sf)

# Get background and detection points with connectivity values extracted
bg <- st_read("data/conn-mod-validation/available_points_bobwhite_II/available.shp") %>% 
  dplyr::select(-cell) %>% 
  mutate(detected = 0)
det <- st_read("data/conn-mod-validation/detection_points_bobwhite_II/detections.shp") %>% 
  mutate(count = obsrvtn_c)
# Subset detections to just those with more than on bird counted
det_mult <- det %>% filter(count > 1)

# Confirm difference between detections and background points
all_pt <- det %>% mutate(detected = 1) %>% 
  dplyr::select(conn_4, conn_8, conn_16, conn_32, detected) %>% 
  rbind(bg)
wilcox.test(all_pt$conn_4[all_pt$detected==0], all_pt$conn_4[all_pt$detected==1])
wilcox.test(all_pt$conn_8[all_pt$detected==0], all_pt$conn_8[all_pt$detected==1])
wilcox.test(all_pt$conn_16[all_pt$detected==0], all_pt$conn_16[all_pt$detected==1])
wilcox.test(all_pt$conn_32[all_pt$detected==0], all_pt$conn_32[all_pt$detected==1])

# Test correlation with counts
cor.test(det$conn_4, det$count, method = 'spearman')
cor.test(det$conn_8, det$count, method = 'spearman')
cor.test(det$conn_16, det$count, method = 'spearman')
cor.test(det$conn_32, det$count, method = 'spearman')

cor.test(det_mult$conn_4, det_mult$count, method = 'spearman')
cor.test(det_mult$conn_8, det_mult$count, method = 'spearman')
cor.test(det_mult$conn_16, det_mult$count, method = 'spearman')
cor.test(det_mult$conn_32, det_mult$count, method = 'spearman')