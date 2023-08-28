############for larger map
library(zoo)
library(tidyverse)
cov_all<-read.csv("all_covariates.csv")

load("s.Rdata")
res<-s$statistics

df_covariates_new<-cov_all %>% select(grid_id_5k,aspect_5km,elevation_5km,slope_5km,tmax_1821_mean_5km,prcp_1821_mean_5km,snowdays_gt2pt5cm_1821,prop_county_crp_2020,
                                      avg_farm_size_2017,mtbs_burn_freq_0721,Ag_LUI_5km,Energy_LUI_5km,Transport_LUI_5km,Urban_LUI_5km,NLCD_1619_mean_rowcropPcov,NLCD_1619_mean_pasturePcov,
                                      RAP_AFG_1821_mean_5km,RAP_BGR_1821_mean_5km,RAP_PFG_1821_mean_5km,RAP_SHR_1821_mean_5km,RAP_TRE_1821_mean_5km,RAP_PFG_1821_mean_surfaceroughness,
                                      RAP_TRE_1821_mean_surfaceroughness,MLRARSYM,coded_mlra,original_mlra,LRRSYM,LRR_NAME,MLRA_ID,MLRA_NAME,ndvi,OrigEvergreen,Origmixed,Origwater,
                                      Origdeciduous,POINT_Y,POINT_X)
                                      
                                      
                                      

MLRA_grid<-df_covariates_new$coded_mlra

for(i in 1:nrow(df_covariates_new)){
  
  mlra <- as.numeric(as.character(MLRA_grid[i]))
  mean_abun0 <- paste0("abun0[",mlra,']')
  cov_bgr<-paste0("abun_bgr[",mlra,']')
  cov_crp<-paste0("abun_crp[",mlra,']')
  cov_elevation<-paste0("abun_elevation[",mlra,']')
  cov_energy<-paste0("abun_energy[",mlra,']')
  cov_fire<-paste0("abun_fire[",mlra,']')
  cov_pfg<-paste0("abun_pfg[",mlra,']')
  cov_pfg_grad<-paste0("abun_pfg_grad[",mlra,']')
  cov_shrub<-paste0("abun_shrub[",mlra,']')
  cov_tree<-paste0("abun_tree[",mlra,']')
  cov_tree_grad<-paste0("abun_tree_grad[",mlra,']')
  cov_rowcrop<-paste0("abun_rowcrop[",mlra,']')
  cov_ndvi<-paste0("abun_NDVI[",mlra,']')
  cov_deciduous<-paste0("abun_deciduous[",mlra,']')
  
  
  cov_evergreen<-paste0("abun_evergreen[",mlra,']')
  cov_mixture<-paste0("abun_mixed[",mlra,']')
  cov_water<-paste0("abun_water[",mlra,']')
  
  cov_urban<-paste0("abun_urban[",mlra,']')
  cov_transport<-paste0("abun_transport[",mlra,']')
  cov_snowdays<-paste0("abun_snowdays[",mlra,']')
  
  cov_tmax<-paste0("abun_tmax")
  cov_lat<-paste0("abun_lat")
  cov_tmaxlat<-paste0("abun_tmaxlat")
  cov_prcp<-paste0("abun_prcp[",mlra,']')
  cov_pasture<-paste0("abun_pasture[",mlra,']')
  
  
  df_covariates_new$predicted[i] <-exp(
    
    (res$Mean[which(res$X %in% mean_abun0)])+
    
    (res$Mean[which(res$X %in% cov_bgr)] *
       ((df_covariates_new$RAP_BGR_1821_mean_5km[i]-mean(df_covariates_new$RAP_BGR_1821_mean_5km))/
          sd(df_covariates_new$RAP_BGR_1821_mean_5km))) +
      
      (res$Mean[which(res$X %in% cov_crp)] *
         ((df_covariates_new$prop_county_crp_2020[i]-mean(df_covariates_new$prop_county_crp_2020))/
            sd(df_covariates_new$prop_county_crp_2020))) +
      
      
      (res$Mean[which(res$X %in% cov_elevation)] *
         ((df_covariates_new$elevation_5km[i]-mean(df_covariates_new$elevation_5km))/
            sd(df_covariates_new$elevation_5km))) +
      
      
      (res$Mean[which(res$X %in% cov_energy)] *
         ((df_covariates_new$Energy_LUI_5km[i]-mean(df_covariates_new$Energy_LUI_5km))/
            sd(df_covariates_new$Energy_LUI_5km))) +
      
      (res$Mean[which(res$X %in% cov_evergreen)] *
         ((df_covariates_new$OrigEvergreen[i]-mean(df_covariates_new$OrigEvergreen))/
            sd(df_covariates_new$OrigEvergreen))) +
      
      (res$Mean[which(res$X %in% cov_fire)] *
         ((df_covariates_new$mtbs_burn_freq_0721[i]-mean(df_covariates_new$mtbs_burn_freq_0721))/
            sd(df_covariates_new$mtbs_burn_freq_0721))) +
      
      (res$Mean[which(res$X %in% cov_mixture)] *
         ((df_covariates_new$Origmixed[i]-mean(df_covariates_new$Origmixed))/
            sd(df_covariates_new$Origmixed))) +
      
      
      (res$Mean[which(res$X %in% cov_pasture)] *
         ((df_covariates_new$NLCD_1619_mean_pasturePcov[i]-mean(df_covariates_new$NLCD_1619_mean_pasturePcov))/
            sd(df_covariates_new$NLCD_1619_mean_pasturePcov))) +
      
      
      (res$Mean[which(res$X %in% cov_pfg)] *
         ((df_covariates_new$RAP_PFG_1821_mean_5km[i]-mean(df_covariates_new$RAP_PFG_1821_mean_5km))/
            sd(df_covariates_new$RAP_PFG_1821_mean_5km))) +
      
      
      (res$Mean[which(res$X %in% cov_pfg_grad)] *
         ((df_covariates_new$RAP_PFG_1821_mean_surfaceroughness[i]-mean(df_covariates_new$RAP_PFG_1821_mean_surfaceroughness))/
            sd(df_covariates_new$RAP_PFG_1821_mean_surfaceroughness))) +
      
      
      (res$Mean[which(res$X %in% cov_prcp)] *
         ((df_covariates_new$prcp_1821_mean_5km[i]-mean(df_covariates_new$prcp_1821_mean_5km))/
            sd(df_covariates_new$prcp_1821_mean_5km))) +
      
      (res$Mean[which(res$X %in% cov_rowcrop)] *
         ((df_covariates_new$NLCD_1619_mean_rowcropPcov[i]-mean(df_covariates_new$NLCD_1619_mean_rowcropPcov))/
            sd(df_covariates_new$NLCD_1619_mean_rowcropPcov))) +
      
      (res$Mean[which(res$X %in% cov_shrub)] *
         ((df_covariates_new$RAP_SHR_1821_mean_5km[i]-mean(df_covariates_new$RAP_SHR_1821_mean_5km))/
            sd(df_covariates_new$RAP_SHR_1821_mean_5km))) +
      
      (res$Mean[which(res$X %in% cov_snowdays)] *
         ((df_covariates_new$snowdays_gt2pt5cm_1821[i]-mean(df_covariates_new$snowdays_gt2pt5cm_1821))/
            sd(df_covariates_new$snowdays_gt2pt5cm_1821))) +
      
      (res$Mean[which(res$X %in% cov_transport)] *
         ((df_covariates_new$Transport_LUI_5km[i]-mean(df_covariates_new$Transport_LUI_5km))/
            sd(df_covariates_new$Transport_LUI_5km))) +
      
      
      (res$Mean[which(res$X %in% cov_tree)] *
         ((df_covariates_new$RAP_TRE_1821_mean_5km[i]-mean(df_covariates_new$RAP_TRE_1821_mean_5km))/
            sd(df_covariates_new$RAP_TRE_1821_mean_5km))) +
      
      
      (res$Mean[which(res$X %in% cov_tree_grad)] *
         ((df_covariates_new$RAP_TRE_1821_mean_surfaceroughness[i]-mean(df_covariates_new$RAP_TRE_1821_mean_surfaceroughness))/
            sd(df_covariates_new$RAP_TRE_1821_mean_surfaceroughness))) +
      
      
      (res$Mean[which(res$X %in% cov_urban)] *
         ((df_covariates_new$Urban_LUI_5km[i]-mean(df_covariates_new$Urban_LUI_5km))/
            sd(df_covariates_new$Urban_LUI_5km))) +
      
      
      (res$Mean[which(res$X %in% cov_water)] *
         ((df_covariates_new$Origwater[i]-mean(df_covariates_new$Origwater))/
            sd(df_covariates_new$Origwater))) +
      
      (res$Mean[which(res$X %in% cov_ndvi)] *
         ((df_covariates_new$ndvi[i]-mean(df_covariates_new$ndvi))/
            sd(df_covariates_new$ndvi))) +
      
      (res$Mean[which(res$X %in% cov_tmax)] *
      ((df_covariates_new$tmax_1821_mean_5km[i]-mean(df_covariates_new$tmax_1821_mean_5km))/
         sd(df_covariates_new$tmax_1821_mean_5km))) +
    
    
    (res$Mean[which(res$X %in% cov_lat)] *
       ((df_covariates_new$POINT_Y[i]-mean(df_covariates_new$POINT_Y))/
          sd(df_covariates_new$POINT_Y))) +
    
    (res$Mean[which(res$X %in% cov_tmaxlat)] *
       ((df_covariates_new$tmax_1821_mean_5km[i]-mean(df_covariates_new$tmax_1821_mean_5km))/
          sd(df_covariates_new$tmax_1821_mean_5km))*
       ((df_covariates_new$POINT_Y[i]-mean(df_covariates_new$POINT_Y))/
          sd(df_covariates_new$POINT_Y)))

)
  
  
  
}


############################################################################################
library(rgdal)
library(raster)
library(sf)
library(tmap)

updated_grids<-st_read(file.path("grid_regions.shp"))
pred_file<-full_join(updated_grids,df_covariates_new,by="grid_id_5k")
state<-st_read(file.path("grid_states.shp"))

tm_shape(pred_file) + 
  tm_polygons("predicted", palette = "Reds", n = 55,style="quantile",border.col = NULL) + 
  tm_shape(state) +
  tm_polygons(border.col="black",fill=NULL,alpha=0)


