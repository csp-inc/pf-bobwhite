library(tidyverse)
library(zoo)
library(wiqid)
set.seed(1234)

###############################MLRA regions
#read data containing details about MLRA regions for each grid
mlra<-read.csv("mlra.csv")

mlra_all <- mlra %>% dplyr::select(grid_id_5k,MLRARSYM,MLRA_ID,MLRA_NAME,LRRSYM,LRR_NAME,BCR)


#extract 50% of each MLRA
mlra50<-mlra_all %>% 
  group_by(MLRARSYM) %>%
  sample_frac(.50)


###############################MLRA regions

###############################processing eBird
#read eBird data
ebd<-read.csv("G:/SDM/model/r code/eBird/pf_ebird_gridassigned.csv")

#filter by year 2018
ebd18<-ebd %>% filter(year==2018)

#select required columns
ebd18_info<-ebd18 %>%
  dplyr::select(grid_id_5km,observation_count,day_of_year,protocol_type,state_code,
                time_observations_started,duration_minutes,effort_distance_km,
                number_observers)

#create checklist record for each grid
ebd18_checklist<-ebd18_info %>%
  group_by(grid_id_5km) %>%
  mutate(cellNo=1:n())

ebd18_checklist<-ebd18_checklist %>%
  group_by(grid_id_5km) %>%
  mutate(maxCellNo=max(cellNo))

#select upto 50 checklists for each grid
ebd18_grids<-ebd18_checklist %>%
  group_by(grid_id_5km) %>%
  slice(1:50)

#create dataset so that each grid has all the information
ebd18_grids_wide <- pivot_wider(ebd18_grids, id_cols=grid_id_5km,names_from=cellNo,values_from=c(observation_count,number_observers,protocol_type,
                                                                                                duration_minutes,effort_distance_km))

#join mlra information to each eBird grid
mlra_ebd<-mlra50
colnames(mlra_ebd)[1]<-"grid_id_5km"
ebd18_grids_wide_mlra<-inner_join(ebd18_grids_wide,mlra_ebd,by="grid_id_5km")

#extract just the count data
ebd18_y_mlra <- ebd18_grids_wide_mlra %>%
  dplyr::select(observation_count_1:observation_count_50)

#create maximum count across each checklist for each grid
ebd18_y_max_mlra<-ebd18_y_mlra %>% rowwise() %>% mutate(max18=max(observation_count_1,observation_count_2,observation_count_3,observation_count_4,observation_count_5,
                                                                  observation_count_6,observation_count_7,observation_count_8,observation_count_9,observation_count_10,
                                                                  observation_count_11,observation_count_12,observation_count_13,observation_count_14,observation_count_15,
                                                                  observation_count_16,observation_count_17,observation_count_18,observation_count_19,observation_count_20,
                                                                  observation_count_21,observation_count_22,observation_count_23,observation_count_24,observation_count_25,
                                                                  observation_count_26,observation_count_27,observation_count_28,observation_count_29,observation_count_30,
                                                                  observation_count_31,observation_count_32,observation_count_33,observation_count_34,observation_count_35,
                                                                  observation_count_36,observation_count_37,observation_count_38,observation_count_39,observation_count_40,
                                                                  observation_count_41,observation_count_42,observation_count_43,observation_count_44,observation_count_45,
                                                                  observation_count_46,observation_count_47,observation_count_48,observation_count_49,observation_count_50,na.rm=T))
ebd18_y_max1_mlra<-ebd18_y_max_mlra %>% dplyr::select(grid_id_5km,max18)

#create detection covariates
ebd18_obs_mlra <- ebd18_grids_wide_mlra %>%
  dplyr::select(number_observers_1:number_observers_50) 
ebd18_obs1_mlra<-round(na.aggregate(ebd18_obs_mlra),0)

ebd18_dur_mlra <- ebd18_grids_wide_mlra %>%
  dplyr::select(duration_minutes_1:duration_minutes_50)
ebd18_dur1_mlra<-na.aggregate(ebd18_dur_mlra)

ebd18_eff_mlra <- ebd18_grids_wide_mlra %>%
  dplyr::select(effort_distance_km_1:effort_distance_km_50)
ebd18_eff1_mlra<-na.aggregate(ebd18_eff_mlra)

ebd18_protocol_mlra <- ebd18_grids_wide_mlra %>%
  dplyr::select(protocol_type_1:protocol_type_50)
ebd18_protocol1_mlra<-round(na.aggregate(ebd18_type1_mlra),0)
#create detection covariates

#formating detection covariates
dur_ebd18_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd18_dur_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd18_dur_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd18_dur_mlra[,-1])),colnames(as.data.frame(ebd18_dur_mlra[,-1])))))

obs_ebd18_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd18_obs_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd18_obs_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd18_obs_mlra[,-1])),colnames(as.data.frame(ebd18_obs_mlra[,-1])))))

protocol_ebd18_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd_protocol_mlra[,-1]))),
                                       dim=c(nrow(as.data.frame(ebd_protocol_mlra[,-1])),50),
                                       dimnames = list(rownames(as.data.frame(ebd_protocol_mlra[,-1])),colnames(as.data.frame(ebd_protocol_mlra[,-1])))))

eff_ebd18_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd18_eff_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd18_eff_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd18_eff_mlra[,-1])),colnames(as.data.frame(ebd18_eff_mlra[,-1])))))


################################################repeating above steps for year 2019 and 2021
ebd19<-ebd %>% filter(year==2019)

ebd19_info<-ebd19 %>%
  dplyr::select(grid_id_5km,observation_count,day_of_year,protocol_type,state_code,
                time_observations_started,duration_minutes,effort_distance_km,
                number_observers)

ebd19_checklist<-ebd19_info %>%
  group_by(grid_id_5km) %>%
  mutate(cellNo=1:n())

ebd19_checklist<-ebd19_checklist %>%
  group_by(grid_id_5km) %>%
  mutate(maxCellNo=max(cellNo))

ebd19_grids<-ebd19_checklist %>%
  group_by(grid_id_5km) %>%
  slice(1:50)


ebd19_grids_wide <- pivot_wider(ebd19_grids, id_cols=grid_id_5km,names_from=cellNo,values_from=c(observation_count,number_observers,protocol_type,
                                                                                                 duration_minutes,effort_distance_km))


ebd19_grids_wide_mlra<-inner_join(ebd19_grids_wide,mlra_ebd,by="grid_id_5km")

ebd19_y_mlra <- ebd19_grids_wide_mlra %>%
  dplyr::select(observation_count_1:observation_count_50)

ebd19_y_max_mlra<-ebd19_y_mlra %>% rowwise() %>% mutate(max19=max(observation_count_1,observation_count_2,observation_count_3,observation_count_4,observation_count_5,
                                                                  observation_count_6,observation_count_7,observation_count_8,observation_count_9,observation_count_10,
                                                                  observation_count_11,observation_count_12,observation_count_13,observation_count_14,observation_count_15,
                                                                  observation_count_16,observation_count_17,observation_count_18,observation_count_19,observation_count_20,
                                                                  observation_count_21,observation_count_22,observation_count_23,observation_count_24,observation_count_25,
                                                                  observation_count_26,observation_count_27,observation_count_28,observation_count_29,observation_count_30,
                                                                  observation_count_31,observation_count_32,observation_count_33,observation_count_34,observation_count_35,
                                                                  observation_count_36,observation_count_37,observation_count_38,observation_count_39,observation_count_40,
                                                                  observation_count_41,observation_count_42,observation_count_43,observation_count_44,observation_count_45,
                                                                  observation_count_46,observation_count_47,observation_count_48,observation_count_49,observation_count_50,na.rm=T))
ebd19_y_max1_mlra<-ebd19_y_max_mlra %>% dplyr::select(grid_id_5km,max19)

ebd19_obs_mlra <- ebd19_grids_wide_mlra %>%
  dplyr::select(number_observers_1:number_observers_50) 
ebd19_obs1_mlra<-round(na.aggregate(ebd19_obs_mlra),0)

ebd19_dur_mlra <- ebd19_grids_wide_mlra %>%
  dplyr::select(duration_minutes_1:duration_minutes_50)
ebd19_dur1_mlra<-na.aggregate(ebd19_dur_mlra)

ebd19_eff_mlra <- ebd19_grids_wide_mlra %>%
  dplyr::select(effort_distance_km_1:effort_distance_km_50)
ebd19_eff1_mlra<-na.aggregate(ebd19_eff_mlra)

ebd19_protocol_mlra <- ebd19_grids_wide_mlra %>%
  dplyr::select(protocol_type_1:protocol_type_50)
ebd19_protocol1_mlra<-round(na.aggregate(ebd19_type1_mlra),0)

dur_ebd19_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd19_dur_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd19_dur_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd19_dur_mlra[,-1])),colnames(as.data.frame(ebd19_dur_mlra[,-1])))))

obs_ebd19_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd19_obs_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd19_obs_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd19_obs_mlra[,-1])),colnames(as.data.frame(ebd19_obs_mlra[,-1])))))

protocol_ebd19_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd_protocol_mlra[,-1]))),
                                       dim=c(nrow(as.data.frame(ebd_protocol_mlra[,-1])),50),
                                       dimnames = list(rownames(as.data.frame(ebd_protocol_mlra[,-1])),colnames(as.data.frame(ebd_protocol_mlra[,-1])))))

eff_ebd19_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd19_eff_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd19_eff_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd19_eff_mlra[,-1])),colnames(as.data.frame(ebd19_eff_mlra[,-1])))))



ebd21<-ebd %>% filter(year==2021)


ebd21_info<-ebd21 %>%
  dplyr::select(grid_id_5km,observation_count,day_of_year,protocol_type,state_code,
                time_observations_started,duration_minutes,effort_distance_km,
                number_observers)

ebd21_checklist<-ebd21_info %>%
  group_by(grid_id_5km) %>%
  mutate(cellNo=1:n())

ebd21_checklist<-ebd21_checklist %>%
  group_by(grid_id_5km) %>%
  mutate(maxCellNo=max(cellNo))

ebd21_grids<-ebd21_checklist %>%
  group_by(grid_id_5km) %>%
  slice(1:50)


ebd21_grids_wide <- pivot_wider(ebd21_grids, id_cols=grid_id_5km,names_from=cellNo,values_from=c(observation_count,number_observers,protocol_type,
                                                                                                 duration_minutes,effort_distance_km))


ebd21_grids_wide_mlra<-inner_join(ebd21_grids_wide,mlra_ebd,by="grid_id_5km")

ebd21_y_mlra <- ebd21_grids_wide_mlra %>%
  dplyr::select(observation_count_1:observation_count_50)

ebd21_y_max_mlra<-ebd21_y_mlra %>% rowwise() %>% mutate(max21=max(observation_count_1,observation_count_2,observation_count_3,observation_count_4,observation_count_5,
                                                                  observation_count_6,observation_count_7,observation_count_8,observation_count_9,observation_count_10,
                                                                  observation_count_11,observation_count_12,observation_count_13,observation_count_14,observation_count_15,
                                                                  observation_count_16,observation_count_17,observation_count_18,observation_count_19,observation_count_20,
                                                                  observation_count_21,observation_count_22,observation_count_23,observation_count_24,observation_count_25,
                                                                  observation_count_26,observation_count_27,observation_count_28,observation_count_29,observation_count_30,
                                                                  observation_count_31,observation_count_32,observation_count_33,observation_count_34,observation_count_35,
                                                                  observation_count_36,observation_count_37,observation_count_38,observation_count_39,observation_count_40,
                                                                  observation_count_41,observation_count_42,observation_count_43,observation_count_44,observation_count_45,
                                                                  observation_count_46,observation_count_47,observation_count_48,observation_count_49,observation_count_50,na.rm=T))
ebd21_y_max1_mlra<-ebd21_y_max_mlra %>% dplyr::select(grid_id_5km,max21)

ebd21_obs_mlra <- ebd21_grids_wide_mlra %>%
  dplyr::select(number_observers_1:number_observers_50) 
ebd21_obs1_mlra<-round(na.aggregate(ebd21_obs_mlra),0)

ebd21_dur_mlra <- ebd21_grids_wide_mlra %>%
  dplyr::select(duration_minutes_1:duration_minutes_50)
ebd21_dur1_mlra<-na.aggregate(ebd21_dur_mlra)

ebd21_eff_mlra <- ebd21_grids_wide_mlra %>%
  dplyr::select(effort_distance_km_1:effort_distance_km_50)
ebd21_eff1_mlra<-na.aggregate(ebd21_eff_mlra)

ebd21_protocol_mlra <- ebd21_grids_wide_mlra %>%
  dplyr::select(protocol_type_1:protocol_type_50)
ebd21_protocol1_mlra<-round(na.aggregate(ebd21_type1_mlra),0)

dur_ebd21_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd21_dur_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd21_dur_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd21_dur_mlra[,-1])),colnames(as.data.frame(ebd21_dur_mlra[,-1])))))

obs_ebd21_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd21_obs_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd21_obs_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd21_obs_mlra[,-1])),colnames(as.data.frame(ebd21_obs_mlra[,-1])))))

protocol_ebd21_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd_protocol_mlra[,-1]))),
                                       dim=c(nrow(as.data.frame(ebd_protocol_mlra[,-1])),50),
                                       dimnames = list(rownames(as.data.frame(ebd_protocol_mlra[,-1])),colnames(as.data.frame(ebd_protocol_mlra[,-1])))))

eff_ebd21_mlra<-standardize(array(data=c(unlist(as.data.frame(ebd21_eff_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(ebd21_eff_mlra[,-1])),50),
                                  dimnames = list(rownames(as.data.frame(ebd21_eff_mlra[,-1])),colnames(as.data.frame(ebd21_eff_mlra[,-1])))))

################################################repeating above steps for year 2019 and 2021

####################combine all three years of eBird dataset 

ebd_all3_mlra<-list(ebd18_y_max1_mlra,ebd19_y_max1_mlra,ebd21_y_max1_mlra) %>% reduce(full_join,by="grid_id_5km")
ebdyr18_mlra<-full_join(ebd18_grids_wide_mlra,ebd_all3_mlra,by="grid_id_5km")
ebdyr19_mlra<-full_join(ebd19_grids_wide_mlra,ebd_all3_mlra,by="grid_id_5km")
ebdyr21_mlra<-full_join(ebd21_grids_wide_mlra,ebd_all3_mlra,by="grid_id_5km")

#############################################################after combining so that each year has equal number of rows format data for models


ebdyr18_y<-ebdyr18_mlra %>% dplyr::select(observation_count_1:observation_count_50)
ebdyr18_dur<-na.aggregate(ebdyr18_mlra %>% dplyr::select(duration_minutes_1:duration_minutes_50))
ebdyr18_dur1<-standardize(array(data=c(unlist(as.data.frame(ebdyr18_dur[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr18_dur[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr18_dur[,-1])),
                                                colnames(as.data.frame(ebdyr18_dur[,-1])))))

ebdyr18_eff<-na.aggregate(ebdyr18_mlra %>% dplyr::select(effort_distance_km_1:effort_distance_km_50))
ebdyr18_eff1<-standardize(array(data=c(unlist(as.data.frame(ebdyr18_eff[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr18_eff[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr18_eff[,-1])),
                                                colnames(as.data.frame(ebdyr18_eff[,-1])))))


ebdyr18_obs<-round(na.aggregate(ebdyr18_mlra %>% dplyr::select(observation_count_1:observation_count_50)),0)
ebdyr18_obs1<-standardize(array(data=c(unlist(as.data.frame(ebdyr18_obs[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr18_obs[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr18_obs[,-1])),
                                                colnames(as.data.frame(ebdyr18_obs[,-1])))))

ebdyr18_type<-cbind(as.numeric(factor(ebdyr18_mlra$protocol_type_1)),as.numeric(factor(ebdyr18_mlra$protocol_type_2)),as.numeric(factor(ebdyr18_mlra$protocol_type_3)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_4)),as.numeric(factor(ebdyr18_mlra$protocol_type_5)),as.numeric(factor(ebdyr18_mlra$protocol_type_6)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_7)),as.numeric(factor(ebdyr18_mlra$protocol_type_8)),as.numeric(factor(ebdyr18_mlra$protocol_type_9)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_10)),as.numeric(factor(ebdyr18_mlra$protocol_type_11)),as.numeric(factor(ebdyr18_mlra$protocol_type_12)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_13)),as.numeric(factor(ebdyr18_mlra$protocol_type_14)),as.numeric(factor(ebdyr18_mlra$protocol_type_15)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_16)),as.numeric(factor(ebdyr18_mlra$protocol_type_17)),as.numeric(factor(ebdyr18_mlra$protocol_type_18)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_19)),as.numeric(factor(ebdyr18_mlra$protocol_type_20)),as.numeric(factor(ebdyr18_mlra$protocol_type_21)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_22)),as.numeric(factor(ebdyr18_mlra$protocol_type_23)),as.numeric(factor(ebdyr18_mlra$protocol_type_24)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_25)),as.numeric(factor(ebdyr18_mlra$protocol_type_26)),as.numeric(factor(ebdyr18_mlra$protocol_type_27)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_28)),as.numeric(factor(ebdyr18_mlra$protocol_type_29)),as.numeric(factor(ebdyr18_mlra$protocol_type_30)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_31)),as.numeric(factor(ebdyr18_mlra$protocol_type_32)),as.numeric(factor(ebdyr18_mlra$protocol_type_33)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_34)),as.numeric(factor(ebdyr18_mlra$protocol_type_35)),as.numeric(factor(ebdyr18_mlra$protocol_type_36)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_37)),as.numeric(factor(ebdyr18_mlra$protocol_type_38)),as.numeric(factor(ebdyr18_mlra$protocol_type_39)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_40)),as.numeric(factor(ebdyr18_mlra$protocol_type_41)),as.numeric(factor(ebdyr18_mlra$protocol_type_42)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_43)),as.numeric(factor(ebdyr18_mlra$protocol_type_44)),as.numeric(factor(ebdyr18_mlra$protocol_type_45)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_46)),as.numeric(factor(ebdyr18_mlra$protocol_type_47)),as.numeric(factor(ebdyr18_mlra$protocol_type_48)),
                    as.numeric(factor(ebdyr18_mlra$protocol_type_49)),as.numeric(factor(ebdyr18_mlra$protocol_type_50)))

ebdyr18_type1<-round(na.aggregate(ebdyr18_type),0)
ebdyr18_type2<-standardize(array(data=c(unlist(as.data.frame(ebdyr18_type1))),
                                 dim=c(nrow(as.data.frame(ebdyr18_type1)),50),
                                 dimnames = list(rownames(as.data.frame(ebdyr18_type1)),
                                                 colnames(as.data.frame(ebdyr18_type1)))))





ebdyr19_y<-ebdyr19_mlra %>% dplyr::select(observation_count_1:observation_count_50)
ebdyr19_dur<-na.aggregate(ebdyr19_mlra %>% dplyr::select(duration_minutes_1:duration_minutes_50))
ebdyr19_dur1<-standardize(array(data=c(unlist(as.data.frame(ebdyr19_dur[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr19_dur[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr19_dur[,-1])),
                                                colnames(as.data.frame(ebdyr19_dur[,-1])))))
ebdyr19_eff<-na.aggregate(ebdyr19_mlra %>% dplyr::select(effort_distance_km_1:effort_distance_km_50))
ebdyr19_eff1<-standardize(array(data=c(unlist(as.data.frame(ebdyr19_eff[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr19_eff[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr19_eff[,-1])),
                                                colnames(as.data.frame(ebdyr19_eff[,-1])))))


ebdyr19_obs<-round(na.aggregate(ebdyr19_mlra %>% dplyr::select(observation_count_1:observation_count_50)),0)
ebdyr19_obs1<-standardize(array(data=c(unlist(as.data.frame(ebdyr19_obs[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr19_obs[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr19_obs[,-1])),
                                                colnames(as.data.frame(ebdyr19_obs[,-1])))))


ebdyr19_type<-cbind(as.numeric(factor(ebdyr19_mlra$protocol_type_1)),as.numeric(factor(ebdyr19_mlra$protocol_type_2)),as.numeric(factor(ebdyr19_mlra$protocol_type_3)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_4)),as.numeric(factor(ebdyr19_mlra$protocol_type_5)),as.numeric(factor(ebdyr19_mlra$protocol_type_6)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_7)),as.numeric(factor(ebdyr19_mlra$protocol_type_8)),as.numeric(factor(ebdyr19_mlra$protocol_type_9)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_10)),as.numeric(factor(ebdyr19_mlra$protocol_type_11)),as.numeric(factor(ebdyr19_mlra$protocol_type_12)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_13)),as.numeric(factor(ebdyr19_mlra$protocol_type_14)),as.numeric(factor(ebdyr19_mlra$protocol_type_15)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_16)),as.numeric(factor(ebdyr19_mlra$protocol_type_17)),as.numeric(factor(ebdyr19_mlra$protocol_type_18)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_19)),as.numeric(factor(ebdyr19_mlra$protocol_type_20)),as.numeric(factor(ebdyr19_mlra$protocol_type_21)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_22)),as.numeric(factor(ebdyr19_mlra$protocol_type_23)),as.numeric(factor(ebdyr19_mlra$protocol_type_24)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_25)),as.numeric(factor(ebdyr19_mlra$protocol_type_26)),as.numeric(factor(ebdyr19_mlra$protocol_type_27)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_28)),as.numeric(factor(ebdyr19_mlra$protocol_type_29)),as.numeric(factor(ebdyr19_mlra$protocol_type_30)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_31)),as.numeric(factor(ebdyr19_mlra$protocol_type_32)),as.numeric(factor(ebdyr19_mlra$protocol_type_33)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_34)),as.numeric(factor(ebdyr19_mlra$protocol_type_35)),as.numeric(factor(ebdyr19_mlra$protocol_type_36)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_37)),as.numeric(factor(ebdyr19_mlra$protocol_type_38)),as.numeric(factor(ebdyr19_mlra$protocol_type_39)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_40)),as.numeric(factor(ebdyr19_mlra$protocol_type_41)),as.numeric(factor(ebdyr19_mlra$protocol_type_42)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_43)),as.numeric(factor(ebdyr19_mlra$protocol_type_44)),as.numeric(factor(ebdyr19_mlra$protocol_type_45)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_46)),as.numeric(factor(ebdyr19_mlra$protocol_type_47)),as.numeric(factor(ebdyr19_mlra$protocol_type_48)),
                    as.numeric(factor(ebdyr19_mlra$protocol_type_49)),as.numeric(factor(ebdyr19_mlra$protocol_type_50)))
ebdyr19_type1<-round(na.aggregate(ebdyr19_type),0)
ebdyr19_type2<-standardize(array(data=c(unlist(as.data.frame(ebdyr19_type1))),
                                 dim=c(nrow(as.data.frame(ebdyr19_type1)),50),
                                 dimnames = list(rownames(as.data.frame(ebdyr19_type1)),
                                                 colnames(as.data.frame(ebdyr19_type1)))))






ebdyr21_y<-ebdyr21_mlra %>% dplyr::select(observation_count_1:observation_count_50)
ebdyr21_dur<-na.aggregate(ebdyr21_mlra %>% dplyr::select(duration_minutes_1:duration_minutes_50))
ebdyr21_dur1<-standardize(array(data=c(unlist(as.data.frame(ebdyr21_dur[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr21_dur[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr21_dur[,-1])),
                                                colnames(as.data.frame(ebdyr21_dur[,-1])))))
ebdyr21_eff<-na.aggregate(ebdyr21_mlra %>% dplyr::select(effort_distance_km_1:effort_distance_km_50))
ebdyr21_eff1<-standardize(array(data=c(unlist(as.data.frame(ebdyr21_eff[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr21_eff[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr21_eff[,-1])),
                                                colnames(as.data.frame(ebdyr21_eff[,-1])))))


ebdyr21_obs<-round(na.aggregate(ebdyr21_mlra %>% dplyr::select(observation_count_1:observation_count_50)),0)
ebdyr21_obs1<-standardize(array(data=c(unlist(as.data.frame(ebdyr21_obs[,-1]))),
                                dim=c(nrow(as.data.frame(ebdyr21_obs[,-1])),50),
                                dimnames = list(rownames(as.data.frame(ebdyr21_obs[,-1])),
                                                colnames(as.data.frame(ebdyr21_obs[,-1])))))



ebdyr21_type<-cbind(as.numeric(factor(ebdyr21_mlra$protocol_type_1)),as.numeric(factor(ebdyr21_mlra$protocol_type_2)),as.numeric(factor(ebdyr21_mlra$protocol_type_3)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_4)),as.numeric(factor(ebdyr21_mlra$protocol_type_5)),as.numeric(factor(ebdyr21_mlra$protocol_type_6)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_7)),as.numeric(factor(ebdyr21_mlra$protocol_type_8)),as.numeric(factor(ebdyr21_mlra$protocol_type_9)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_10)),as.numeric(factor(ebdyr21_mlra$protocol_type_11)),as.numeric(factor(ebdyr21_mlra$protocol_type_12)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_13)),as.numeric(factor(ebdyr21_mlra$protocol_type_14)),as.numeric(factor(ebdyr21_mlra$protocol_type_15)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_16)),as.numeric(factor(ebdyr21_mlra$protocol_type_17)),as.numeric(factor(ebdyr21_mlra$protocol_type_18)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_19)),as.numeric(factor(ebdyr21_mlra$protocol_type_20)),as.numeric(factor(ebdyr21_mlra$protocol_type_21)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_22)),as.numeric(factor(ebdyr21_mlra$protocol_type_23)),as.numeric(factor(ebdyr21_mlra$protocol_type_24)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_25)),as.numeric(factor(ebdyr21_mlra$protocol_type_26)),as.numeric(factor(ebdyr21_mlra$protocol_type_27)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_28)),as.numeric(factor(ebdyr21_mlra$protocol_type_29)),as.numeric(factor(ebdyr21_mlra$protocol_type_30)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_31)),as.numeric(factor(ebdyr21_mlra$protocol_type_32)),as.numeric(factor(ebdyr21_mlra$protocol_type_33)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_34)),as.numeric(factor(ebdyr21_mlra$protocol_type_35)),as.numeric(factor(ebdyr21_mlra$protocol_type_36)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_37)),as.numeric(factor(ebdyr21_mlra$protocol_type_38)),as.numeric(factor(ebdyr21_mlra$protocol_type_39)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_40)),as.numeric(factor(ebdyr21_mlra$protocol_type_41)),as.numeric(factor(ebdyr21_mlra$protocol_type_42)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_43)),as.numeric(factor(ebdyr21_mlra$protocol_type_44)),as.numeric(factor(ebdyr21_mlra$protocol_type_45)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_46)),as.numeric(factor(ebdyr21_mlra$protocol_type_47)),as.numeric(factor(ebdyr21_mlra$protocol_type_48)),
                    as.numeric(factor(ebdyr21_mlra$protocol_type_49)),as.numeric(factor(ebdyr21_mlra$protocol_type_50)))
ebdyr21_type1<-round(na.aggregate(ebdyr21_type),0)
ebdyr21_type2<-standardize(array(data=c(unlist(as.data.frame(ebdyr21_type1))),
                                 dim=c(nrow(as.data.frame(ebdyr21_type1)),50),
                                 dimnames = list(rownames(as.data.frame(ebdyr21_type1)),
                                                 colnames(as.data.frame(ebdyr21_type1)))))


#############final formatting for input data for models
library(abind)
ebdall3_y<-abind(ebdyr18_y[,-1],ebdyr19_y[,-1],ebdyr21_y[,-1],along=3)
ebdall3_dur<-abind(ebdyr18_dur1,ebdyr19_dur1,ebdyr21_dur1,along=3)
ebdall3_eff<-abind(ebdyr18_eff1,ebdyr19_eff1,ebdyr21_eff1,along=3)
ebdall3_obs<-abind(ebdyr18_obs1,ebdyr19_obs1,ebdyr21_obs1,along=3)
ebdall3_type<-abind(ebdyr18_type2,ebdyr19_type2,ebdyr21_type2,along=3)


###############################processing eBird

#############################processing BBS dataset
bbs18<-read.csv("bbs18Grid.csv")

#remove any outliers and select required columns
bbs18_grid1<-bbs18[bbs18$grid_id_5k!=0,]
bbs18_grid2<-bbs18_grid1 %>% dplyr::select(stop_id,stops,stops_coun,grid_id_5k,Noise1:Noise50,Car1:Car50)

#information like nosie and car is associated with each stops
bbs18_grid3_noise<-bbs18_grid2 %>% pivot_longer(cols=c('Noise1','Noise2','Noise3','Noise4','Noise5','Noise6','Noise7','Noise8','Noise9','Noise10',
                                                       'Noise11','Noise12','Noise13','Noise14','Noise15','Noise16','Noise17','Noise18','Noise19','Noise20',
                                                       'Noise21','Noise22','Noise23','Noise24','Noise25','Noise26','Noise27','Noise28','Noise29','Noise30',
                                                       'Noise31','Noise32','Noise33','Noise34','Noise35','Noise36','Noise37','Noise38','Noise39','Noise40',
                                                       'Noise41','Noise42','Noise43','Noise44','Noise45','Noise46','Noise47','Noise48','Noise49','Noise50'),
                                                names_to = 'Noise', values_to = 'NoiseIndex')

bbs18_grid3_car<-bbs18_grid2 %>% pivot_longer(cols=c('Car1','Car2','Car3','Car4','Car5','Car6','Car7','Car8','Car9','Car10',
                                                     'Car11','Car12','Car13','Car14','Car15','Car16','Car17','Car18','Car19','Car20',
                                                     'Car21','Car22','Car23','Car24','Car25','Car26','Car27','Car28','Car29','Car30',
                                                     'Car31','Car32','Car33','Car34','Car35','Car36','Car37','Car38','Car39','Car40',
                                                     'Car41','Car42','Car43','Car44','Car45','Car46','Car47','Car48','Car49','Car50'),
                                              names_to = 'Car', values_to = 'CarIndex')


bbs18_grid4_car<-bbs18_grid3_car %>% group_by(stop_id) %>% slice(1:1)

bbs18_grid5_car<-bbs18_grid4_car %>% select(stop_id,stops_coun,grid_id_5k,CarIndex)

#processing for each grid, select stops upto maximum number of stops in a grid
bbs18_grid_stops_car<-bbs18_grid5_car %>%
  group_by(grid_id_5k) %>%
  mutate(cellNo=1:n())


bbs18_grid_stops_car<-bbs18_grid_stops_car %>%
  group_by(grid_id_5k) %>%
  mutate(maxcellNo=max(cellNo))



bbs18_grid_stops1_car<-bbs18_grid_stops_car %>%
  group_by(grid_id_5k) %>%
  slice(1:18)


bbs18_5_grid_wide_car <- pivot_wider(bbs18_grid_stops1_car, id_cols=grid_id_5k,names_from=cellNo,values_from=c(stops_coun,CarIndex))


bbs18_grid4_noise<-bbs18_grid3_noise %>% group_by(stop_id) %>% slice(1:1)

bbs18_grid5_noise<-bbs18_grid4_noise %>% select(stop_id,stops_coun,grid_id_5k,NoiseIndex)

bbs18_grid_stops_noise<-bbs18_grid5_noise %>%
  group_by(grid_id_5k) %>%
  mutate(cellNo=1:n())


bbs18_grid_stops1_noise<-bbs18_grid_stops_noise %>%
  group_by(grid_id_5k) %>%
  slice(1:18)


bbs18_5_grid_wide_noise <- pivot_wider(bbs18_grid_stops1_noise, id_cols=grid_id_5k,names_from=cellNo,values_from=c(stops_coun,NoiseIndex))

bbs18_5_wide_noise <- bbs18_5_grid_wide_noise %>%
  dplyr::select(NoiseIndex_1:NoiseIndex_18)
#processing for each grid, select stops upto maximum number of stops in a grid

#all information about bbs18 and join mlra information
bbs18_grid_wide<-full_join(bbs18_5_grid_wide_car,bbs18_5_wide_noise,by="grid_id_5k")

bbs18_grid_wide_mlra<-inner_join(bbs18_grid_wide,mlra50,by="grid_id_5k")


bbs18_y_mlra <- bbs18_grid_wide_mlra %>%
  dplyr::select(stops_coun_1:stops_coun_18)

bbs18_noise_mlra <-bbs18_grid_wide_mlra %>%
  dplyr::select(NoiseIndex_1:NoiseIndex_18)

bbs18_car_mlra <-bbs18_grid_wide_mlra %>%
  dplyr::select(CarIndex_1:CarIndex_18)


bbs18_noise1_mlra<-na.aggregate(bbs18_noise_mlra)

noise_bbs18_mlra<-standardize(array(data=c(unlist(as.data.frame(bbs18_noise_mlra[,-1]))),
                                    dim=c(nrow(as.data.frame(bbs18_noise_mlra[,-1])),18),
                                    dimnames = list(rownames(as.data.frame(bbs18_noise_mlra[,-1])),colnames(as.data.frame(bbs18_noise_mlra[,-1])))))

bbs18_car1_mlra<-na.aggregate(bbs18_car_mlra)

car_bbs18_mlra<-standardize(array(data=c(unlist(as.data.frame(bbs18_car_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(bbs18_car_mlra[,-1])),18),
                                  dimnames = list(rownames(as.data.frame(bbs18_car_mlra[,-1])),colnames(as.data.frame(bbs18_car_mlra[,-1])))))

bbs18_y_max_mlra<-bbs18_y_mlra %>% rowwise() %>% mutate(bmax18=max(stops_coun_1,stops_coun_2,stops_coun_3,stops_coun_4,stops_coun_5,
                                                                   stops_coun_6,stops_coun_7,stops_coun_8,stops_coun_9,stops_coun_10,
                                                                   stops_coun_11,stops_coun_12,stops_coun_13,stops_coun_14,stops_coun_15,
                                                                   stops_coun_16,stops_coun_17,stops_coun_18,na.rm = T))
bbs18_max_mlra<-bbs18_y_max_mlra %>% select(grid_id_5k,bmax18)

##########Repeat above steps for more years####
bbs19<-read.csv("bbs19Grid.csv")

bbs19_grid1<-bbs19[bbs19$grid_id_5k!=0,]
bbs19_grid2<-bbs19_grid1 %>% dplyr::select(stop_id,stops,stops_coun,grid_id_5k,Noise1:Noise50,Car1:Car50)


bbs19_grid3_noise<-bbs19_grid2 %>% pivot_longer(cols=c('Noise1','Noise2','Noise3','Noise4','Noise5','Noise6','Noise7','Noise8','Noise9','Noise10',
                                                       'Noise11','Noise12','Noise13','Noise14','Noise15','Noise16','Noise17','Noise18','Noise19','Noise20',
                                                       'Noise21','Noise22','Noise23','Noise24','Noise25','Noise26','Noise27','Noise28','Noise29','Noise30',
                                                       'Noise31','Noise32','Noise33','Noise34','Noise35','Noise36','Noise37','Noise38','Noise39','Noise40',
                                                       'Noise41','Noise42','Noise43','Noise44','Noise45','Noise46','Noise47','Noise48','Noise49','Noise50'),
                                                names_to = 'Noise', values_to = 'NoiseIndex')

bbs19_grid3_car<-bbs19_grid2 %>% pivot_longer(cols=c('Car1','Car2','Car3','Car4','Car5','Car6','Car7','Car8','Car9','Car10',
                                                     'Car11','Car12','Car13','Car14','Car15','Car16','Car17','Car18','Car19','Car20',
                                                     'Car21','Car22','Car23','Car24','Car25','Car26','Car27','Car28','Car29','Car30',
                                                     'Car31','Car32','Car33','Car34','Car35','Car36','Car37','Car38','Car39','Car40',
                                                     'Car41','Car42','Car43','Car44','Car45','Car46','Car47','Car48','Car49','Car50'),
                                              names_to = 'Car', values_to = 'CarIndex')


bbs19_grid4_car<-bbs19_grid3_car %>% group_by(stop_id) %>% slice(1:1)

bbs19_grid5_car<-bbs19_grid4_car %>% select(stop_id,stops_coun,grid_id_5k,CarIndex)

bbs19_grid_stops_car<-bbs19_grid5_car %>%
  group_by(grid_id_5k) %>%
  mutate(cellNo=1:n())

bbs19_grid_stops_car<-bbs19_grid_stops_car %>%
  group_by(grid_id_5k) %>%
  mutate(maxcellNo=max(cellNo))


bbs19_grid_stops1_car<-bbs19_grid_stops_car %>%
  group_by(grid_id_5k) %>%
  slice(1:18)


bbs19_5_grid_wide_car <- pivot_wider(bbs19_grid_stops1_car, id_cols=grid_id_5k,names_from=cellNo,values_from=c(stops_coun,CarIndex))


bbs19_grid4_noise<-bbs19_grid3_noise %>% group_by(stop_id) %>% slice(1:1)

bbs19_grid5_noise<-bbs19_grid4_noise %>% select(stop_id,stops_coun,grid_id_5k,NoiseIndex)

bbs19_grid_stops_noise<-bbs19_grid5_noise %>%
  group_by(grid_id_5k) %>%
  mutate(cellNo=1:n())


bbs19_grid_stops1_noise<-bbs19_grid_stops_noise %>%
  group_by(grid_id_5k) %>%
  slice(1:18)


bbs19_5_grid_wide_noise <- pivot_wider(bbs19_grid_stops1_noise, id_cols=grid_id_5k,names_from=cellNo,values_from=c(stops_coun,NoiseIndex))

bbs19_5_wide_noise <- bbs19_5_grid_wide_noise %>%
  dplyr::select(NoiseIndex_1:NoiseIndex_18)

#all bbs19 information
bbs19_grid_wide<-full_join(bbs19_5_grid_wide_car,bbs19_5_wide_noise,by="grid_id_5k")

bbs19_grid_wide_mlra<-inner_join(bbs19_grid_wide,mlra50,by="grid_id_5k")

bbs19_y_mlra <- bbs19_grid_wide_mlra %>%
  dplyr::select(stops_coun_1:stops_coun_18)

bbs19_noise_mlra <- bbs19_grid_wide_mlra %>%
  dplyr::select(NoiseIndex_1:NoiseIndex_18)

bbs19_car_mlra <- bbs19_grid_wide_mlra %>%
  dplyr::select(CarIndex_1:CarIndex_18)

bbs19_noise1_mlra<-na.aggregate(bbs19_noise_mlra)

noise_bbs19_mlra<-standardize(array(data=c(unlist(as.data.frame(bbs19_noise_mlra[,-1]))),
                                    dim=c(nrow(as.data.frame(bbs19_noise_mlra[,-1])),18),
                                    dimnames = list(rownames(as.data.frame(bbs19_noise_mlra[,-1])),colnames(as.data.frame(bbs19_noise_mlra[,-1])))))

bbs19_car1_mlra<-na.aggregate(bbs19_car_mlra)

car_bbs19_mlra<-standardize(array(data=c(unlist(as.data.frame(bbs19_car_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(bbs19_car_mlra[,-1])),18),
                                  dimnames = list(rownames(as.data.frame(bbs19_car_mlra[,-1])),colnames(as.data.frame(bbs19_car_mlra[,-1])))))

bbs19_y_max_mlra<-bbs19_y_mlra %>% rowwise() %>% mutate(bmax19=max(stops_coun_1,stops_coun_2,stops_coun_3,stops_coun_4,stops_coun_5,
                                                                   stops_coun_6,stops_coun_7,stops_coun_8,stops_coun_9,stops_coun_10,
                                                                   stops_coun_11,stops_coun_12,stops_coun_13,stops_coun_14,stops_coun_15,
                                                                   stops_coun_16,stops_coun_17,stops_coun_18,na.rm = T))
bbs19_max_mlra<-bbs19_y_max_mlra %>% select(grid_id_5k,bmax19)

#---------------------------------------------------------------------------------------------------#
bbs21<-read.csv("bbs21Grid.csv")
bbs21_grid1<-bbs21[bbs21$grid_id_5k!=0,]
bbs21_grid2<-bbs21_grid1 %>% dplyr::select(stop_id,stops,stops_coun,grid_id_5k,Noise1:Noise50,Car1:Car50)


bbs21_grid3_noise<-bbs21_grid2 %>% pivot_longer(cols=c('Noise1','Noise2','Noise3','Noise4','Noise5','Noise6','Noise7','Noise8','Noise9','Noise10',
                                                       'Noise11','Noise12','Noise13','Noise14','Noise15','Noise16','Noise17','Noise18','Noise19','Noise20',
                                                       'Noise21','Noise22','Noise23','Noise24','Noise25','Noise26','Noise27','Noise28','Noise29','Noise30',
                                                       'Noise31','Noise32','Noise33','Noise34','Noise35','Noise36','Noise37','Noise38','Noise39','Noise40',
                                                       'Noise41','Noise42','Noise43','Noise44','Noise45','Noise46','Noise47','Noise48','Noise49','Noise50'),
                                                names_to = 'Noise', values_to = 'NoiseIndex')

bbs21_grid3_car<-bbs21_grid2 %>% pivot_longer(cols=c('Car1','Car2','Car3','Car4','Car5','Car6','Car7','Car8','Car9','Car10',
                                                     'Car11','Car12','Car13','Car14','Car15','Car16','Car17','Car18','Car19','Car20',
                                                     'Car21','Car22','Car23','Car24','Car25','Car26','Car27','Car28','Car29','Car30',
                                                     'Car31','Car32','Car33','Car34','Car35','Car36','Car37','Car38','Car39','Car40',
                                                     'Car41','Car42','Car43','Car44','Car45','Car46','Car47','Car48','Car49','Car50'),
                                              names_to = 'Car', values_to = 'CarIndex')


bbs21_grid4_car<-bbs21_grid3_car %>% group_by(stop_id) %>% slice(1:1)

bbs21_grid5_car<-bbs21_grid4_car %>% select(stop_id,stops_coun,grid_id_5k,CarIndex)

bbs21_grid_stops_car<-bbs21_grid5_car %>%
  group_by(grid_id_5k) %>%
  mutate(cellNo=1:n())

bbs21_grid_stops_car<-bbs21_grid_stops_car %>%
  group_by(grid_id_5k) %>%
  mutate(maxcellNo=max(cellNo))
range(bbs21_grid_stops_car$maxcellNo)


bbs21_grid_stops1_car<-bbs21_grid_stops_car %>%
  group_by(grid_id_5k) %>%
  slice(1:18)


bbs21_5_grid_wide_car <- pivot_wider(bbs21_grid_stops1_car, id_cols=grid_id_5k,names_from=cellNo,values_from=c(stops_coun,CarIndex))


bbs21_grid4_noise<-bbs21_grid3_noise %>% group_by(stop_id) %>% slice(1:1)

bbs21_grid5_noise<-bbs21_grid4_noise %>% select(stop_id,stops_coun,grid_id_5k,NoiseIndex)

bbs21_grid_stops_noise<-bbs21_grid5_noise %>%
  group_by(grid_id_5k) %>%
  mutate(cellNo=1:n())


bbs21_grid_stops1_noise<-bbs21_grid_stops_noise %>%
  group_by(grid_id_5k) %>%
  slice(1:18)


bbs21_5_grid_wide_noise <- pivot_wider(bbs21_grid_stops1_noise, id_cols=grid_id_5k,names_from=cellNo,values_from=c(stops_coun,NoiseIndex))

bbs21_5_wide_noise <- bbs21_5_grid_wide_noise %>%
  dplyr::select(NoiseIndex_1:NoiseIndex_18)

#all bbs21 information
bbs21_grid_wide<-full_join(bbs21_5_grid_wide_car,bbs21_5_wide_noise,by="grid_id_5k")

bbs21_grid_wide_mlra<-inner_join(bbs21_grid_wide,mlra50,by="grid_id_5k")

bbs21_y_mlra <- bbs21_grid_wide_mlra %>%
  dplyr::select(stops_coun_1:stops_coun_18)

bbs21_noise_mlra <- bbs21_grid_wide_mlra %>%
  dplyr::select(NoiseIndex_1:NoiseIndex_18)

bbs21_car_mlra <- bbs21_grid_wide_mlra %>%
  dplyr::select(CarIndex_1:CarIndex_18)

bbs21_noise1_mlra<-na.aggregate(bbs21_noise_mlra)

noise_bbs21_mlra<-standardize(array(data=c(unlist(as.data.frame(bbs21_noise_mlra[,-1]))),
                                    dim=c(nrow(as.data.frame(bbs21_noise_mlra[,-1])),18),
                                    dimnames = list(rownames(as.data.frame(bbs21_noise_mlra[,-1])),colnames(as.data.frame(bbs21_noise_mlra[,-1])))))

bbs21_car1_mlra<-na.aggregate(bbs21_car_mlra)

car_bbs21_mlra<-standardize(array(data=c(unlist(as.data.frame(bbs21_car_mlra[,-1]))),
                                  dim=c(nrow(as.data.frame(bbs21_car_mlra[,-1])),18),
                                  dimnames = list(rownames(as.data.frame(bbs21_car_mlra[,-1])),colnames(as.data.frame(bbs21_car_mlra[,-1])))))

bbs21_y_max_mlra<-bbs21_y_mlra %>% rowwise() %>% mutate(bmax21=max(stops_coun_1,stops_coun_2,stops_coun_3,stops_coun_4,stops_coun_5,
                                                                   stops_coun_6,stops_coun_7,stops_coun_8,stops_coun_9,stops_coun_10,
                                                                   stops_coun_11,stops_coun_12,stops_coun_13,stops_coun_14,stops_coun_15,
                                                                   stops_coun_16,stops_coun_17,stops_coun_18,na.rm = T))
bbs21_max_mlra<-bbs21_y_max_mlra %>% select(grid_id_5k,bmax21)

######################combine all 3 years of data########################################
bbs_all3_mlra<-list(bbs18_max_mlra,bbs19_max_mlra,bbs21_max_mlra) %>% reduce(full_join,by="grid_id_5k")
bbsyr18_mlra<-full_join(bbs18_grid_wide_mlra,bbs_all3_mlra,by="grid_id_5k")
bbsyr19_mlra<-full_join(bbs19_grid_wide_mlra,bbs_all3_mlra,by="grid_id_5k")
bbsyr21_mlra<-full_join(bbs21_grid_wide_mlra,bbs_all3_mlra,by="grid_id_5k")


bbsyr18_y<-bbsyr18_mlra %>% select(stops_coun_1:stops_coun_18)
bbsyr18_noise <-bbsyr18_mlra %>% select(NoiseIndex_1:NoiseIndex_18)
bbsyr18_noisea<-na.aggregate(bbsyr18_noise)
bbsyr18_noise1<-standardize(array(data=c(unlist(as.data.frame(bbsyr18_noisea[,-1]))),
                                  dim=c(nrow(as.data.frame(bbsyr18_noisea[,-1])),18),
                                  dimnames = list(rownames(as.data.frame(bbsyr18_noisea[,-1])),
                                                  colnames(as.data.frame(bbsyr18_noisea[,-1])))))
bbsyr18_noise1[!is.finite(bbsyr18_noise1)]<-0

bbsyr18_car <-bbsyr18_mlra %>% select(CarIndex_1:CarIndex_18)
bbsyr18_cara<-na.aggregate(bbsyr18_car)
bbsyr18_car1<-standardize(array(data=c(unlist(as.data.frame(bbsyr18_cara[,-1]))),
                                dim=c(nrow(as.data.frame(bbsyr18_cara[,-1])),18),
                                dimnames = list(rownames(as.data.frame(bbsyr18_cara[,-1])),
                                                colnames(as.data.frame(bbsyr18_cara[,-1])))))
bbsyr18_car1[!is.finite(bbsyr18_car1)]<-0



bbsyr19_y<-bbsyr19_mlra %>% select(stops_coun_1:stops_coun_18)
bbsyr19_noise <-bbsyr19_mlra %>% select(NoiseIndex_1:NoiseIndex_18)
bbsyr19_noisea<-na.aggregate(bbsyr19_noise)
bbsyr19_noise1<-standardize(array(data=c(unlist(as.data.frame(bbsyr19_noisea[,-1]))),
                                  dim=c(nrow(as.data.frame(bbsyr19_noisea[,-1])),18),
                                  dimnames = list(rownames(as.data.frame(bbsyr19_noisea[,-1])),
                                                  colnames(as.data.frame(bbsyr19_noisea[,-1])))))

bbsyr19_noise1[!is.finite(bbsyr19_noise1)]<-0

bbsyr19_car <-bbsyr19_mlra %>% select(CarIndex_1:CarIndex_18)
bbsyr19_cara<-na.aggregate(bbsyr19_car)
bbsyr19_car1<-standardize(array(data=c(unlist(as.data.frame(bbsyr19_cara[,-1]))),
                                dim=c(nrow(as.data.frame(bbsyr19_cara[,-1])),18),
                                dimnames = list(rownames(as.data.frame(bbsyr19_cara[,-1])),
                                                colnames(as.data.frame(bbsyr19_cara[,-1])))))

bbsyr19_car1[!is.finite(bbsyr19_car1)]<-0

bbsyr21_y<-bbsyr21_mlra %>% select(stops_coun_1:stops_coun_18)
bbsyr21_noise <-bbsyr21_mlra %>% select(NoiseIndex_1:NoiseIndex_18)
bbsyr21_noisea<-na.aggregate(bbsyr21_noise)
bbsyr21_noise1<-standardize(array(data=c(unlist(as.data.frame(bbsyr21_noisea[,-1]))),
                                  dim=c(nrow(as.data.frame(bbsyr21_noisea[,-1])),18),
                                  dimnames = list(rownames(as.data.frame(bbsyr21_noisea[,-1])),
                                                  colnames(as.data.frame(bbsyr21_noisea[,-1])))))

bbsyr21_noise1[!is.finite(bbsyr21_noise1)]<-0

bbsyr21_car <-bbsyr21_mlra %>% select(CarIndex_1:CarIndex_18)
bbsyr21_cara<-na.aggregate(bbsyr21_car)
bbsyr21_car1<-standardize(array(data=c(unlist(as.data.frame(bbsyr21_cara[,-1]))),
                                dim=c(nrow(as.data.frame(bbsyr21_cara[,-1])),18),
                                dimnames = list(rownames(as.data.frame(bbsyr21_cara[,-1])),
                                                colnames(as.data.frame(bbsyr21_cara[,-1])))))


bbsyr21_car1[!is.finite(bbsyr21_car1)]<-0


#########################################################combine all 3 years of data
bbsall3_y<-abind(bbsyr18_y[,-1],bbsyr19_y[,-1],bbsyr21_y[,-1],along=3)
bbsall3_noise<-abind(bbsyr18_noise1,bbsyr19_noise1,bbsyr21_noise1,along=3)
bbsall3_car<-abind(bbsyr18_car1,bbsyr19_car1,bbsyr21_car1,along=3)


#############################processing BBS dataset

#################################################################################################
##################combine both bbs and eBird for modeling
#combining all grids with values either from BBS, ebird or both
# incase of overlap of grids, chosing maximum value

#maximum for BBS
bbs18_max<-bbsyr18_y %>% rowwise() %>% mutate(bmax18=max(stops_coun_1,stops_coun_2,stops_coun_3,stops_coun_4,stops_coun_5,
                                                         stops_coun_6,stops_coun_7,stops_coun_8,stops_coun_9,stops_coun_10,
                                                         stops_coun_11,stops_coun_12,stops_coun_13,stops_coun_14,stops_coun_15,
                                                         stops_coun_16,stops_coun_17,stops_coun_18,na.rm = T))
bbs18_max[bbs18_max==-Inf]<-0
bbs18_max1<-bbs18_max[,c("grid_id_5k","bmax18")]
colnames(bbs18_max1)[2]<-"max18"

bbs19_max<-bbsyr19_y %>% rowwise() %>% mutate(bmax19=max(stops_coun_1,stops_coun_2,stops_coun_3,stops_coun_4,stops_coun_5,
                                                         stops_coun_6,stops_coun_7,stops_coun_8,stops_coun_9,stops_coun_10,
                                                         stops_coun_11,stops_coun_12,stops_coun_13,stops_coun_14,stops_coun_15,
                                                         stops_coun_16,stops_coun_17,stops_coun_18,na.rm = T))
bbs19_max[bbs19_max==-Inf]<-0
bbs19_max1<-bbs19_max[,c("grid_id_5k","bmax19")]
colnames(bbs19_max1)[2]<-"max19"

bbs21_max<-bbsyr21_y %>% rowwise() %>% mutate(bmax21=max(stops_coun_1,stops_coun_2,stops_coun_3,stops_coun_4,stops_coun_5,
                                                         stops_coun_6,stops_coun_7,stops_coun_8,stops_coun_9,stops_coun_10,
                                                         stops_coun_11,stops_coun_12,stops_coun_13,stops_coun_14,stops_coun_15,
                                                         stops_coun_16,stops_coun_17,stops_coun_18,na.rm = T))
bbs21_max[bbs21_max==-Inf]<-0
bbs21_max1<-bbs21_max[,c("grid_id_5k","bmax21")]
colnames(bbs21_max1)[2]<-"max21"
#maximum for BBS

#maximum for eBird

ebd18_max<-ebdyr18_y %>% rowwise() %>% mutate(max18=max(observation_count_1,observation_count_2,observation_count_3,observation_count_4,observation_count_5,
                                                        observation_count_6,observation_count_7,observation_count_8,observation_count_9,observation_count_10,
                                                        observation_count_11,observation_count_12,observation_count_13,observation_count_14,observation_count_15,
                                                        observation_count_16,observation_count_17,observation_count_18,observation_count_19,observation_count_20,
                                                        observation_count_21,observation_count_22,observation_count_23,observation_count_24,observation_count_25,
                                                        observation_count_26,observation_count_27,observation_count_28,observation_count_29,observation_count_30,
                                                        observation_count_31,observation_count_32,observation_count_33,observation_count_34,observation_count_35,
                                                        observation_count_36,observation_count_37,observation_count_38,observation_count_39,observation_count_40,
                                                        observation_count_41,observation_count_42,observation_count_43,observation_count_44,observation_count_45,
                                                        observation_count_46,observation_count_47,observation_count_48,observation_count_49,observation_count_50,na.rm=T))

ebd18_max[ebd18_max==-Inf]<-0
ebd18_max1<-ebd18_max[,c("grid_id_5km","max18")]
colnames(ebd18_max1)[1]<-"grid_id_5k"

ebd19_max<-ebdyr19_y %>% rowwise() %>% mutate(max19=max(observation_count_1,observation_count_2,observation_count_3,observation_count_4,observation_count_5,
                                                        observation_count_6,observation_count_7,observation_count_8,observation_count_9,observation_count_10,
                                                        observation_count_11,observation_count_12,observation_count_13,observation_count_14,observation_count_15,
                                                        observation_count_16,observation_count_17,observation_count_18,observation_count_19,observation_count_20,
                                                        observation_count_21,observation_count_22,observation_count_23,observation_count_24,observation_count_25,
                                                        observation_count_26,observation_count_27,observation_count_28,observation_count_29,observation_count_30,
                                                        observation_count_31,observation_count_32,observation_count_33,observation_count_34,observation_count_35,
                                                        observation_count_36,observation_count_37,observation_count_38,observation_count_39,observation_count_40,
                                                        observation_count_41,observation_count_42,observation_count_43,observation_count_44,observation_count_45,
                                                        observation_count_46,observation_count_47,observation_count_48,observation_count_49,observation_count_50,na.rm=T))

ebd19_max[ebd19_max==-Inf]<-0
ebd19_max1<-ebd19_max[,c("grid_id_5km","max19")]
colnames(ebd19_max1)[1]<-"grid_id_5k"

ebd21_max<-ebdyr21_y %>% rowwise() %>% mutate(max21=max(observation_count_1,observation_count_2,observation_count_3,observation_count_4,observation_count_5,
                                                        observation_count_6,observation_count_7,observation_count_8,observation_count_9,observation_count_10,
                                                        observation_count_11,observation_count_12,observation_count_13,observation_count_14,observation_count_15,
                                                        observation_count_16,observation_count_17,observation_count_18,observation_count_19,observation_count_20,
                                                        observation_count_21,observation_count_22,observation_count_23,observation_count_24,observation_count_25,
                                                        observation_count_26,observation_count_27,observation_count_28,observation_count_29,observation_count_30,
                                                        observation_count_31,observation_count_32,observation_count_33,observation_count_34,observation_count_35,
                                                        observation_count_36,observation_count_37,observation_count_38,observation_count_39,observation_count_40,
                                                        observation_count_41,observation_count_42,observation_count_43,observation_count_44,observation_count_45,
                                                        observation_count_46,observation_count_47,observation_count_48,observation_count_49,observation_count_50,na.rm=T))

ebd21_max[ebd21_max==-Inf]<-0
ebd21_max1<-ebd21_max[,c("grid_id_5km","max21")]
colnames(ebd21_max1)[1]<-"grid_id_5k"
##maximum for eBird

##########combine both dataset
all_18<-rbind(bbs18_max1,ebd18_max1)
all_18a<-all_18 %>% group_by(grid_id_5k) %>% slice(which.max(max18))


all_19<-rbind(bbs19_max1,ebd19_max1)
all_19a<-all_19 %>% group_by(grid_id_5k) %>% slice(which.max(max19))

all_21<-rbind(bbs21_max1,ebd21_max1)
all_21a<-all_21 %>% group_by(grid_id_5k) %>% slice(which.max(max21))

N_all<-cbind(all_18a$max18,all_19a$max19,all_21a$max21)

###################combine both dataset

##################extract bbs and ebird grid details
bbsGrid18<-which(all_18a$grid_id_5k %in% bbs18_max1$grid_id_5k)
bbsGrid19<-which(all_19a$grid_id_5k %in% bbs19_max1$grid_id_5k)
bbsGrid21<-which(all_21a$grid_id_5k %in% bbs21_max1$grid_id_5k)

ebdGrid18<-which(all_18a$grid_id_5k %in% ebd18_max1$grid_id_5k)
ebdGrid19<-which(all_19a$grid_id_5k %in% ebd19_max1$grid_id_5k)
ebdGrid21<-which(all_21a$grid_id_5k %in% ebd21_max1$grid_id_5k)

bbs_grids<-cbind(bbsGrid18,bbsGrid19,bbsGrid21)
ebd_grids<-cbind(ebdGrid18,ebdGrid19,ebdGrid21)
##################extract bbs and ebird grid details
