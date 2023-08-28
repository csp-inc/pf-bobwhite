library(rjags)
library(abind)
set.seed(1234)
load("alldata.Rdata")

jags.data <- list(nCells_bbs=nrow(bbsall3_y),nStops=stops_bbs,
                  nCells_ebd=nrow(ebdall3_y),nSites_ebd=checklist_ebird,nYear=3,
                  y_bbs=bbsall3_y,noise_bbs=bbsall3_noise,car_bbs=bbsall3_car,
                  y_ebd=ebdall3_y,
                  obs_ebd=ebdall3_obs,
                  dur_ebd=ebdall3_dur,
                  eff_ebd=ebdall3_eff,
                  type_ebd=ebdall3_type,
                  pfg_grad1=pfg_grad,
                  tree_grad1=tree_grad,
                  bgr1=bgr,
                  pfg1=pfg,
                  shrub1=shrub,
                  tree1=tree,
                  rowcrop1=rowcrop,
                  energy1=energy,
                  transport1=transport,
                  urban1=urban,
                  fire1=fire,
                  crp1=crp,
                  farmsize1=farmsize,
                  tmax1=tmax,
                  prcp1=prcp,
                  snowdays1=snowdays,
                  aspect1=aspect,
                  slope1=slope,
                  elevation1=elevation,
                  pasture1=pasture,
                  NDVI1=std_ndvi,
                  evergreen1=evergreen,
                  deciduous1=deciduous,
                  water1=water,
                  mixed1=mixed,
                  latitude1=lat,
                  grid_bbs_id=bbs_grids,
                  grid_ebd_id=ebd_grids,
                  region_grid=coded_mlra,
                  lrr=region_lrr,
                  lrr_no=no_lrr,
                  mlra_no=no_mlra,
                  grid_area=2500,BBS_survey_area=50.26548,
                  tot=nrow(N_all))



N_bbs_all<-abind(apply(bbsyr18_y[,-1],c(1,2),function(x) max(x, na.rm=T))+1,
                 apply(bbsyr19_y[,-1],c(1,2),function(x) max(x, na.rm=T))+1,
                 apply(bbsyr21_y[,-1],c(1,2),function(x) max(x, na.rm=T))+1,along=3)
N_bbs_all[is.infinite(N_bbs_all)]<-NA

N_all1<-N_all+50

inits<-function(){
  list(N_overall=N_all1,N_bbs=N_bbs_all,det0=rnorm(1,0.5,0.001),
       det_obs=runif(0.05,1),
       det_eff=rnorm(1,0.04,0.001),
       det_noise=rnorm(1,0.09,0.001),
       det_type=rnorm(1,0.6,0.001),
       det_dur=rnorm(1,0.2,0.001),
       det_car=rnorm(1,0.5,0.001)
  )} 

pars <- c('abun0',
          'mean_abun0','sd_abun0','mean_abun_shrub','sd_abun_shrub','abun_shrub',
          'mean_abun_pfg','sd_abun_pfg','abun_pfg','mean_abun_pfg_grad','sd_abun_pfg_grad','abun_pfg_grad',
          'mean_abun_pasture','sd_abun_pasture','abun_pasture',
          'mean_abun_tree','sd_abun_tree','abun_tree','mean_abun_tree_grad','sd_abun_tree_grad','abun_tree_grad',
          'sd_abun_bgr','mean_abun_bgr','abun_bgr',
          'mean_abun_rowcrop','sd_abun_rowcrop','abun_rowcrop',
          'mean_abun_urban','sd_abun_urban','abun_urban','mean_abun_transport','sd_abun_transport','abun_transport',
          'mean_abun_energy','sd_abun_energy','abun_energy',
          'sd_abun_fire','mean_abun_fire','abun_fire','mean_abun_crp','sd_abun_crp',
          'abun_crp',
          'abun_tmax','abun_tmaxlat','abun_lat','mean_abun_prcp','sd_abun_prcp',
          'abun_prcp','mean_abun_snowdays','sd_abun_snowdays',
          'abun_snowdays','mean_abun_elevation','sd_abun_elevation','abun_elevation',
          'mean_abun_evergreen','sd_abun_evergreen','abun_evergreen',
          'mean_abun_deciduous','sd_abun_deciduous','abun_deciduous',
          'mean_abun_mixed','sd_abun_mixed','abun_mixed',
          'mean_abun_water','sd_abun_water','abun_water',
          'mean_abun_NDVI','sd_abun_NDVI','abun_NDVI',
          'det0','det_noise','det_obs','det_dur','det_eff','det_type','det_car')

n.adapt=200
n.iter =1000
n.burn=200


jm <-jags.model(file="model.txt", jags.data, inits, n.chains=3,n.adapt = n.adapt)
update(jm,n.burn=n.burn)
cs <- coda.samples(jm, pars, thin=1, n.iter=n.iter)
save(cs,file="cs.Rdata")
s<-summary(cs)
save(s,file="s.Rdata")
write.csv(gelman.diag(cs,multivariate = FALSE)$psrf,"gelman1.csv")
