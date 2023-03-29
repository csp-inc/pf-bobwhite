library(tidyverse)
library(Hmisc)


rndm_covs <- read_csv("/Users/patrickfreeman-csp/Downloads/randomPtsCovs.csv")

keeps <- c("Ag_5km", "Urban_5km", "Transport_5km", "Energy_5km", "2017_PFG_5km",
           "2017_SHR_5km", "2017_TRE_5km", "2017_BGR_5km", "2017_AFG_5km",
           "tmax_17_5km", "tmin_17_5km", "prcp_17_5km", "swe_17_5km", "snodays_17",
           "avg_farm_size_17", "prop_crp_acres_20", "system:index")

prep <- rndm_covs %>%
  dplyr::select(any_of(keeps)) %>%
  dplyr::select(-`system:index`)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res2<-rcorr(as.matrix(prep))
View(flattenCorrMatrix(res2$r, res2$P))
