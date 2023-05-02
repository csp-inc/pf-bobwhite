library(rjags)
library(R2jags)

### Set working directory # change as necessary
setwd("C:/Users/patrick/Downloads/materialsforrunningjagscodeonvirtualmachine")

### Load test data # change as necessary
load("testCode.Rdata")

### Create jags data as list 
jags.data<-list(nCells=nrow(bbs16_5_wide_y),
                nSites=50,
                y1=y_bbs16,
                noise_bbs=noise_bbs16)

### Modify data (I don't know what is happening here )
N1<-apply(jags.data$y1, c(1), function(x) max(x,na.rm=T)) + 1
N1[!is.finite(N1)]<-0

### Set inits 

inits<-function(){
  list(N1=N1
  )} 

### Set parameters to save 
pars <- c('abun0','mean_alpha0','det0','det1')

### Set path to model file 

model.file <- "C:/Users/patrick/Downloads/materialsforrunningjagscodeonvirtualmachine/testmodel.txt"

### Show file to confirm
file.show(model.file)

### Confirm model can run normally (not in parallel) - seems to work ok 
jagsfit <- jags(data=jags.data, 
                inits=inits,
                pars,
                n.iter=10, 
                model.file=model.file, 
                n.chains=3, 
                n.adapt=n.adapt)

### Try running in parallel using R2jags - not working yet 
parallel_jags <- 
  jags.parallel(
    data = jags.data,
    model.file = model.file,
    parameters.to.save = pars,
    n.chains = 3, 
    n.iter = 100, 
    n.burnin = 10,
    jags.seed = 1234
  )
