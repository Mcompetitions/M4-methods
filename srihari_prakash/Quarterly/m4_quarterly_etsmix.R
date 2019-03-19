library("forecast")
library("MAPA")
library("TStools")
library("smooth")
library("pbmcapply")

##No of cores for parallel
ncore = detectCores()-4           # no. of cores
set.seed(12345)      # random seed

perd  = "QUARTERLY"  # capitalized letters
h.fc  = 8            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 4            # series frequency (1/4/12 for yearly/quarterly/monthly)

## Load dataset

load("mydata.full.ts.rda")


## Function to implement Srihari's method
forx = function(x, h.fc, frequency = freq, transform = TRUE) {
  
  #x = tsclean(x)
  
  if(transform & min(x, na.rm = TRUE) >= 0) {
    lambda    = BoxCox.lambda(na.contiguous(x), method = "guerrero", 
                              lower = 0, upper = 1)
    x.bc      = BoxCox(x, lambda)
  } else {
    lambda    = NULL
    x.bc      = x
    transform = FALSE
  }
  
  # Forecast with Box-Cox transformation
  ets.bc    = tryCatch({forecast::forecast(ets(x.bc), h = h.fc, lambda = lambda,biasadj=F)$mean},
                       error=function(e){snaive(x, h = h.fc)$mean})
  arima.bc  = tryCatch({forecast::forecast(auto.arima(x.bc), h = h.fc, lambda = lambda,biasadj=F)$mean},
                       error=function(e){snaive(x, h = h.fc)$mean})
  theta.bc  = tryCatch({InvBoxCox(forecast(theta(x.bc), h = h.fc)$mean, lambda = lambda)},
                       error=function(e){snaive(x, h = h.fc)$mean})
  etsd.bc   = tryCatch({forecast::forecast(ets(x.bc, damped = TRUE), h=h.fc, lambda = lambda,biasadj=F)$mean},
                       error=function(e){snaive(x, h = h.fc)$mean})
  
  
  if (frequency > 1) {
    stl.bc = tryCatch({
      stlf.bc   = forecast(stlm(x.bc), h = h.fc, lambda = lambda,biasadj=F)$mean
      stlfar.bc = forecast(stlm(x.bc,  method = "arima"), h = h.fc, lambda = lambda,biasadj=F)$mean
      stlfth.bc = stlf(x, forecastfunction = thetaf, h = h.fc, lambda = lambda,biasadj=F)$mean
      cbind(stlf.bc,stlfar.bc,stlfth.bc)
    },error=function(e){
      stlf.bc   = ets.bc
      stlfar.bc = arima.bc
      stlfth.bc = theta.bc
      cbind(stlf.bc,stlfar.bc,stlfth.bc)
    })
  }  else {
    stlf.bc   = NA
    stlfar.bc = NA
    stlfth.bc = NA
  }
  
  if (frequency > 1) {
    naive.f   = snaive(x, h = h.fc)$mean
  } else {
    naive.f   = naive(x, h = h.fc)$mean
  }
  
  return(cbind(ets.bc = as.vector(ets.bc), arima.bc = as.vector(arima.bc),
               theta.bc = as.vector(theta.bc), etsd.bc = as.vector(etsd.bc),naive.f = as.vector(naive.f), 
               as.data.frame(stl.bc)))
}



# Parallelized Forecast (12 mins)

system.time(for.m.eats <- pbmclapply(mydata.full.ts, forx, h.fc = h.fc, frequency = freq, mc.cores = ncore))
save(for.m.eats, file = paste0("q_eats_", perd, "_m4.rda"))



# Htheta

source("hybrid_theta.R")

frequency = freq

system.time(for.Hytheta <- pbmclapply(mydata.full.ts, Forecast_ThetaSm, h = h.fc, mc.cores = ncore))

save(for.Hytheta, file = "for.Htheta.rda")


