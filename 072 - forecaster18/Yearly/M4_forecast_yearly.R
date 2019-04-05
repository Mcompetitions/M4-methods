
library("forecast")
library("MAPA")
library("TStools")
library("smooth")
library("pbmcapply")



load("mydata.full.ts.rda")

## Make Prediction


set.seed(12345)      # random seed

ncore = 18
perd  = "YEARLY"  # capitalized letters
h.fc  = 6            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 1            # series frequency (1/4/12 for yearly/quarterly/monthly)

## Function to implement Srihari's method
forx = function(x, h.fc, frequency = 1, transform = TRUE) {
  
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
  ets.bc    = forecast::forecast(ets(x.bc), h = h.fc, lambda = lambda)$mean
  arima.bc  = forecast::forecast(auto.arima(x.bc), h = h.fc, lambda = lambda)$mean
  etsd.bc   = forecast::forecast(ets(x.bc, damped = TRUE), h=h.fc, lambda = lambda)$mean
  mapa.bc   = InvBoxCox(mapa(x.bc, fh = h.fc, outplot = 0)$outfor, lambda = lambda)
  theta.bc  = tryCatch({InvBoxCox(forecast(theta(x.bc), h = h.fc)$mean, lambda = lambda)},error=function(e){
    etsd.bc
  })
  
  
  if (frequency > 1) {
    naive.f   = snaive(x, h = h.fc)$mean
  } else {
    naive.f   = naive(x, h = h.fc)$mean
  }
  
  
    return(cbind(ets.bc = as.vector(ets.bc), arima.bc = as.vector(arima.bc),
               theta.bc = as.vector(theta.bc), etsd.bc = as.vector(etsd.bc), 
               mapa.bc = as.vector(mapa.bc), naive.f = as.vector(naive.f)
               
               ))
}


## Parallelized Forecast (12 mins)

system.time(for.eats <- pbmclapply(mydata.full.ts, forx, h.fc = h.fc, frequency = freq, mc.cores = ncore))

save(for.eats, file = paste0("M4_", perd, "_srihari.rda"))


## Hybrid Theta

source("hybrid_theta.R")

frequency = freq

system.time(for.Hytheta <- pbmclapply(mydata.full.ts, Forecast_ThetaSm, h = h.fc, mc.cores = ncore))

save(for.Hytheta, file = "for.Htheta.rda")


