
library("forecast")
library("MAPA")
library("TStools")
library("smooth")
library("thief")
library("pbmcapply")

##No of cores for parallel
ncore = 18           # no. of cores
set.seed(12345)      # random seed

perd  = "WEEKLY"      # capitalized letters
h.fc  = 13            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 52            # series frequency (1/4/12 for yearly/quarterly/monthly)


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
  ets.bc    = InvBoxCox(es(x.bc, h = h.fc,silent=T)$forecast, lambda = lambda)
  xets.bc   = InvBoxCox(ces(x.bc, h = h.fc,silent=T)$forecast, lambda = lambda)
  #arima.bc  = forecast::forecast(auto.arima(x.bc), h = h.fc, lambda = lambda,biasadj=F)$mean
  theta.bc  = InvBoxCox(forecast(theta(x.bc), h = h.fc)$mean, lambda = lambda)
  #mapa.bc   = InvBoxCox(mapa(x.bc, fh = h.fc, outplot = 0,type="es")$outfor, lambda = lambda)
  thief.bc = tryCatch({
    InvBoxCox(thief(x.bc, h = h.fc,usemodel = "theta")$mean, lambda = lambda)},
    error=function(e){
      theta.bc
    })
  
  
  thief.es <- function(x,h.fc){
  
  ftes <- function(y,h,...){forecast(es(y),h,...)}
    
    thief.es <- tryCatch({
      InvBoxCox(thief(x, h = h.fc,forecastfunction=ftes)$mean, lambda = lambda)},
      error=function(e){
        ets.bc
      })
    
    return(thief.es)
  }
    
    
  thief.es<- ts(thief.es(x.bc,h.fc=h.fc),start=start(ets.bc),frequency = 52)

  
 
  
  if (frequency > 1) {
    naive.f   = snaive(x, h = h.fc)$mean
  } else {
    naive.f   = naive(x, h = h.fc)$mean
  }
  
  
  if (frequency > 1) {
    stl.bc = tryCatch({
      stlfth.bc = stlf(x, forecastfunction = thetaf, h = h.fc, lambda = lambda,biasadj=F)$mean
      
    },error=function(e){
      stlfth.bc = theta.bc
    })
  }  else {
    stlfth.bc = theta.bc
  }
  
  
  f.tbat = tryCatch(tbats(x,use.parallel = F),error=function(e){
    snaive(x)
  })
  
  g.tbat = forecast(f.tbat,h=h.fc)$mean
  
  

  return(cbind(ets.bc = as.vector(ets.bc), xets.bc = as.vector(xets.bc),
               theta.bc = as.vector(theta.bc),stl.bc=as.vector(stl.bc),
               naive.f = as.vector(naive.f),thief.bc=as.vector(thief.bc),thief.es=as.vector(thief.es),
               g.tbat = as.vector(g.tbat)
))
}

system.time(for.weekly.m4 <- pbmclapply(mydata.full.ts, forx, h.fc = h.fc, frequency = freq, mc.cores = ncore))
save(for.weekly.m4, file = "for.weekly.m4.rda")

