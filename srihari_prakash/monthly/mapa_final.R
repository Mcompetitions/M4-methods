
library("MAPA")
library("parallel")
library("thief")
library("pbmcapply")

##No of cores for parallel
ncore = detectCores()-4           # no. of cores
set.seed(12345)      # random seed

perd  = "MONTHLY"  # capitalized letters
h.fc  = 18            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 12            # series frequency (1/4/12 for yearly/quarterly/monthly)



# Create Time Series

load("mydata.full.ts.rda")



#tr <-  sample(mydata.full.ts,5)

## Function to implement Srihari's method
forx.b = function(x, h.fc, frequency = freq) {
  
  lambda    = BoxCox.lambda(na.contiguous(x), method = "guerrero",lower = 0, upper = 1)
  x.bc      = BoxCox(x, lambda)
  
  mapa.bc       = InvBoxCox(mapa(x.bc, fh = h.fc, outplot = 0)$outfor, lambda = lambda)
  thief.bc.ets  = InvBoxCox(thief(x.bc,h=h.fc,usemodel ="ets")$mean, lambda = lambda)
  thief.bc.arm  = InvBoxCox(thief(x.bc,h = h.fc,usemodel ="arima")$mean, lambda = lambda)
  
  return(cbind(mapa.bc = as.vector(mapa.bc), thief.ets = as.vector(thief.bc.ets), thief.arm = as.vector(thief.bc.arm)))
}


#pbmclapply(tr, forx.b, h.fc = h.fc, frequency = freq, mc.cores = ncore)

system.time(for.M4.mapa<- pbmclapply(mydata.full.ts, forx.b, h.fc = h.fc, frequency = freq, mc.cores = ncore))
save(for.M4.mapa, file = paste0("M4_mapa_", perd, "_srihari.rda"))