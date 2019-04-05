

library("forecast")
library("parallel")

##No of cores for parallel
ncore = detectCores() - 4           # no. of cores
set.seed(12345)      # random seed

perd  = "QUARTERLY"  # capitalized letters
h.fc  = 8            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 4            # series frequency (1/4/12 for yearly/quarterly/monthly)

## Extract raw data

load("mydata.full.ts.rda")

mydata.full.ts_8 <- mydata.full.ts[(1:8000)]
mydata.full.ts_16 <- mydata.full.ts[(8001:16000)]
mydata.full.ts_24 <- mydata.full.ts[(16001:24000)]


#tr <- sample(mydata.full.ts_8,5)

## Function to implement Srihari's method
forx.b = function(x, h.fc) {
  
  
  model = tryCatch({
    baggedModel(x)
  },error=function(e){
    ets(x)
  })
  
  f.mod = if (class(model)=="baggedModel") {
    forecast(model,h=h.fc)$median} else {forecast(model,h=h.fc)$mean}
  
  method = class(model)
  
  return(list(f.mod = f.mod, method=method))
}



system.time(M4_bg_ets_8 <- pbmclapply(mydata.full.ts_8, forx.b, h.fc = h.fc, mc.cores = ncore))
save(M4_bg_ets_8, file = paste0("M4_bg_ets_8", perd, ".rda"))

system.time(M4_bg_ets_16 <- pbmclapply(mydata.full.ts_16, forx.b, h.fc = h.fc, mc.cores = ncore))
save(M4_bg_ets_16, file = paste0("M4_bg_ets_16", perd, ".rda"))

system.time(M4_bg_ets_24 <- pbmclapply(mydata.full.ts_24, forx.b, h.fc = h.fc, mc.cores = ncore))
save(M4_bg_ets_24, file = paste0("M4_bg_ets_24", perd, ".rda"))



M4_b24_ets <- c(M4_bg_ets_8,M4_bg_ets_16,M4_bg_ets_24)


save(M4_b24_ets, file = paste0("M4_b48_ets_", perd, ".rda"))