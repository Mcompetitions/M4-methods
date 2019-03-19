

##No of cores for parallel
ncore = detectCores() - 4           # no. of cores
set.seed(12345)      # random seed

perd  = "MONTHLY"  # capitalized letters
h.fc  = 18            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 12            # series frequency (1/4/12 for yearly/quarterly/monthly)

## Extract raw data

load("mydata.full.ts.rda")

mydata.full.ts_10 <- mydata.full.ts[(1:10000)]
mydata.full.ts_20 <- mydata.full.ts[(10001:20000)]
mydata.full.ts_30 <- mydata.full.ts[(20001:30000)]
mydata.full.ts_40 <- mydata.full.ts[(30001:40000)]
mydata.full.ts_48 <- mydata.full.ts[(40001:48000)]

#tr <- sample(mydata.full.ts_10,5)

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

#pbmclapply(tr, forx.b, h.fc = h.fc, mc.cores = ncore)



system.time(M4_bg_ets_10 <- pbmclapply(mydata.full.ts_10, forx.b, h.fc = h.fc, mc.cores = ncore))
save(M4_bg_ets_10, file = paste0("M4_bg_ets_10", perd, ".rda"))

system.time(M4_bg_ets_20 <- pbmclapply(mydata.full.ts_20, forx.b, h.fc = h.fc, mc.cores = ncore))
save(M4_bg_ets_20, file = paste0("M4_bg_ets_20", perd, ".rda"))

system.time(M4_bg_ets_30 <- pbmclapply(mydata.full.ts_30, forx.b, h.fc = h.fc, mc.cores = ncore))
save(M4_bg_ets_30, file = paste0("M4_bg_ets_30", perd, ".rda"))

system.time(M4_bg_ets_40 <- pbmclapply(mydata.full.ts_40, forx.b, h.fc = h.fc, mc.cores = ncore))
save(M4_bg_ets_40, file = paste0("M4_bg_ets_40", perd, ".rda"))

system.time(M4_bg_ets_48 <- pbmclapply(mydata.full.ts_48, forx.b, h.fc = h.fc, mc.cores = ncore))
save(M4_bg_ets_48, file = paste0("M4_bg_ets_48", perd, ".rda"))

M4_b48_ets <- c(M4_bg_ets_10,M4_bg_ets_20,M4_bg_ets_30,M4_bg_ets_40,M4_bg_ets_48)


save(M4_b48_ets, file = paste0("M4_b48_ets_", perd, ".rda"))