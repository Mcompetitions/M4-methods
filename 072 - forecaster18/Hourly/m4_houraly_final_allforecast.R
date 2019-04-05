setwd("/home/rstudio2/m4")

library("forecast")
library("MAPA")
library("nnfor")
library("thief")
library("pbmcapply")


##No of cores for parallel
ncore = detectCores()-4           # no. of cores


set.seed(12345)      # random seed

perd  = "HOURLY"    # capitalized letters
h.fc  = 48            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 168            # series frequency (1/4/12 for yearly/quarterly/monthly)


## Extract raw data

MyData <- read.csv(file="Hourly-train.csv", header=TRUE, sep=",")

# Create List

mydata.list <- setNames(split(MyData[,-1],seq(nrow(MyData))),MyData[,1])
mydata.list <-  mclapply(mydata.list, function(x) x[!is.na(x)],mc.cores = ncore)

# Create Time Series

mydata.full.ts <- mclapply(mydata.list,function(x) ts(as.vector(t(x)),frequency = freq),mc.cores = ncore)


mydata.full.ts_207 <- mydata.full.ts[1:207]
mydata.full.ts_414 <- mydata.full.ts[208:414]

## Make Prediction

forx = function(x, h.fc, frequency = freq, transform = FALSE) {
  
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
  
  # Forecast
  #Exposmooth
  k <- ges(x.bc)
  kf <- (forecast(k,h=h.fc))
  
  #Auto.arima
  #z <- forecast(auto.arima(x.bc),h=48)$mean
  
  #arimathief
  m <- thief(x.bc,h = h.fc,usemodel = "arima")
  
  #naive
  n <- thief(as.ts(x.bc,frequency=168),h = 48,usemodel = "snaive")
  
  #els
  nn <- elm(x.bc)
  f.nn <- forecast(nn,h=h.fc)
  
  #mlp
  mlpm <- mlp(x.bc)
  f.mlp <- forecast(mlpm,h=h.fc)
  
  #dshw
  iu <- msts(x.bc,c(24,168))
  decomp <- dshw(iu)
  ui <- forecast(decomp,h=h.fc)$mean
  
  #tbats
  ty <- tbats(x.bc,use.box.cox = T,use.parallel = F)
  ty.f <- (forecast(ty,h=h.fc))$mean
  
  agg <- cbind( ges=kf$forecast[1:h.fc],arima=m$mean[1:h.fc],
                naive=n$mean[1:h.fc],tbats = ty.f[1:h.fc],
                dshw = ui[1:h.fc],elm = f.nn$mean,mlp=f.mlp$mean
  )
  
  med <- apply(agg,1, median, na.rm = TRUE)
  men <- rowMeans(agg)
  
  med.noelm <- apply(agg[,c("ges","arima","naive","tbats","dshw")],1, median, na.rm = TRUE)
  men.noelm <- rowMeans(agg[,c("ges","arima","naive","tbats","dshw")])
  
  
  
  return(cbind (ges=kf$forecast[1:h.fc],
                arima=m$mean[1:h.fc],naive=n$mean[1:h.fc],tbats = ty.f[1:h.fc],
                dshw = ui[1:h.fc],elm = f.nn$mean,mlp=f.mlp$mean,
                med = med, men=men,
                med.noelm = med.noelm, men.noelm = men.noelm
  )
  )
}


## Parallelized Forecast (12 mins)

system.time(for.hm4_207 <- pbmclapply(mydata.full.ts_207, forx, h.fc = h.fc, frequency = freq, transform = FALSE, mc.cores = ncore))

save(for.hm4_207, file = paste0("M4_", perd, "207_srihari.rda"))
