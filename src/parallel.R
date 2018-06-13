# =====================================================================
# Cockpit - here you can setup your run
# =====================================================================

# The number of forecasts required by each method is 
# 6 for yearly data, 
# 8 for quarterly, 
# 18 for monthly, 
# 13 for weekly, 
# 14 for daily and 
# 48 for hourly. 

# D=1,H=2,M=3,Q=4,W=5,Y=6

fileIndex <- 1        # select data file
freqName <- "D"       # name-pattern for the output-file
forecastLength <- 14  # forecast horizon

files <- dir("/path/to/M4_data",full.names=T)

ts_range <- c(1:300)


# =====================================================================


# the r packages I need:
library(stats)
library(parallel) # I calculated on e google cloud server.
library(compiler)

# prepare functions
source("functions.R")
smape_cal <- cmpfun(smape_cal)
mase_cal <- cmpfun(mase_cal)
arima <- cmpfun(arima)
LjungBoxP <- cmpfun(LjungBoxP)
arimasim <- cmpfun(arimasim)
arimastat <- cmpfun(arimastat)
cv <- cmpfun(cv)
arimasimCV <- cmpfun(arimasimCV)
pred <- cmpfun(pred)


# read data
#info <- read.csv("M4-info.csv")
d <- read.csv(files[fileIndex],header = T)
rownames(d) <- d[,1]
d <- d[,-1]
d <- t(d)
dim(d)
d <- rbind(1:ncol(d),d) # add index to series
d <- as.list(data.frame(d)) # parallelize needs a list

# prepare cluster
cl <- makeCluster(detectCores()) # use all cores on the cpu :-)
clusterExport(cl=cl, varlist=ls())
clusterEvalQ(cl, {
  library(stats)
})


# do prediction in parallel
start_time <- Sys.time()
clusterApplyLB(cl,x=d[ts_range],fun=pred,fcst=forecastLength,typ=freqName) # this is where the action is....
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)

# =====================================================================
