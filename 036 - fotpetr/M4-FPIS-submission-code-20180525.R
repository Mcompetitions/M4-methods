library(forecast)
library(smooth)
library(forecTheta)
library(foreach)
library(doSNOW)
cl = registerDoSNOW(makeCluster(4, type = "SOCK"))

# Datasets sizes
tsns = c(23000, 24000, 48000, 359, 4227, 414)

# *** SELECT WHICH SERIES ***

# The below will produce forecasts for the first 10 series of each frequency
ids = array(NA, c(2, 6*12))
ids[1,] = rep(1:6, each=12)
ids[2,] = rep(12:1, 6)

# # The below will produce forecasts for the 13th yearly series, the 45th and the 10001th quarterly, the 30102th monthly and the 11th weekly
# ids = array(NA, c(2, 5))
# ids[1,] = c(1,2,2,3,4)
# ids[2,] = c(13,45,10001,30102,11)

# # The below will produce forecasts for all series
# ids = array(NA, c(2,100000))
# ids[1,] = c(rep(1,tsns[1]),rep(2,tsns[2]),rep(3,tsns[3]),rep(4,tsns[4]),rep(5,tsns[5]),rep(6,tsns[6]))
# ids[2,] = c(1:tsns[1],1:tsns[2],1:tsns[3],1:tsns[4],1:tsns[5],1:tsns[6])

# # The below will produce forecasts for Y and Q series
# ids = array(NA, c(2, sum(tsns[1:2])))
# ids[1,] = c(rep(1, tsns[1]), rep(2, tsns[2]))
# ids[2,] = c(1:tsns[1], 1:tsns[2])

# # The below will produce forecasts for M series
# ids = array(NA, c(2, sum(tsns[3])))
# ids[1,] = c(rep(3, tsns[3]))
# ids[2,] = c(1:tsns[3])

# # The below will produce forecasts for W, D, and H series
# ids = array(NA, c(2, sum(tsns[4:6])))
# ids[1,] = c(rep(4, tsns[4]), rep(5, tsns[5]), rep(6, tsns[6]))
# ids[2,] = c(1:tsns[4], 1:tsns[5], 1:tsns[6])

# *** END OF SELECT WHICH SERIES ***

# Define structures for all forecasts and prediction intervals
datasets = sort(unique(ids[1,]))
the_names = list(c(if (any(datasets == 1)){paste0("Y", sort((ids[2,][ids[1,]==1])))}, 
                   if (any(datasets == 2)){paste0("Q", sort((ids[2,][ids[1,]==2])))}, 
                   if (any(datasets == 3)){paste0("M", sort((ids[2,][ids[1,]==3])))}, 
                   if (any(datasets == 4)){paste0("W", sort((ids[2,][ids[1,]==4])))}, 
                   if (any(datasets == 5)){paste0("D", sort((ids[2,][ids[1,]==5])))}, 
                   if (any(datasets == 6)){paste0("H", sort((ids[2,][ids[1,]==6])))}),
                 paste0("F", 1:48))
all_ffcs = array(NA, c(dim(ids)[2], 48), dimnames = the_names)
all_lower = array(NA, c(dim(ids)[2], 48), dimnames = the_names)
all_upper = array(NA, c(dim(ids)[2], 48), dimnames = the_names)

for (d in datasets){
  
  # Read dataset (produced by the separate R script)
  if (d==1){
    frequency = 1; horizon = 6; load("Yearly.RData")
  } else if (d==2){
    frequency = 4; horizon = 8; load("Quarterly.RData")
  } else if (d==3){
    frequency = 12; horizon = 18; load("Monthly.RData")
  } else if (d==4){
    frequency = 52; horizon = 13; load("Weekly.RData")
  } else if (d==5){
    frequency = 7; horizon = 14; load("Daily.RData")
  } else if (d==6){
    frequency = 168; horizon = 48; load("Hourly.RData")
  }
  tsn = length(series)
  
  # Parallel processing of series
  all_fcs <- foreach (tsi=sort((ids[2,][ids[1,]==d])), .combine='rbind', .packages=c('forecast', 'forecTheta', 'smooth')) %dopar% {
    
    # Record progress
    write(paste(d, tsi), file = paste("progress.txt"), append=FALSE)
    
    # Retrieve series
    y = ts(as.vector(series[[tsi]]$x), frequency = frequency)
    
    # Define structures to hold forecasts and prediction intervals for one series
    fcs = array(NA, c(4, horizon))
    pis = array(NA, c(4, horizon, 2))
    
    # ETS
    if (frequency(y) > 24){
      # condition for W and H
      fcs1 = es(y, h=horizon, intervals="p", level=0.95)
      fcs[1,] = as.vector(fcs1$forecast)
    } else {
      fcs1 = forecast(ets(y), h=horizon, level=95)
      fcs[1,] = as.vector(forecast(fcs1, h=horizon)$mean)
    }
    pis[1,,1] = fcs1$lower
    pis[1,,2] = fcs1$upper
    
    # CES
    fcs2 = auto.ces(y, h=horizon, intervals="p", level=0.95)
    fcs[2,] = as.vector(fcs2$forecast)
    pis[2,,1] = fcs2$lower
    pis[2,,2] = fcs2$upper
    
    # AutoARIMA
    fcs3 = forecast(auto.arima(y), h=horizon, level=95)
    fcs[3,] = as.vector(forecast(fcs3, h=horizon)$mean)
    pis[3,,1] = fcs3$lower
    pis[3,,2] = fcs3$upper
    
    # DOTM
    if (length(as.vector(y)) > 5000){
      # condition for D2047, D2194 and D4099
      fcs4 = dotm(ts(tail(as.vector(y), 5000), frequency = frequency(y)), h=horizon, level=95)
    } else {
      fcs4 = dotm(y, h=horizon, level=95)
    }
    fcs[4,] = as.vector(fcs4$mean)
    pis[4,,1] = fcs4$lower
    pis[4,,2] = fcs4$upper

    # bind forecasts and prediction intervals in a singe vector
    fcspis = c(apply(fcs, 2, median), apply(pis[,,1], 2 ,median), apply(pis[,,2], 2 ,median))
    
    # set negative values to zero
    fcspis[fcspis < 0] = 0
    
    fcspis

  }

  all_ffcs[(sum(ids[1,]<d)+1):(sum(ids[1,]<=d)), 1:horizon] = all_fcs[,1:horizon]
  all_lower[(sum(ids[1,]<d)+1):(sum(ids[1,]<=d)), 1:horizon] = all_fcs[,(horizon+1):(2*horizon)]
  all_upper[(sum(ids[1,]<d)+1):(sum(ids[1,]<=d)), 1:horizon] = all_fcs[,(2*horizon+1):(3*horizon)]
  
}

# Save forecasts and intervals in csv files
write.csv(all_ffcs, "all-ffcs.csv")
write.csv(all_lower, "all-lower.csv")
write.csv(all_upper, "all-upper.csv")