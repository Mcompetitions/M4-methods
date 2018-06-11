## Load libraries
library(forecast)
library(thief)
library(opera)

## Load data
# Download it from https://github.com/carlanetto/M4comp2018/tree/master/data
load("M4.RData")
H_M4 <- Filter(function(l) l$period == "Hourly", M4)


########################### Training
# The provided data in M4 were splitted into training and out-of-sample sets
# The size of out-of-sample set is equal to the needed forecasts in the competition
# STEP-1: Four models were chosen for HOURLY forecasts: 3 THIEF models and TBATS
# STEP-2: Aggregate the forecasts from the the models using Polynomial Potential aggregation rule algorithm in "opera" package 

fh<-48
fq<-24
series_num<-length(H_M4)
forecasts<-array(NA,dim=c(4,fh,series_num))

for (i in 1:series_num){

	sze<-H_M4[[i]]$n-H_M4[[i]]$h-1
	insampleend<-(sze/fq)+1
	insample <- window(H_M4[[i]]$x,end=insampleend)

	# THIEF
	forecastsTHIEF1 <-thief(insample, h = fh, comb = "struc", usemodel="ets")$mean
	forecastsTHIEF2 <-thief(insample, h = fh, comb = "struc", usemodel="arima")$mean
	forecastsTHIEF3 <-thief(insample, h = fh, comb = "struc", usemodel="snaive")$mean

	# TBATS
	mdlTBATS <-tbats(insample, biasadj = FALSE)
	forecastsTBATS <-forecast(mdlTBATS ,h=fh)$mean

	if(min(forecastsTHIEF1,na.rm=T)<0){
		lgx<-log(insample)
		forecastsTHIEF1 <- exp(thief(lgx, h = fh, comb = "struc", usemodel="ets")$mean)
  	}

  	if(min(forecastsTHIEF2,na.rm=T)<0){
		lgx<-log(insample)
		forecastsTHIEF2 <- exp(thief(lgx, h = fh, comb = "struc", usemodel="arima")$mean)
  	}

  	if(min(forecastsTHIEF3,na.rm=T)<0){
		lgx<-log(insample)
		forecastsTHIEF3 <- exp(thief(lgx, h = fh, comb = "struc", usemodel="snaive")$mean)
  	}

  	if(min(forecastsTBATS ,na.rm=T)<0){
		lgx<-log(insample)
		ff1 <- tbats(lgx, biasadj = FALSE)
		forecastsTBATS <-exp(forecast(ff1,h=fh)$mean)
  	}

	forecasts[1,1:fh,i] <-forecastsTHIEF1 
	forecasts[2,1:fh,i] <-forecastsTHIEF2
	forecasts[3,1:fh,i] <-forecastsTHIEF3
	forecasts[4,1:fh,i] <-forecastsTBATS 
}

OPERA<-NULL
for(i in 1:series_num){
	MLpol0 <- mixture(model = "MLpol", loss.type = "square")
	X <- cbind(mdl1=forecasts[1,,i],mdl2=forecasts[2,,i],mdl3=forecasts[3,,i],mdl4=forecasts[4,,i])

   	sze<-H_M4[[i]]$n-H_M4[[i]]$h-1
   	outsamplestart<-((sze+1)/fq)+1
   	outsample <- window(H_M4[[i]]$x,start=outsamplestart)

	MLpol <- predict(MLpol0, newexpert = X, newY = outsample, online = TRUE, type="all")
	OPERA[i]<-list(MLpol)
}


########################### M4 HOURLY data forecasting
# Forecast using the models
# Aggregate the forecasts from the models based on the weights optained perviously using "opera" package OR you can load it

# load("operaHourly.RData")

fforecasts<-array(NA,dim=c(series_num,fh))
mforecasts<-array(NA,dim=c(4,fh,1))

for(i in 1:series_num){
	fname<-H_M4[[i]]$st

  	# THIEF
  	forecastsTHIEF1 <-thief(H_M4[[i]]$x, h = fh, comb = "struc", usemodel="ets")$mean
  	forecastsTHIEF2 <-thief(H_M4[[i]]$x, h = fh, comb = "struc", usemodel="arima")$mean
  	forecastsTHIEF3 <-thief(H_M4[[i]]$x, h = fh, comb = "struc", usemodel="snaive")$mean

  	# TBATS
  	mdlTBATS <-tbats(H_M4[[i]]$x, biasadj = FALSE)
  	forecastsTBATS <-forecast(mdlTBATS ,h=fh)$mean

  	if(min(forecastsTHIEF1 ,na.rm=T)<0){
		lgx<-log(H_M4[[i]]$x)
		forecastsTHIEF1 <- exp(thief(lgx, h = fh, comb = "struc", usemodel="ets")$mean)
  	}

  	if(min(forecastsTHIEF2 ,na.rm=T)<0){
		lgx<-log(H_M4[[i]]$x)
		forecastsTHIEF2 <- exp(thief(lgx, h = fh, comb = "struc", usemodel="arima")$mean)
  	}

  	if(min(forecastsTHIEF3 ,na.rm=T)<0){
		lgx<-log(H_M4[[i]]$x)
		forecastsTHIEF2 <- exp(thief(lgx, h = fh, comb = "struc", usemodel="snaive")$mean)
  	}

  	if(min(forecastsTBATS ,na.rm=T)<0){
		lgx<-log(H_M4[[i]]$x)
		ff1 <- tbats(lgx, biasadj = FALSE)
		forecastsTBATS <-exp(forecast(ff1,h=fh)$mean)
  	}

	mforecasts[1,,1]<-forecastsTHIEF1
	mforecasts[2,,1]<-forecastsTHIEF2
	mforecasts[3,,1]<-forecastsTHIEF3 
	mforecasts[4,,1]<-forecastsTBATS 

	fcast<-numeric(fh)
	for(j in 1:fh){
		fcast[j]<-round(sum(OPERA[[i]]$weights[j,] * mforecasts[1:4,j,1]),4)
	}
	
	fcastNA<-rep(NA, 48-fh)
	strfcast<-noquote(paste(c(fname,fcast,fcastNA),collapse = ","))
	fforecasts[i,]<-fcast	

	if(i==1){
		write.table(strfcast, file = "D:\\M4\\Hourly.csv", append = F,quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	} else{
		write.table(strfcast, file = "D:\\M4\\Hourly.csv", append = T, quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	}
}
