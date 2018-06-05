## Load libraries
library(forecast)
library(thief)
library(opera)


## Load data
# Download it from https://github.com/carlanetto/M4comp2018/tree/master/data
load("M4.RData")
Q_M4 <- Filter(function(l) l$period == "Quarterly", M4)

########################### Training
# The provided data in M4 were splitted into training and out-of-sample sets
# The size of out-of-sample set is equal to the needed forecasts in the competition
# STEP-1: Five models were chosen for MONTHLY forecasts: 2 THIEF models, BATS, TBATS, and ETS
# STEP-2: Aggregate the forecasts from the the models using Polynomial Potential aggregation rule algorithm in "opera" package

fh<-8
fq<-4
series_num<-length(Q_M4)
forecasts<-array(NA,dim=c(5,fh,series_num))

for (i in 1:series_num){

	sze<-Q_M4[[i]]$n-Q_M4[[i]]$h-1
	insampleend<-(sze/fq)+1
	insample <- window(Q_M4[[i]]$x,end=insampleend)

	# THIEF
	forecastsTHIEF1 <-thief(insample, h = fh, comb = "struc", usemodel="ets")$mean
	forecastsTHIEF2 <-thief(insample, h = fh, comb = "mse", usemodel="ets")$mean

	# BATS
	mdlBATS <-bats(insample, biasadj = FALSE)
	forecastsBATS <-forecast(mdlBATS ,h=fh)$mean

	# TBATS
	mdlTBATS <-tbats(insample, biasadj = FALSE)
	forecastsTBATS <-forecast(mdlTBATS ,h=fh)$mean

	# ETS
	mdlETS <- ets(insample)
	forecastsETS <- forecast(mdlETS ,h=fh)$mean

	if(min(forecastsTHIEF1,na.rm=T)<0){
		lgx<-log(insample)
		forecastsTHIEF1 <- exp(thief(lgx, h = fh, comb = "struc", usemodel="ets")$mean)
  	}

  	if(min(forecastsTHIEF2,na.rm=T)<0){
		lgx<-log(insample)
		forecastsTHIEF2 <- exp(thief(lgx, h = fh, comb = "mse", usemodel="ets")$mean)
  	}

  	if(min(forecastsBATS ,na.rm=T)<0){
		lgx<-log(insample)
		ff1<-bats(lgx, biasadj = FALSE)
		forecastsBATS <-exp(forecast(ff1,h=fh)$mean)
  	}

  	if(min(forecastsTBATS ,na.rm=T)<0){
		lgx<-log(insample)
		ff1 <- tbats(lgx, biasadj = FALSE)
		forecastsTBATS <-exp(forecast(ff1,h=fh)$mean)
  	}

  	if(min(forecastsETS ,na.rm=T)<0){
		lgx<-log(insample)
		ff1<-ets(lgx)
		forecastsETS <-exp(forecast(ff1,h=fh)$mean)
  	}  

  	forecasts[1,1:fh,i] <-forecastsTHIEF1 
  	forecasts[2,1:fh,i] <-forecastsTHIEF2
  	forecasts[3,1:fh,i] <-forecastsBATS 
  	forecasts[4,1:fh,i] <-forecastsTBATS 
  	forecasts[5,1:fh,i] <-forecastsETS 
}

OPERA<-NULL
for(i in 1:series_num){
	MLpol0 <- mixture(model = "MLpol", loss.type = "square")
	X <- cbind(THIEF1=forecasts[1,,i],THIEF2=forecasts[2,,i],BATS=forecasts[3,,i],TBATS=forecasts[4,,i],ETS=forecasts[5,,i])

	sze<-Q_M4[[i]]$n-Q_M4[[i]]$h-1
	outsamplestart<-((sze+1)/fq)+1
	outsample <- window(Q_M4[[i]]$x,start=outsamplestart) 

	MLpol <- predict(MLpol0, newexpert = X, newY = outsample, online = TRUE, type="all")
	OPERA[i]<-list(MLpol)
}


########################### M4 QUARTERLY data forecasting
# Forecast using the models
# Aggregate the forecasts from the models based on the weights optained perviously using "opera" package OR you can load it

# load("operaQuarterly.RData")

fforecasts<-array(NA,dim=c(series_num,fh))
mforecasts<-array(NA,dim=c(5,fh,1))

for(i in 1:series_num){
	fname<-Q_M4[[i]]$st

  	# THIEF
  	forecastsTHIEF1 <-thief(Q_M4[[i]]$x, h = fh, comb = "struc", usemodel="ets")$mean
  	forecastsTHIEF2 <-thief(Q_M4[[i]]$x, h = fh, comb = "mse", usemodel="ets")$mean

  	# BATS
  	mdlBATS <-bats(Q_M4[[i]]$x, biasadj = FALSE)
  	forecastsBATS <-forecast(mdlBATS ,h=fh)$mean

  	# TBATS
  	mdlTBATS <-tbats(Q_M4[[i]]$x, biasadj = FALSE)
  	forecastsTBATS <-forecast(mdlTBATS ,h=fh)$mean

  	# ETS
  	mdlETS <- ets(Q_M4[[i]]$x)
  	forecastsETS <- forecast(mdlETS ,h=fh)$mean

  	if(min(forecastsTHIEF1,na.rm=T)<0){
		lgx<-log(Q_M4[[i]]$x)
		forecastsTHIEF1 <- exp(thief(lgx, h = fh, comb = "struc", usemodel="ets")$mean)
  	}

  	if(min(forecastsTHIEF2,na.rm=T)<0){
		lgx<-log(Q_M4[[i]]$x)
		forecastsTHIEF2 <- exp(thief(lgx, h = fh, comb = "mse", usemodel="ets")$mean)
  	}

  	if(min(forecastsBATS ,na.rm=T)<0){
		lgx<-log(Q_M4[[i]]$x)
		ff1<-bats(lgx, biasadj = FALSE)
		forecastsBATS <-exp(forecast(ff1,h=fh)$mean)
  	}

  	if(min(forecastsTBATS ,na.rm=T)<0){
		lgx<-log(Q_M4[[i]]$x)
		ff1 <- tbats(lgx, biasadj = FALSE)
		forecastsTBATS <-exp(forecast(ff1,h=fh)$mean)
  	}

  	if(min(forecastsETS ,na.rm=T)<0){
		lgx<-log(Q_M4[[i]]$x)
		ff1<-ets(lgx)
		forecastsETS <-exp(forecast(ff1,h=fh)$mean)
  	} 

	mforecasts[1,,1]<-forecastsTHIEF1
	mforecasts[2,,1]<-forecastsTHIEF2
	mforecasts[3,,1]<-forecastsBATS 
	mforecasts[4,,1]<-forecastsTBATS 
	mforecasts[5,,1]<-forecastsETS 

	fcast<-numeric(fh)
	for(j in 1:fh){
		fcast[j]<-round(sum(OPERA[[i]]$weights[j,] * mforecasts[1:5,j,1]),4)
	}
	
	fcastNA<-rep(NA, 48-fh)
	strfcast<-noquote(paste(c(fname,fcast,fcastNA),collapse = ","))
	fforecasts[i,]<-fcast	

	if(i==1){
		write.table(strfcast, file = "D:\\M4\\Quarterly.csv", append = F,quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	} else{
		write.table(strfcast, file = "D:\\M4\\Quarterly.csv", append = T, quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	}
}