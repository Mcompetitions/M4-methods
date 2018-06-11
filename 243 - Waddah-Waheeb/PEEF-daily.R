## Load libraries
library(forecast)
library(opera)


## Load daily data. 
# Download it from https://github.com/carlanetto/M4comp2018/tree/master/data
load("M4.RData")
D_M4 <- Filter(function(l) l$period == "Daily", M4)


########################### Training
# The provided data in M4 were split into training and out-of-sample sets
# The size of out-of-sample set is equal to the needed forecasts in the competition
# STEP-1: Three models were chosen for daily forecasts: ETS, BATS and Bagged ETS
# STEP-2: Aggregate the forecasts from the three models using Polynomial Potential aggregation rule algorithm in "opera" package 

series_num<-length(D_M4)
fh<-14
forecasts<-array(NA,dim=c(3,fh,series_num))

for (i in 1:series_num){

	insampleend=D_M4[[i]]$n-fh

	insample <- window(D_M4[[i]]$x,end=insampleend)

	# ETS
	mdlETS <- ets(insample)
	forecastsETS <- forecast(mdlETS ,h=fh)$mean

	# BATS
	mdlBATS <-bats(insample, biasadj = FALSE)
	forecastsBATS <-forecast(mdlBATS ,h=fh)$mean

	# baggedETS
	mdlbaggedETS <-baggedETS(insample) 
	forecastsbaggedETS<-forecast(mdlbaggedETS ,h=fh)$mean
  
	forecasts[1,1:fh,i] <-forecastsETS
	forecasts[2,1:fh,i] <-forecastsBATS
	forecasts[3,1:fh,i] <-forecastsbaggedETS
}

OPERA<-NULL
for(i in 1:series_num){
	MLpol0 <- mixture(model = "MLpol", loss.type = "square")
	X <- cbind(ETS=forecasts[1,,i], BATS=forecasts[2,,i], BAGGEDETS=forecasts[3,,i])

	insampleend=D_M4[[i]]$n-fh
	outsample <- window(D_M4[[i]]$x,start=insampleend+1) 

	MLpol <- predict(MLpol0, newexpert = X, newY = outsample, online = TRUE, type="all")
	OPERA[i]<-list(MLpol)
}



########################### M4 daily data forecasting
# Forecast using the three models
# Aggregate the forecasts from the three models based on the weights optained perviously using "opera" package OR you can load it

# load("operaDaily.RData")

fforecasts<-array(NA,dim=c(series_num,fh))
mforecasts<-array(NA,dim=c(3,fh,1))
for(i in 1:series_num){
	fname<-D_M4[[i]]$st

	# ETS
	mdlETS <- ets(D_M4[[i]]$x)
	forecastsETS <- forecast(mdlETS ,h=fh)$mean

	# BATS
	mdlBATS <-bats(D_M4[[i]]$x, biasadj = FALSE)
	forecastsBATS <-forecast(mdlBATS ,h=fh)$mean

	# baggedETS
	mdlbaggedETS <-baggedETS(D_M4[[i]]$x) 
	forecastsbaggedETS<-forecast(mdlbaggedETS ,h=fh)$mean

	if(min(forecastsETS)<0){ 
		lgx<-log(D_M4[[i]]$x)
		mdlETS <- ets(lgx)
		forecastsETS <- exp(forecast(mdlETS ,h=fh)$mean)
		print(paste("ETS",i))	
	}
	if(min(forecastsBATS)<0){ 
		lgx<-log(D_M4[[i]]$x)
		mdlBATS <-bats(lgx, biasadj = FALSE)
		forecastsBATS <- exp(forecast(mdlBATS ,h=fh)$mean)
		print(paste("BATS",i))
	}
	if(min(forecastsbaggedETS)<0){ 
		lgx<-log(D_M4[[i]]$x)
		mdlbaggedETS <-baggedETS(lgx) 
		forecastsbaggedETS<- exp(forecast(mdlbaggedETS ,h=fh)$mean)
		print(paste("baggedETS",i))
	}

	mforecasts[1,,1]<-forecastsETS 
	mforecasts[2,,1]<-forecastsBATS 
	mforecasts[3,,1]<-forecastsbaggedETS

	fcast<-numeric(fh)
	for(j in 1:fh){
		fcast[j]<-round(sum(OPERA[[i]]$weights[j,] * mforecasts[1:3,j,1]),4)
	}
	
	fcastNA<-rep(NA, 48-fh)
	strfcast<-noquote(paste(c(fname,fcast,fcastNA),collapse = ","))
	fforecasts[i,]<-fcast	

	if(i==1){
		write.table(strfcast, file = "D:\\M4\\Daily.csv", append = F,quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	} else{
		write.table(strfcast, file = "D:\\M4\\Daily.csv", append = T, quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	}
}