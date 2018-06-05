## Load libraries
library(forecast)
library(opera)


## Load data
# Download it from https://github.com/carlanetto/M4comp2018/tree/master/data
load("M4.RData")
Y_M4 <- Filter(function(l) l$period == "Yearly", M4)

########################### Training
# The provided data in M4 were splitted into training and out-of-sample sets
# The size of out-of-sample set is equal to the needed forecasts in the competition
# STEP-1: Two models were chosen for YEARLY forecasts: TBATS and ETS
# STEP-2: Aggregate the forecasts from the the models using Polynomial Potential aggregation rule algorithm in "opera" package

fh<-6
fq<-1
series_num<-length(Y_M4)
forecasts<-array(NA,dim=c(2,fh,series_num))


for (i in 1:series_num){

  insampleend=Y_M4[[i]]$n-fh
  insample <- window(Y_M4[[i]]$x,end=insampleend)

  # TBATS
  mdlTBATS <-tbats(insample, biasadj = FALSE)
  forecastsTBATS <-forecast(mdlTBATS ,h=fh)$mean

  # ETS
  mdlETS <- ets(insample)
  forecastsETS <- forecast(mdlETS ,h=fh)$mean
  
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

  forecasts[1,1:fh,i] <-forecastsTBATS 
  forecasts[2,1:fh,i] <-forecastsETS
}


OPERA<-NULL
for(i in 1:series_num){
	MLpol0 <- mixture(model = "MLpol", loss.type = "square")
	X <- cbind(TBATS=forecasts[1,,i], ETS=forecasts[2,,i])

      insampleend=Y_M4[[i]]$n-fh
      outsample <- window(Y_M4[[i]]$x,start=insampleend+1) 

	MLpol <- predict(MLpol0, newexpert = X, newY = outsample, online = TRUE, type="all")
	OPERA[i]<-list(MLpol)
}


########################### M4 YEARLY data forecasting
# Forecast using the models
# Aggregate the forecasts from the models based on the weights optained perviously using "opera" package OR you can load it

# load("operaYearly.RData")

fforecasts<-array(NA,dim=c(series_num,fh))
mforecasts<-array(NA,dim=c(2,fh,1))
for(i in 1:series_num){
	fname<-Y_M4[[i]]$st

  	# TBATS
  	mdlTBATS <-tbats(Y_M4[[i]]$x, biasadj = FALSE)
  	forecastsTBATS <-forecast(mdlTBATS ,h=fh)$mean

  	# ETS
  	mdlETS <- ets(Y_M4[[i]]$x)
  	forecastsETS <- forecast(mdlETS ,h=fh)$mean

  	if(min(forecastsTBATS ,na.rm=T)<0){
		lgx<-log(Y_M4[[i]]$x)
		ff1 <- tbats(lgx, biasadj = FALSE)
		forecastsTBATS <-exp(forecast(ff1,h=fh)$mean)
  	}

  	if(min(forecastsETS ,na.rm=T)<0){
		lgx<-log(Y_M4[[i]]$x)
		ff1<-ets(lgx)
		forecastsETS <-exp(forecast(ff1,h=fh)$mean)
  	} 

	mforecasts[1,,1]<-forecastsTBATS 
	mforecasts[2,,1]<-forecastsETS 

	fcast<-numeric(fh)
	for(j in 1:fh){
		fcast[j]<-round(sum(OPERA[[i]]$weights[j,] * mforecasts[1:2,j,1]),4)
	}
	
	fcastNA<-rep(NA, 48-fh)
	strfcast<-noquote(paste(c(fname,fcast,fcastNA),collapse = ","))
	fforecasts[i,]<-fcast	

	if(i==1){
		write.table(strfcast, file = "D:\\M4\\Yearly.csv", append = F,quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	} else{
		write.table(strfcast, file = "D:\\M4\\Yearly.csv", append = T, quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	}
}
