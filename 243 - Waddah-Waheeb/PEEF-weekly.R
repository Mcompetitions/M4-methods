## Load libraries
library(forecast)
library(opera)


## Load data
# Download it from https://github.com/carlanetto/M4comp2018/tree/master/data
load("M4.RData")
W_M4 <- Filter(function(l) l$period == "Weekly", M4)

########################### Training
# The provided data in M4 were split into training and out-of-sample sets
# The size of out-of-sample set is equal to the needed forecasts in the competition
# STEP-1: Two models were chosen for WEEKLY forecasts: Regression with ARIMA errors and TBATS
# STEP-2: Aggregate the forecasts from the the models using Polynomial Potential aggregation rule algorithm in "opera" package

fh<-13
series_num<-length(W_M4)
forecasts<-array(NA,dim=c(2,fh,series_num))

for (i in 1:series_num){

	insampleend=W_M4[[i]]$n-fh
	insample <- window(W_M4[[i]]$x,end=insampleend)
	Train<- ts(insample , frequency=365.25/7)

	# Regression with ARIMA errors
 	# https://robjhyndman.com/hyndsight/forecasting-weekly-data/
	bestfit <- list(aicc=Inf)
	for(K in seq(25)) {
		fit <- auto.arima(Train, xreg=fourier(Train, K=K),seasonal=FALSE)
		if(fit[["aicc"]] < bestfit[["aicc"]]) {
			bestfit <- fit
    			bestK <- K
  		}
  	}
  	f1 <- forecast(bestfit,xreg=fourier(Train, K=bestK, h=fh))$mean
  
  	# TBATS
	ff2<-tbats(Train, biasadj = FALSE)
  	f2<-forecast(ff2,h=fh)$mean

	if(min(f1,na.rm=T)<0){
		lgx<-log(Train)
		bestfit <- list(aicc=Inf)
		for(K in seq(25)) {
  			fit <- auto.arima(lgx, xreg=fourier(lgx, K=K),seasonal=FALSE)
  			if(fit[["aicc"]] < bestfit[["aicc"]]) {
    				bestfit <- fit
    				bestK <- K
  			}
		}
		f1 <- exp(forecast(bestfit,xreg=fourier(lgx, K=bestK, h=fh))$mean)
   	}

   	if(min(f2,na.rm=T)<0){
		lgx<-log(Train)
		ff2<-tbats(lgx, biasadj = FALSE)
		f2<-exp(forecast(ff2,h=fh)$mean)
  	}

  	forecasts[1,1:fh,i] <-f1
  	forecasts[2,1:fh,i] <-f2
}

OPERA<-NULL
for(i in 1:series_num){
	MLpol0 <- mixture(model = "MLpol", loss.type = "square")
	X <- cbind(m1=forecasts[1,,i], m2=forecasts[2,,i])

      insampleend=W_M4[[i]]$n-fh
      outsample <- window(W_M4[[i]]$x,start=insampleend+1) 

	MLpol <- predict(MLpol0, newexpert = X, newY = outsample, online = TRUE, type="all")
	OPERA[i]<-list(MLpol)
}


########################### M4 WEEKLY data forecasting
# Forecast using the models
# Aggregate the forecasts from the models based on the weights optained perviously using "opera" package OR you can load it

# load("operaWeekly.RData")

fforecasts<-array(NA,dim=c(series_num,fh))
mforecasts<-array(NA,dim=c(2,fh,1))
for(i in 1:series_num){
	fname<-W_M4[[i]]$st

  	Data<- ts(W_M4[[i]]$x, frequency=365.25/7)

	# Regression with ARIMA errors
 	# https://robjhyndman.com/hyndsight/forecasting-weekly-data/
  	bestfit <- list(aicc=Inf)
  	for(K in seq(25)) {
  		fit <- auto.arima(Data, xreg=fourier(Data, K=K),seasonal=FALSE)
  		if(fit[["aicc"]] < bestfit[["aicc"]]) {
    			bestfit <- fit
    			bestK <- K
  		}
  	}
  	f1 <- forecast(bestfit,xreg=fourier(Data, K=bestK, h=fh))$mean

	#TBATS
  	ff2<-tbats(Data, biasadj = FALSE)
  	f2<-forecast(ff2,h=fh)$mean

  	if(min(f1,na.rm=T)<0){
		lgx<-log(Data)
		bestfit <- list(aicc=Inf)
		for(K in seq(25)) {
  			fit <- auto.arima(lgx, xreg=fourier(lgx, K=K),seasonal=FALSE)
  			if(fit[["aicc"]] < bestfit[["aicc"]]) {
    				bestfit <- fit
    				bestK <- K
  			}
		}
		f1 <- exp(forecast(bestfit,xreg=fourier(lgx, K=bestK, h=fh))$mean)
   	}

   	if(min(f2,na.rm=T)<0){
		lgx<-log(Data)
		ff2<-tbats(lgx, use.parallel = TRUE, biasadj = FALSE)
		f2<-exp(forecast(ff2,h=fh)$mean)
  	}

	mforecasts[1,,1]<-f1 
	mforecasts[2,,1]<-f2 

	fcast<-numeric(fh)
	for(j in 1:fh){
		fcast[j]<-round(sum(OPERA[[i]]$weights[j,] * mforecasts[1:2,j,1]),4)
	}
	
	fcastNA<-rep(NA, 48-fh)
	strfcast<-noquote(paste(c(fname,fcast,fcastNA),collapse = ","))
	fforecasts[i,]<-fcast	

	if(i==1){
		write.table(strfcast, file = "D:\\M4\\Weekly.csv", append = F,quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	} else{
		write.table(strfcast, file = "D:\\M4\\Weekly.csv", append = T, quote = F, sep = "",
            eol = "\n", row.names = F, col.names = F, qmethod = "escape")
	}
}
