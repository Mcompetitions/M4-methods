
install.packages(c("zoo","forecast","doParallel","rstudioapi"))
library(zoo)
library(forecast)
library(doParallel)
library(rstudioapi)

rm(list = ls())
no_cores <- detectCores()
registerDoParallel(cores=no_cores) 
cl <- makeCluster(no_cores)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

mydata <- data.frame(t(read.csv("input/daily.csv", header=FALSE)))
n <- ncol(mydata)

#################################smape function
smape_cal <- function(forecasts, outsample){
  
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- mean((abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts)))
  return(smape)
}


#####################################cross validation function
cross_validation <-function(ts=NULL, method=c('holt', 'theta', 'ets', 'naive', 'Dholt', 'arima', 'tbats', 'str', 'loets','lotheta','loarima'),
                            error=c('RMSE'), step_ahead=1, start=1)
{
  tsCV <- function(y, forecastfunction, h=1, window=NULL, ..., start=1)
  {
    #y <- as.ts(y)
    n = length(y)
    e <- y*NA
    for(i in (start:(n-h)))
    {
      fc <- try(suppressWarnings(forecastfunction(subset(y, start=ifelse(is.null(window),1,ifelse(i-window >= 0, i-window + 1, stop("small window"))), end=i), h=h, ...)), silent=TRUE)
      if(!is.element("try-error", class(fc)))
        e[i+h] <- y[i+h] - fc$mean[h]
    }
    return(e)
  }
  
  
  if (method == 'ets'){
    f_ets <- function(x, h){forecast(ets(x), h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_ets, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
      
      
    }
  }
  
  
  else if (method == 'theta'){
    f_arima <- function(x, h){thetaf(x, h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_arima, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
      
    }
  }
  
  
  else if (method == 'holt'){
    f_tbats <- function(x, h){holt(x, h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'naive'){
    f_tbats <- function(x, h){naive(x, h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'Dholt'){
    f_tbats <- function(x, h){holt(x, h=h, damped = TRUE)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'arima'){
    f_tbats <- function(x, h){forecast(auto.arima(x,approximation = FALSE, stepwise = FALSE),h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'tbats'){
    f_tbats <- function(x, h){forecast(tbats(x),h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'str'){
    f_tbats <- function(x, h){forecast(StructTS(x),h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'loets'){
    f_tbats <- function(x, h){forecast(ets((loess(x~time(x)))$fitted), h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'lotheta'){
    f_tbats <- function(x, h){thetaf((loess(x~time(x)))$fitted, h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'loarima'){
    f_tbats <- function(x, h){forecast(auto.arima((loess(x~time(x)))$fitted), h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in 1:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  
}





  daily_model <- 
  foreach(i = 1:n, .combine = 'rbind', .packages=c('zoo','forecast')) %dopar% {
  raw_data <- ts(na.trim(mydata[,i], sides = "right"), f=frequency(na.trim(mydata[,i], sides = "right")))
  train <- subset(raw_data, end = (length(raw_data)-14))
  test <- subset(raw_data, start = (length(raw_data)-13))

  
  
  fit_tbats <- tbats(train)    #1
  fcst_tbats <- forecast(fit_tbats, h=14)
  error_tbats <- smape_cal(fcst_tbats$mean,test)
  
  fcst_arima <- forecast(auto.arima(train, approximation = FALSE, stepwise = FALSE), h=14) #2
  error_arima<- smape_cal(fcst_arima$mean,test)

  fcst_ets1 <- forecast(ets(train), h=14) #3
  error_ets1<- smape_cal(fcst_ets1$mean,test)
  
  fcst_ets2 <- forecast(ets(train), h=14)  #4
  error_ets2<- smape_cal(fcst_ets2$mean,test)
  
  com1 <- (fcst_tbats$mean + fcst_arima$mean + fcst_ets2$mean)/3 #5
  error_com1 <- smape_cal(com1,test)
 
  
  fcst_theta <- thetaf(train, h=14)    #6
  error_theta <- smape_cal(fcst_theta$mean,test)
  
  avg <- mean(train[(length(train)-14):length(train)]) #7
  fcst_avg <- rep(avg,14)
  error_avg <- smape_cal(fcst_avg,test)
  
  fcst_naive <- naive(train, h=14)     #8
  error_naive <- smape_cal(fcst_naive$mean,test)
  
  fcst_ses1 <- ses(train, h=14, alpha = .3) #9
  error_ses1 <- smape_cal(fcst_ses1$mean,test)
  
  fcst_ses2 <- ses(train, h=14, alpha = .1) #10
  error_ses2 <- smape_cal(fcst_ses2$mean,test)
  
  fcst_ses3 <- ses(train, h=14) #11
  error_ses3 <- smape_cal(fcst_ses3$mean,test)
  
  fcst_holt <- holt(train, h=14) #12
  error_holt <- smape_cal(fcst_holt$mean,test)
  
  fcst_Dholt <- holt(train, h=14, damped = TRUE) #13
  error_Dholt <- smape_cal(fcst_Dholt$mean,test)
  
  com2 <- (fcst_theta$mean+fcst_avg+fcst_naive$mean+fcst_ses1$mean+fcst_ses2$mean+fcst_ses3$mean+fcst_holt$mean+fcst_Dholt$mean)/8 #14
  error_com2 <- smape_cal(com2,test)
  
  com3 <- (fcst_arima$mean+fcst_ets1$mean+fcst_ets2$mean+fcst_theta$mean)/4 #15
  error_com3 <- smape_cal(com3,test)
  
  com4 <- (fcst_arima$mean+fcst_theta$mean)/2 #16
  error_com4 <- smape_cal(com4,test)

  com5 <- (fcst_ets2$mean+fcst_theta$mean)/2 #17
  error_com5 <- smape_cal(com5,test)

  com6 <- (fcst_theta$mean+ fcst_ses1$mean)/2 #18
  error_com6 <- smape_cal(com6,test)
  
  com7 <- (fcst_ses1$mean+ fcst_ses2$mean)/2 #19
  error_com7 <- smape_cal(com7,test)
  
  com8 <- (fcst_ses1$mean+ fcst_ses2$mean + fcst_ses3$mean)/3 #20
  error_com8 <- smape_cal(com8,test)
  
  lo <- loess(train~time(train))
  fcst_loets <- forecast(ets(lo$fitted), h=14)
  error_loets <- smape_cal(fcst_loets$mean, test)
  
  fcst_lotheta <- thetaf(lo$fitted, h=14)
  error_lotheta <- smape_cal(fcst_lotheta$mean, test)
  
  fcst_loarima <- forecast(auto.arima(lo$fitted), h=14)
  error_loarima <- smape_cal(fcst_loarima$mean, test)
  
  
  alpha <- c(error_tbats,error_arima,error_ets1,error_ets2,error_com1,error_theta,error_avg,error_naive,error_ses1,error_ses2,
             error_ses3,error_holt,error_Dholt,error_com2, error_com3,error_com4,error_com5,error_com6,error_com7,error_com8, error_loets,error_lotheta, error_loarima)
  
  
   
pq <- which.min(as.matrix( alpha, nrow=23))
pqv <- alpha[pq]
if (pqv <= 3) {
    if(pq == 1) {
                  fcst <- forecast(tbats(raw_data), h=14)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==2){
                  fcst <- forecast(auto.arima(raw_data, approximation = FALSE, stepwise = FALSE), h=14)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==3){
                  fcst <- forecast(ets(raw_data), h=14)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==4){
                  fcst <- forecast(ets(raw_data), h=14)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==5){
                  fcst1 <- forecast(tbats(raw_data), h=14)
                  fcst2 <- forecast(auto.arima(raw_data, approximation = FALSE, stepwise = FALSE), h=14)
                  fcst3 <- forecast(ets(raw_data), h=14) 
                  fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean)/3
                  fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2])/3
                  fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2])/3
                } else if (pq==6){
                  fcst <- thetaf(raw_data, h=14)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==7){
                  fcstm <- rep(mean(raw_data[(length(raw_data)-14):length(raw_data)]),14)
                  fcstu <- naive(raw_data, h=14)$upper[,2]
                  fcstl <- naive(raw_data, h=14)$lower[,2]
                } else if (pq==8){
                  fcst <- naive(raw_data, h=14)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==9){
                  fcst <- ses(raw_data, h=14, alpha = .3)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==10){
                  fcst <- ses(raw_data, h=14, alpha = .1)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==11){
                  fcst <- ses(raw_data, h=14)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==12){
                  fcst <- holt(raw_data, h=14)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==13){
                  fcst <- holt(raw_data, h=14, damped = TRUE)
                  fcstm <- fcst$mean
                  fcstu <- fcst$upper[,2]
                  fcstl <- fcst$lower[,2]
                } else if (pq==14){
                  fcstm <- (thetaf(raw_data, h=14)$mean + rep(mean(raw_data[(length(raw_data)-14):length(raw_data)]),14) + naive(raw_data, h=14)$mean+ses(raw_data, h=14, alpha = .3)$mean + 
                    ses(raw_data, h=14, alpha = .1)$mean + ses(raw_data, h=14)$mean + holt(raw_data, h=14)$mean + holt(raw_data, h=14, damped = TRUE)$mean)/8
                  fcstu <- (thetaf(raw_data, h=14)$upper[,2] + naive(raw_data, h=14)$upper[,2] + naive(raw_data, h=14)$upper[,2]+ses(raw_data, h=14, alpha = .3)$upper[,2] + 
                              ses(raw_data, h=14, alpha = .1)$upper[,2] + ses(raw_data, h=14)$upper[,2] + holt(raw_data, h=14)$upper[,2] + holt(raw_data, h=14, damped = TRUE)$upper[,2])/8
                  fcstu <- (thetaf(raw_data, h=14)$lower[,2] + naive(raw_data, h=14)$lower[,2] + naive(raw_data, h=14)$lower[,2]+ses(raw_data, h=14, alpha = .3)$lower[,2] + 
                              ses(raw_data, h=14, alpha = .1)$lower[,2] + ses(raw_data, h=14)$lower[,2] + holt(raw_data, h=14)$lower[,2] + holt(raw_data, h=14, damped = TRUE)$lower[,2])/8
                } else if (pq==15){
                  fcst1 <- forecast(auto.arima(raw_data, approximation = FALSE, stepwise = FALSE), h=14)
                  fcst2 <- forecast(ets(raw_data), h=14)
                  fcst3 <- thetaf(raw_data, h=14)
                  fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean)/3
                  fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2])/3
                  fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2])/3
                } else if (pq==16){
                  fcst1 <- forecast(auto.arima(raw_data, approximation = FALSE, stepwise = FALSE), h=14)
                  fcst2 <- thetaf(raw_data, h=14) 
                  fcstm <- (fcst1$mean + fcst2$mean)/2
                  fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
                  fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
                } else if (pq==17){
                  fcst1 <- forecast(ets(raw_data), h=14)
                  fcst2 <- thetaf(raw_data, h=14) 
                  fcstm <- (fcst1$mean + fcst2$mean)/2
                  fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
                  fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
                  
                } else if (pq==18){
                  fcst1 <- ses(raw_data, h=14, alpha = .1)
                  fcst2 <- thetaf(raw_data, h=14) 
                  fcstm <- (fcst1$mean + fcst2$mean)/2
                  fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
                  fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
                } else if (pq==19){
                  fcst1 <- ses(raw_data, h=14, alpha = .1)
                  fcst2 <- ses(raw_data, h=14, alpha = .3)
                  fcstm <- (fcst1$mean + fcst2$mean)/2
                  fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
                  fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
                } else if (pq==20){
                  fcst1 <- ses(raw_data, h=14, alpha = .1)
                  fcst2 <- ses(raw_data, h=14, alpha = .3)
                  fcst3 <- ses(raw_data, h=14)
                  fcstm <- (fcst1$mean + fcst2$mean + fcst3$mean)/3
                  fcstu <- (fcst1$upper[,2] + fcst2$upper[,2] + fcst3$upper[,2])/3
                  fcstl <- (fcst1$lower[,2] + fcst2$lower[,2] + fcst3$lower[,2])/3
                  
                } else if (pq==21){
                  lo <- loess(raw_data~time(raw_data))
                  fcst1 <- forecast(ets(lo$fitted), h=14)
                  
                  fcstm <- fcst1$mean
                  fcstu <- fcst1$upper[,2]
                  fcstl <- fcst1$lower[,2]
                  
                } else if (pq==22){
                  lo <- loess(raw_data~time(raw_data))
                  fcst1 <- thetaf(lo$fitted, h=14)
                  
                  fcstm <- fcst1$mean
                  fcstu <- fcst1$upper[,2]
                  fcstl <- fcst1$lower[,2]
                  
                } else if (pq==23){
                  lo <- loess(raw_data~time(raw_data))
                  fcst1 <- forecast(auto.arima(lo$fitted), h=14)
                  
                  fcstm <- fcst1$mean
                  fcstu <- fcst1$upper[,2]
                  fcstl <- fcst1$lower[,2]
                  
                }
  }else if (pqv > 3) {
    a1 <- mean(cross_validation(raw_data, method = 'ets', error = 'RMSE',step_ahead = 4, start = length(raw_data)-14)[4,], na.rm=TRUE)
    b1 <- mean(cross_validation(raw_data, method = 'holt', error = 'RMSE',step_ahead = 4, start = length(raw_data)-14)[4,], na.rm=TRUE)
    c1<- mean(cross_validation(raw_data, method = 'theta', error = 'RMSE',step_ahead = 4, start = length(raw_data)-14)[4,], na.rm=TRUE)
    d1 <- mean(cross_validation(raw_data, method = 'naive', error = 'RMSE',step_ahead = 4, start = length(raw_data)-14)[4,], na.rm=TRUE)
    e1 <- mean(cross_validation(raw_data, method = 'Dholt', error = 'RMSE',step_ahead = 4, start = length(raw_data)-14)[4,], na.rm=TRUE)
    f1 <- mean(cross_validation(raw_data, method = 'arima', error = 'RMSE',step_ahead = 4, start = length(raw_data)-14)[4,], na.rm=TRUE)
    g1 <- mean(cross_validation(raw_data, method = 'tbats', error = 'RMSE',step_ahead = 4, start = length(raw_data)-14)[4,], na.rm=TRUE)
    #h1 <- mean(cross_validation(raw_data, method = 'loets', error = 'RMSE',step_ahead = 4, start = length(raw_data)-12)[4,], na.rm = TRUE)
    #i1 <- mean(cross_validation(raw_data, method = 'lotheta', error = 'RMSE',step_ahead = 4, start = length(raw_data)-12)[4,], na.rm = TRUE)
    #j1 <- mean(cross_validation(raw_data, method = 'loarima', error = 'RMSE',step_ahead = 4, start = length(raw_data)-12)[4,], na.rm = TRUE)
    
    h1 <- Inf
    i1 <- Inf
    j1 <- Inf
    
    beta <- c(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1)
    lv <- which.min(as.matrix( beta, nrow=10))
    
    if(lv == 1) {
      fcst <- forecast(ets(raw_data), h=14)
      fcstm <- fcst$mean
      fcstu <- fcst$upper[,2]
      fcstl <- fcst$lower[,2]
    } else if (lv==2){
      fcst <- holt(raw_data, h=14)
      fcstm <- fcst$mean
      fcstu <- fcst$upper[,2]
      fcstl <- fcst$lower[,2]
    } else if (lv==3){
      fcst <- thetaf(raw_data, h=14)
      fcstm <- fcst$mean
      fcstu <- fcst$upper[,2]
      fcstl <- fcst$lower[,2]
    } else if (lv==4){
      fcst <- naive(raw_data, h=14)
      fcstm <- fcst$mean
      fcstu <- fcst$upper[,2]
      fcstl <- fcst$lower[,2]
    } else if (lv==5){
      fcst <- holt(raw_data, h=14, damped = TRUE)
      fcstm <- fcst$mean
      fcstu <- fcst$upper[,2]
      fcstl <- fcst$lower[,2]
    } else if (lv==6){
      fcst <- forecast(auto.arima(raw_data, approximation = FALSE, stepwise = FALSE), h=14)
      fcstm <- fcst$mean
      fcstu <- fcst$upper[,2]
      fcstl <- fcst$lower[,2]
    } else if (lv==7){
      fcst <- forecast(tbats(raw_data), h=14)
      fcstm <- fcst$mean
      fcstu <- fcst$upper[,2]
      fcstl <- fcst$lower[,2]
    } else if (lv==8){
      lo <- loess(raw_data~time(raw_data))
      fcst1 <- forecast(ets(lo$fitted), h=14)
      
      fcstm <- fcst1$mean
      fcstu <- fcst1$upper[,2]
      fcstl <- fcst1$lower[,2]
    } else if (lv==9){
      lo <- loess(raw_data~time(raw_data))
      fcst1 <- thetaf(lo$fitted, h=14)
      
      fcstm <- fcst1$mean
      fcstu <- fcst1$upper[,2]
      fcstl <- fcst1$lower[,2]
    } else if (lv==10){
      lo <- loess(raw_data~time(raw_data))
      fcst1 <- forecast(auto.arima(lo$fitted), h=14)
      
      fcstm <- fcst1$mean
      fcstu <- fcst1$upper[,2]
      fcstl <- fcst1$lower[,2]
    }
    
    
    
  }
    
 c(fcstm, fcstu, fcstl)
 
}
stopCluster(cl)  

  
  
  
  
  
  fcstm <- daily_model[,1:14]  
  fcstu <- daily_model[,15:28]  
  fcstl <- daily_model[,29:42]  
  
  dir.create(file.path(getwd(), 'output'), showWarnings = FALSE)
  
  write.csv(fcstm,file = "output/daily-v12.csv") 
  write.csv(fcstu,file = "output/daily-v12-upper.csv") 
  write.csv(fcstl,file = "output/daily-v12-lower.csv") 
  
  
  
  
  
  
  
  
  
  
  
  

