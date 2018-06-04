
install.packages(c("zoo","forecast","doParallel","rstudioapi"))
library(zoo)
library(forecast)
library(doParallel)
library(data.table)
library(rstudioapi)

rm(list = ls())
no_cores <- detectCores()
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores)


current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

mydata <- data.frame(t(read.csv("input/monthly.csv", header=FALSE)))
n <- ncol(mydata)

#########################smape function
smape_cal <- function(forecasts, outsample){
  
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- mean((abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts)))
  return(smape)
}


##########################cross validation function
cross_validation <-function(ts=NULL, method=c('holt', 'theta', 'ets', 'naive', 'Dholt', 'arima', 'tbats','stla','stle','stlt','loets','lotheta','loarima'),
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
      for(h in step_ahead:step_ahead)
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
      for(h in step_ahead:step_ahead)
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
      for(h in step_ahead:step_ahead)
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
      for(h in step_ahead:step_ahead)
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
      for(h in step_ahead:step_ahead)
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
    f_tbats <- function(x, h){forecast(auto.arima(x),h=h)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in step_ahead:step_ahead)
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
      for(h in step_ahead:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'stla'){
    f_tbats <- function(x, h){stlf(x, h=h,method='arima')}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in step_ahead:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'stle'){
    f_tbats <- function(x, h){stlf(x, h=h,method='ets')}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in step_ahead:step_ahead)
      { 
        e <- tsCV(ts, forecastfunction = f_tbats, h=h, start = start) 
        e1 <- sqrt(e^2)[(start+h):(length(ts))]
        RMSE_tscv[h,1:length(e1)] <- e1
        print(h)
      } 
      return(RMSE_tscv)
      
    }
  }
  else if (method == 'stlt'){
    f_tbats <- function(x, h){stlf(x, h=h,forecastfunction=thetaf)}
    
    if (error == 'RMSE'){
      RMSE_tscv <- matrix(NA, nrow = step_ahead, ncol = (length(ts)-start))
      for(h in step_ahead:step_ahead)
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
      for(h in step_ahead:step_ahead)
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
      for(h in step_ahead:step_ahead)
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
      for(h in step_ahead:step_ahead)
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




  monthly_model <- 
  foreach(i = 1:n, .combine = 'rbind', .packages=c('zoo','forecast')) %dopar% {
  raw_data <- ts(na.trim(mydata[,i], sides = "right"), f=12)
  train <- subset(raw_data, end = (length(raw_data)-18))
  test <- subset(raw_data, start = (length(raw_data)-17))

  raw_data2 <- ts(na.trim(mydata[,i], sides = "right"), f=1)
  

 
  tb1 <- forecast(tbats(train), h=18)
  ifelse(length(raw_data)<=1000,error_tbats <- smape_cal(tb1$mean,test),error_tbats <- Inf)  #1

  ar1 <- forecast(auto.arima(train), h=18)
  ifelse(length(raw_data)<=1000,error_arima <- smape_cal(ar1$mean,test),error_arima <- Inf)

  
  fcst_ets2 <- forecast(ets(train), h=18)  #3
  error_ets2<- smape_cal(fcst_ets2$mean,test) 
  
  
  ifelse(length(raw_data)<=1000,error_com1 <- smape_cal((tb1$mean + ar1$mean + fcst_ets2$mean)/3,test),error_com1 <- Inf)    #4

  
  fcst_stla <- try(stlf(train, h=18, forecastfunction = thetaf),silent = TRUE)    #5
  error_stla <- tryCatch(error_stla<- smape_cal(fcst_stla$mean,test) ,
                         error = function(e) {return(Inf)})                     
  
  fcst_stle <- try(stlf(train, h=18, method = "ets"),silent = TRUE)      #6
  error_stle <- tryCatch(error_stle<- smape_cal(fcst_stle$mean,test) ,
                         error = function(e) {return(Inf)})   
  
  
   
  error_nnetar <- Inf  #7

  
  
  fcst_theta <- try(thetaf(train, h=18), silent = TRUE)    #8
  error_theta <- tryCatch(error_theta <- smape_cal(fcst_theta$mean,test) ,
                          error = function(e) {return(Inf)})
  
  avg <- mean(train[(length(train)-18):length(train)]) #9
  fcst_avg <- rep(avg,18)
  error_avg <- smape_cal(fcst_avg,test)
  
  fcst_naive <- naive(train, h=18)     #10
  error_naive <- smape_cal(fcst_naive$mean,test) 
  
  fcst_ses1 <- ses(train, h=18, alpha = .3) #11
  error_ses1 <- smape_cal(fcst_ses1$mean,test) 
  
  fcst_ses2 <- ses(train, h=18, alpha = .1) #12
  error_ses2 <- smape_cal(fcst_ses2$mean,test) 
  
  fcst_ses3 <- ses(train, h=18) #13
  error_ses3 <- smape_cal(fcst_ses3$mean,test) 
  
  fcst_holt <- holt(train, h=18) #14
  error_holt <- smape_cal(fcst_holt$mean,test) 
  
  fcst_Dholt <- holt(train, h=18, damped = TRUE) #15
  error_Dholt <- smape_cal(fcst_Dholt$mean,test) 
  
  com2 <- (fcst_theta$mean+fcst_avg+fcst_naive$mean+fcst_ses1$mean+fcst_ses2$mean+fcst_ses3$mean+fcst_holt$mean+fcst_Dholt$mean)/8 #16
  error_com2 <- smape_cal(com2,test)
   
  ifelse(length(raw_data)<=1000,error_com3 <- smape_cal((ar1$mean+fcst_ets2$mean+fcst_theta$mean)/3,test),error_com3 <- Inf)     #17
  
  ifelse((is.infinite(error_theta) | length(raw_data)>1000),error_com4 <- Inf, com4 <- (ar1$mean+fcst_theta$mean)/2)   #18
  ifelse((is.infinite(error_theta) | length(raw_data)>1000),error_com4 <- Inf, error_com4 <- smape_cal(com4,test))


  ifelse(is.infinite(error_theta),error_com5 <- Inf, com5 <- (fcst_ets2$mean+fcst_theta$mean)/2)  #19
  ifelse(is.infinite(error_theta),error_com5 <- Inf, error_com5 <- smape_cal(com5,test)) 

  ifelse(is.infinite(error_theta),error_com6 <- Inf, com6 <- (fcst_theta$mean+ fcst_ses1$mean)/2)  #20
  ifelse(is.infinite(error_theta),error_com6 <- Inf, error_com6 <- smape_cal(com6,test)) 
  
  com7 <- (fcst_ses1$mean+ fcst_ses2$mean)/2 #21
  error_com7 <- smape_cal(com7,test)
  
  ifelse(length(raw_data)<=1000,error_com8 <- smape_cal((ar1$mean+fcst_ets2$mean)/2,test),error_com8 <- Inf) #22

   fcst_snaive <- snaive(train, h=18)     #23
  error_snaive <- smape_cal(fcst_snaive$mean,test) 
  
  
  ifelse(is.infinite(error_stla),error_com9 <- Inf, com9 <- (fcst_stla$mean + fcst_stle$mean)/2) #24
  ifelse(is.infinite(error_stla),error_com9 <- Inf, error_com9 <- smape_cal(com9,test))
  
  ifelse((is.infinite(error_stla) | is.infinite(error_theta) | length(raw_data)>1000),error_com10 <- Inf, com10 <- (fcst_stla$mean + fcst_stle$mean + fcst_ets2$mean + ar1$mean + fcst_theta$mean)/5)   #25
  ifelse((is.infinite(error_stla) | is.infinite(error_theta) | length(raw_data)>1000),error_com10 <- Inf, error_com10 <- smape_cal(com10,test))
   
  
  alpha <- c(error_tbats,error_arima,error_ets2,error_com1,error_stla,error_stle,error_nnetar,error_theta,error_avg,error_naive,error_ses1,error_ses2,
             error_ses3,error_holt,error_Dholt,error_com2, error_com3,error_com4,error_com5,error_com6,error_com7,error_com8,error_snaive, error_com9,error_com10)
  
  
   
pq <- which.min(as.matrix( alpha, nrow=25))
pqv <- alpha[pq]

if (pqv <= 5) {
if(pq == 1) {
              fcst <- forecast(tbats(raw_data), h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==2){
              fcst <- forecast(auto.arima(raw_data), h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==3){
              fcst <- forecast(ets(raw_data), h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==4){
              fcst1 <- forecast(tbats(raw_data), h=18)
              fcst2 <- forecast(auto.arima(raw_data), h=18)
              fcst3 <- forecast(ets(raw_data), h=18) 
              fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean)/3
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2])/3
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2])/3
            } else if (pq==5){
              fcst <- stlf(raw_data, h=18, forecastfunction = thetaf)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==6){
              fcst <- stlf(raw_data, h=18, method = "ets")
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==7){
              fcst <- forecast(nnetar(raw_data),PI=TRUE,h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==8){
              fcst <- thetaf(raw_data, h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==9){
              fcstm <- rep(mean(raw_data[(length(raw_data)-18):length(raw_data)]),18)
              fcstu <- naive(raw_data, h=18)$upper[,2] 
              fcstl <- naive(raw_data, h=18)$lower[,2] 
            } else if (pq==10){
              fcst <- naive(raw_data, h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==11){
              fcst <- ses(raw_data, h=18, alpha = .3)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==12){
              fcst <- ses(raw_data, h=18, alpha = .1)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==13){
              fcst <- ses(raw_data, h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==14){
              fcst <- holt(raw_data, h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==15){
              fcst <- holt(raw_data, h=18, damped = TRUE)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==16){
              fcstm <- (thetaf(raw_data, h=18)$mean + rep(mean(raw_data[(length(raw_data)-18):length(raw_data)]),18) + naive(raw_data, h=18)$mean+ses(raw_data, h=18, alpha = .3)$mean + 
                ses(raw_data, h=18, alpha = .1)$mean + ses(raw_data, h=18)$mean + holt(raw_data, h=18)$mean + holt(raw_data, h=18, damped = TRUE)$mean)/8
              fcstu <- (thetaf(raw_data, h=18)$upper[,2] + snaive(raw_data, h=18)$upper[,2] + naive(raw_data, h=18)$upper[,2]+ses(raw_data, h=18, alpha = .3)$upper[,2]+ 
                          ses(raw_data, h=18, alpha = .1)$upper[,2] + ses(raw_data, h=18)$upper[,2] + holt(raw_data, h=18)$upper[,2] + holt(raw_data, h=18, damped = TRUE)$upper[,2])/8
              fcstl <- (thetaf(raw_data, h=18)$lower[,2] + snaive(raw_data, h=18)$lower[,2] + naive(raw_data, h=18)$lower[,2]+ses(raw_data, h=18, alpha = .3)$lower[,2]+ 
                          ses(raw_data, h=18, alpha = .1)$lower[,2] + ses(raw_data, h=18)$lower[,2] + holt(raw_data, h=18)$lower[,2] + holt(raw_data, h=18, damped = TRUE)$lower[,2])/8
            } else if (pq==17){
              fcst1 <- forecast(auto.arima(raw_data), h=18)
              fcst2 <- forecast(ets(raw_data), h=18)
              fcst3 <- thetaf(raw_data, h=18)
              fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean)/3
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2])/3
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2])/3
            } else if (pq==18){
              fcst1 <- forecast(auto.arima(raw_data), h=18)
              fcst2 <- thetaf(raw_data, h=18) 
              fcstm <- (fcst1$mean + fcst2$mean)/2
              fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
            } else if (pq==19){
              fcst1 <- forecast(ets(raw_data), h=18)
              fcst2 <- thetaf(raw_data, h=18) 
              fcstm <- (fcst1$mean + fcst2$mean)/2
              fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
              
            } else if (pq==20){
              fcst1 <- ses(raw_data, h=18, alpha = .1)
              fcst2 <- thetaf(raw_data, h=18) 
              fcstm <- (fcst1$mean + fcst2$mean)/2
              fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
              
            } else if (pq==21){
              fcst1 <- ses(raw_data, h=18, alpha = .1)
              fcst2 <- ses(raw_data, h=18, alpha = .3)
              fcstm <- (fcst1$mean + fcst2$mean)/2
              fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
            } else if (pq==22){
              fcst1 <- forecast(auto.arima(raw_data), h=18)
              fcst2 <- forecast(ets(raw_data), h=18)
              fcstm <- (fcst1$mean + fcst2$mean)/2
              fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
              
            } else if (pq==23){
              fcst <- snaive(raw_data, h=18)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==24){
              fcst1 <- stlf(raw_data, h=18, forecastfunction=thetaf)
              fcst2 <- stlf(raw_data, h=18, method = "ets")   
              fcstm <- (fcst1$mean+fcst2$mean)/2
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2])/2
            } else if (pq==25){
              fcst1 <- forecast(auto.arima(raw_data), h=18)
              fcst2 <- stlf(raw_data, h=18, forecastfunction=thetaf)
              fcst3 <- stlf(raw_data, h=18, method = "ets")
              fcst4 <- forecast(ets(raw_data), h=18)
              fcst5 <- thetaf(raw_data, h=18)
              fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean+fcst4$mean+fcst5$mean)/5
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2]+fcst4$upper[,2]+fcst5$upper[,2])/5
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2]+fcst4$lower[,2]+fcst5$lower[,2])/5

            } 
  
}else if (pqv > 5) { 
  a1 <- mean(cross_validation(raw_data, method = 'ets', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE)
  b1 <- mean(cross_validation(raw_data, method = 'holt', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE)
  c1<- mean(cross_validation(raw_data, method = 'theta', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE)
  d1 <- mean(cross_validation(raw_data, method = 'naive', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE)
  e1 <- mean(cross_validation(raw_data, method = 'Dholt', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE)
  ifelse(length(raw_data)< 250, f1 <- mean(cross_validation(raw_data, method = 'arima', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE), f1 <- Inf )
  ifelse(length(raw_data)<250, g1 <- mean(cross_validation(raw_data, method = 'tbats', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE), g1 <- Inf )
  ifelse(length(raw_data)>36, h1 <- mean(cross_validation(raw_data, method = 'stla', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE), h1 <- Inf )
  ifelse(length(raw_data)>36, i1 <- mean(cross_validation(raw_data, method = 'stle', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE), i1 <- Inf )
  ifelse(length(raw_data)>36, j1 <- mean(cross_validation(raw_data, method = 'stlt', error = 'RMSE',step_ahead = 4, start = length(raw_data)-18)[4,],na.rm = TRUE), j1 <- Inf )
  
  
  k1 <- mean(cross_validation(raw_data2, method = 'ets', error = 'RMSE',step_ahead = 4, start = length(raw_data2)-18)[4,],na.rm = TRUE)
  l1 <- mean(cross_validation(raw_data2, method = 'holt', error = 'RMSE',step_ahead = 4, start = length(raw_data2)-18)[4,],na.rm = TRUE)
  m1 <- mean(cross_validation(raw_data2, method = 'theta', error = 'RMSE',step_ahead = 4, start = length(raw_data2)-18)[4,],na.rm = TRUE)
  n1 <- mean(cross_validation(raw_data2, method = 'Dholt', error = 'RMSE',step_ahead = 4, start = length(raw_data2)-18)[4,],na.rm = TRUE)
  ifelse(length(raw_data2)< 250, o1 <- mean(cross_validation(raw_data2, method = 'arima', error = 'RMSE',step_ahead = 4, start = length(raw_data2)-18)[4,],na.rm = TRUE), o1 <- Inf )
  #p1 <- mean(cross_validation(raw_data2, method = 'loets', error = 'RMSE',step_ahead = 4, start = length(raw_data2)-18)[4,],na.rm = TRUE)
  #q1 <- mean(cross_validation(raw_data2, method = 'lotheta', error = 'RMSE',step_ahead = 4, start = length(raw_data2)-18)[4,],na.rm = TRUE)
  #ifelse(length(raw_data2)< 250, r1 <- mean(cross_validation(raw_data2, method = 'loarima', error = 'RMSE',step_ahead = 4, start = length(raw_data2)-18)[4,],na.rm = TRUE), r1 <- Inf )

  p1 <- Inf
  q1 <-Inf
  r1 <- Inf
  
  
  beta <- c(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1)
  lv <- which.min(as.matrix( beta, nrow=18))
  if(lv == 1) {
    fcst <- forecast(ets(raw_data), h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==2){
    fcst <- holt(raw_data, h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==3){
    fcst <- thetaf(raw_data, h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==4){
    fcst <- naive(raw_data, h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==5){
    fcst <- holt(raw_data, h=18, damped = TRUE)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==6){
    fcst <- forecast(auto.arima(raw_data), h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==7){
    fcst <- forecast(tbats(raw_data), h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==8){
    fcst <- stlf(raw_data, h=18, method='arima')
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==9){
    fcst <- stlf(raw_data, h=18, method='ets')
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==10){
    fcst <- stlf(raw_data, h=18, forecastfunction=thetaf)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==11){
    fcst <- forecast(ets(raw_data2), h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==12){
    fcst <- holt(raw_data2, h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==13){
    fcst <- thetaf(raw_data2, h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==14){
    fcst <- holt(raw_data2, h=18, damped = TRUE)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==15){
    fcst <- forecast(auto.arima(raw_data2), h=18)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
    
  } else if (lv==16){
    lo <- loess(raw_data2~time(raw_data2))
    fcst1 <- forecast(ets(lo$fitted), h=18)
    
    fcstm <- fcst1$mean
    fcstu <- fcst1$upper[,2]
    fcstl <- fcst1$lower[,2]
    
  } else if (lv==17){
    lo <- loess(raw_data2~time(raw_data2))
    fcst1 <- thetaf(lo$fitted, h=18)
    
    fcstm <- fcst1$mean
    fcstu <- fcst1$upper[,2]
    fcstl <- fcst1$lower[,2]
    
  } else if (lv==18){
    lo <- loess(raw_data2~time(raw_data2))
    fcst1 <- forecast(auto.arima(lo$fitted), h=18)
    
    fcstm <- fcst1$mean
    fcstu <- fcst1$upper[,2]
    fcstl <- fcst1$lower[,2]
    
  }
  
  
}

  
 c(fcstm,fcstu,fcstl)
}
stopCluster(cl)  

  
  
  
  fcstm <- monthly_model[,1:18]  
  fcstu <- monthly_model[,19:36]  
  fcstl <- monthly_model[,37:54]  
  
  dir.create(file.path(getwd(), 'output'), showWarnings = FALSE)
  
  write.csv(fcstm,file = "output/monthly-v18.csv") 
  write.csv(fcstu,file = "output/monthly-v18-upper.csv") 
  write.csv(fcstl,file = "output/monthly-v18-lower.csv") 
  
  
  
  
  
  
