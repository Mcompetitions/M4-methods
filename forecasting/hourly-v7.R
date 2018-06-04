
install.packages(c("zoo","forecast","doParallel","rstudioapi"))
install.packages("curl")
library(zoo)
library(curl)
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

mydata <- data.frame(t(read.csv("input/hourly.csv", header=FALSE)))
n <- ncol(mydata)

#############################################smape function
smape_cal <- function(forecasts, outsample){
  
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- mean((abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts)))
  return(smape)
}

#############################################Cross validation function
cross_validation <-function(ts=NULL, method=c('holt','Dholt', 'theta', 'tbats', 'naive', 'snaive'),
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
  
  
  if (method == 'tbats'){
    f_ets <- function(x, h){forecast(tbats(x), h=h)}
    
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
  else if (method == 'snaive'){
    f_tbats <- function(x, h){snaive(x, h=h)}
    
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
  
}


  
  yearly_model <- 
  foreach(i = 1:n, .combine = 'rbind', .packages=c('zoo','forecast')) %dopar% {
  raw_data <- ts(na.trim(mydata[,i], sides = "right"), f=24)
  train <- subset(raw_data, end = (length(raw_data)-48))
  test <- subset(raw_data, start = (length(raw_data)-47))

  
  
 
  #######################TBATS###############
  
  fit_tbats <- tbats(train)    #1
  fcst_tbats <- forecast(fit_tbats, h=48)
  error_tbats <- smape_cal(fcst_tbats$mean,test)
  ######################AF##################
  
  aic_model<- matrix(NA, nrow=12, ncol=1)
  for(j in 1:12)
  {
    bestfit <- auto.arima(train, xreg=fourier(train, K=j), seasonal=FALSE)
    aic_model[j,] <- bestfit$aic
  }
  k=which.min(aic_model)
  bestfit <- auto.arima(train, xreg=fourier(train, K=k), seasonal=FALSE)
  fcst_af <- forecast(bestfit, xreg = fourier(train, K=k, h=48))                    #2
  error_af<- smape_cal(fcst_af$mean,test)
  
  

  
  com1 <- (fcst_tbats$mean + fcst_af$mean)/2 #3
  error_com1 <- smape_cal(com1,test)
  
  ################################NAIVE
  
  fcst_stla <- try(stlf(train, h=48,forecastfunction = thetaf),silent = TRUE)    #4
  error_stla <- tryCatch(error_stla<- smape_cal(fcst_stla$mean,test) ,
                         error = function(e) {return(Inf)})                     
  
  fcst_stle <- try(stlf(train, h=48, method = "ets"),silent = TRUE)      #5
  error_stle <- tryCatch(error_stle<- smape_cal(fcst_stle$mean,test) ,
                         error = function(e) {return(Inf)})   
  
  fcst_nnetar <- forecast(nnetar(train), h=48)                          #6
  error_nnetar <- smape_cal(fcst_nnetar$mean, test) 
  
  
  
  fcst_theta <- thetaf(train, h=48)    #7
  error_theta <- smape_cal(fcst_theta$mean,test) 
  
  avg <- mean(train[(length(train)-48):length(train)]) #8
  fcst_avg <- rep(avg,48)
  error_avg <- smape_cal(fcst_avg,test)
  
  fcst_naive <- naive(train, h=48)     #9
  error_naive <- smape_cal(fcst_naive$mean,test) 
  
  fcst_ses1 <- ses(train, h=48, alpha = .3) #10
  error_ses1 <- smape_cal(fcst_ses1$mean,test) 
  
  fcst_ses2 <- ses(train, h=48, alpha = .1) #11
  error_ses2 <- smape_cal(fcst_ses2$mean,test) 
  
  fcst_ses3 <- ses(train, h=48) #12
  error_ses3 <- smape_cal(fcst_ses3$mean,test) 
  
  fcst_holt <- holt(train, h=48) #13
  error_holt <- smape_cal(fcst_holt$mean,test) 
  
  fcst_Dholt <- holt(train, h=48, damped = TRUE) #14
  error_Dholt <- smape_cal(fcst_Dholt$mean,test) 
  
  com2 <- (fcst_theta$mean+fcst_avg+fcst_naive$mean+fcst_ses1$mean+fcst_ses2$mean+fcst_ses3$mean+fcst_holt$mean+fcst_Dholt$mean)/8 #15
  error_com2 <- smape_cal(com2,test)
  
  com3 <- (fcst_tbats$mean+fcst_af$mean+fcst_theta$mean)/3 #16
  error_com3 <- smape_cal(com3,test)
  
  com4 <- (fcst_af$mean+fcst_theta$mean)/2 #17
  error_com4 <- smape_cal(com4,test)

  com5 <- (fcst_tbats$mean+fcst_theta$mean)/2 #18
  error_com5 <- smape_cal(com5,test)

  com6 <- (fcst_theta$mean+ fcst_ses1$mean)/2 #19
  error_com6 <- smape_cal(com6,test)
  
  com7 <- (fcst_ses1$mean+ fcst_ses2$mean)/2 #20
  error_com7 <- smape_cal(com7,test)
  
  com8 <- (fcst_ses1$mean+ fcst_ses2$mean + fcst_ses3$mean)/3 #21
  error_com8 <- smape_cal(com8,test)
  
  
  fcst_snaive <- snaive(train, h=48)     #22
  error_snaive <- smape_cal(fcst_snaive$mean,test) 
  
  
  ifelse(is.infinite(error_stla),error_com9 <- Inf, com9 <- (fcst_stla$mean + fcst_stle$mean)/2) #23
  ifelse(is.infinite(error_stla),error_com9 <- Inf, error_com9 <- smape_cal(com9,test))
  
  ifelse(is.infinite(error_stla),error_com10 <- Inf, com10 <- (fcst_stla$mean + fcst_stle$mean + fcst_tbats$mean + fcst_af$mean + fcst_theta$mean)/5)   #24
  ifelse(is.infinite(error_stla),error_com10 <- Inf, error_com10 <- smape_cal(com10,test))
  
  com11 <- (fcst_nnetar$mean+ fcst_theta$mean)/2 #25
  error_com11 <- smape_cal(com11,test)
  
  
  com12 <- (fcst_nnetar$mean+ fcst_theta$mean + fcst_tbats$mean)/3 #26
  error_com12 <- smape_cal(com12,test)
  
  alpha <- c(error_tbats,error_af,error_com1,error_stla,error_stle,error_nnetar,error_theta,error_avg,error_naive,error_ses1,error_ses2,
             error_ses3,error_holt,error_Dholt,error_com2, error_com3,error_com4,error_com5,error_com6,error_com7,error_com8,error_snaive, error_com9,error_com10,error_com11,error_com12)
  

  
  
   
pq <- which.min(as.matrix( alpha, nrow=26))
pqv <- alpha[pq]

  ############################################
    raw_data2 <- msts(na.trim(mydata[,i], sides = "right"), seasonal.periods = c(24,168))
    train2 <- subset(raw_data2, end = (length(raw_data2)-48))
    test2 <- subset(raw_data2, start = (length(raw_data2)-47))

    fit_tbats2 <- tbats(train2)    #1
    fcst_tbats2 <- forecast(fit_tbats2, h=48)
    error_tbats2 <- smape_cal(fcst_tbats2$mean,test2)
 

    
    fcst_stla2 <- try(stlf(train2, h=48,forecastfunction = thetaf),silent = TRUE)    #4
    error_stla2 <- tryCatch(error_stla2<- smape_cal(fcst_stla2$mean,test2) ,
                           error = function(e) {return(Inf)})                     
    
    fcst_stle2 <- try(stlf(train2, h=48, method = "ets"),silent = TRUE)      #5
    error_stle2 <- tryCatch(error_stle2<- smape_cal(fcst_stle2$mean,test2) ,
                           error = function(e) {return(Inf)})   
    
    
    fcst_stlar <- try(stlf(train2, h=48, method = "arima"),silent = TRUE)      #5
    error_stlar <- tryCatch(error_stlar<- smape_cal(fcst_stlar$mean,test2) ,
                           error = function(e) {return(Inf)})   

    
    
    
    beta <- c(error_tbats2,error_stla2,error_stle2,error_stlar)
    lv <- which.min(as.matrix( beta, nrow=4))
    lvq <- beta[lv]
    
    
    #c(i, which.min(as.matrix(pqv,lvq)))


if (pqv <= lvq) {
if(pq == 1) {
              fcst <- forecast(tbats(raw_data), h=48)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==2){
              aic_model2<- matrix(NA, nrow=12, ncol=1)
              for(j in 1:12)
              {
                bestfit2 <- auto.arima(raw_data, xreg=fourier(raw_data, K=j), seasonal=FALSE)
                aic_model2[j,] <- bestfit2$aic
              }
              kk=which.min(aic_model2)
              bestfit3 <- auto.arima(raw_data, xreg=fourier(raw_data, K=kk), seasonal=FALSE)
              fcst <- forecast(bestfit3, xreg = fourier(raw_data, K=kk, h=48))
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==3){
              aic_model2<- matrix(NA, nrow=12, ncol=1)
              for(j in 1:12)
              {
                bestfit2 <- auto.arima(raw_data, xreg=fourier(raw_data, K=j), seasonal=FALSE)
                aic_model2[j,] <- bestfit2$aic
              }
              kk=which.min(aic_model2)
              bestfit3 <- auto.arima(raw_data, xreg=fourier(raw_data, K=kk), seasonal=FALSE)
              fcst1 <- forecast(bestfit3, xreg = fourier(raw_data, K=kk, h=48)) 
              fcst2 <- forecast(tbats(raw_data), h=48)
              fcstm <- (fcst1$mean+fcst2$mean)/2
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2])/2
            } else if (pq==4){
              fcst <- stlf(raw_data, h=48, forecastfunction=thetaf)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==5){
              fcst <- stlf(raw_data, h=48, method = "ets")
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==6){
              fcst <- forecast(nnetar(raw_data), PI=TRUE, h=48)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==7){
              fcst <- thetaf(raw_data, h=48)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==8){
              fcstm <- rep(mean(raw_data[(length(raw_data)-48):length(raw_data)]),48)
              fcstu <- naive(raw_data, h=48)$upper[,2] 
              fcstl <- naive(raw_data, h=48)$lower[,2] 
            } else if (pq==9){
              fcst <- naive(raw_data, h=48)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==10){
              fcst <- ses(raw_data, h=48, alpha = .3)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==11){
              fcst <- ses(raw_data, h=48, alpha = .1)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==12){
              fcst <- ses(raw_data, h=48)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==13){
              fcst <- holt(raw_data, h=48)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==14){
              fcst <- holt(raw_data, h=48, damped = TRUE)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==15){
              fcstm <- (thetaf(raw_data, h=48)$mean +  snaive(raw_data, h=48)$mean + naive(raw_data, h=48)$mean+ses(raw_data, h=48, alpha = .3)$mean +  
                         ses(raw_data, h=48, alpha = .1)$mean + ses(raw_data, h=48)$mean + holt(raw_data, h=48)$mean + holt(raw_data, h=48, damped = TRUE)$mean)/8
              fcstu <- (thetaf(raw_data, h=48)$upper[,2] +  snaive(raw_data, h=48)$upper[,2] + naive(raw_data, h=48)$upper[,2]+ses(raw_data, h=48, alpha = .3)$upper[,2] +  
                ses(raw_data, h=48, alpha = .1)$upper[,2] + ses(raw_data, h=48)$upper[,2] + holt(raw_data, h=48)$upper[,2] + holt(raw_data, h=48, damped = TRUE)$upper[,2])/8
              fcstl <- (thetaf(raw_data, h=48)$lower[,2]+  snaive(raw_data, h=48)$lower[,2] + naive(raw_data, h=48)$lower[,2]+ses(raw_data, h=48, alpha = .3)$lower[,2] +  
                         ses(raw_data, h=48, alpha = .1)$lower[,2] + ses(raw_data, h=48)$lower[,2] + holt(raw_data, h=48)$lower[,2] + holt(raw_data, h=48, damped = TRUE)$lower[,2])/8
              
            } else if (pq==16){
              aic_model2<- matrix(NA, nrow=12, ncol=1)
              for(j in 1:12)
              {
                bestfit2 <- auto.arima(raw_data, xreg=fourier(raw_data, K=j), seasonal=FALSE)
                aic_model2[j,] <- bestfit2$aic
              }
              kk=which.min(aic_model2)
              bestfit3 <- auto.arima(raw_data, xreg=fourier(raw_data, K=kk), seasonal=FALSE)
              fcst1 <- forecast(bestfit3, xreg = fourier(raw_data, K=kk, h=48)) 
              fcst2 <- forecast(tbats(raw_data), h=48)
              fcst3 <- thetaf(raw_data, h=48)
              fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean)/3
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2])/3
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2])/3
            } else if (pq==17){
              aic_model2<- matrix(NA, nrow=12, ncol=1)
              for(j in 1:12)
              {
                bestfit2 <- auto.arima(raw_data, xreg=fourier(raw_data, K=j), seasonal=FALSE)
                aic_model2[j,] <- bestfit2$aic
              }
              kk=which.min(aic_model2)
              bestfit3 <- auto.arima(raw_data, xreg=fourier(raw_data, K=kk), seasonal=FALSE)
              fcst1 <- forecast(bestfit3, xreg = fourier(raw_data, K=kk, h=48)) 
              fcst2 <- thetaf(raw_data, h=48)
              fcstm <- (fcst1$mean+fcst2$mean)/2
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2])/2
              
            } else if (pq==18){
              fcst1 <- forecast(tbats(raw_data), h=48)
              fcst2 <- thetaf(raw_data, h=48)
              fcstm <- (fcst1$mean+fcst2$mean)/2
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2])/2
            } else if (pq==19){
              fcst1 <- thetaf(raw_data, h=48)
              fcst2 <- ses(raw_data, h=48, alpha = .1)   
              fcstm <- (fcst1$mean + fcst2$mean)/2
              fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
            } else if (pq==20){
              fcst1 <- ses(raw_data, h=48, alpha = .3)
              fcst2 <- ses(raw_data, h=48, alpha = .1) 
              fcstm <- (fcst1$mean + fcst2$mean)/2
              fcstu <- (fcst1$upper[,2] + fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2] + fcst2$lower[,2])/2
            } else if (pq==21){
              fcst1 <- ses(raw_data, h=48, alpha = .3)
              fcst2 <- ses(raw_data, h=48, alpha = .1)
              fcst3 <- ses(raw_data, h=48)
              fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean)/3
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2])/3
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2])/3
            } else if (pq==22){
              fcst <- snaive(raw_data, h=48)
              fcstm <- fcst$mean
              fcstu <- fcst$upper[,2]
              fcstl <- fcst$lower[,2]
            } else if (pq==23){
              fcst1 <- stlf(raw_data, h=48, forecastfunction=thetaf)
              fcst2 <- stlf(raw_data, h=48, method = "ets")   
              fcstm <- (fcst1$mean+fcst2$mean)/2
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2])/2
            } else if (pq==24){
              aic_model2<- matrix(NA, nrow=12, ncol=1)
              for(j in 1:12)
              {
                bestfit2 <- auto.arima(raw_data, xreg=fourier(raw_data, K=j), seasonal=FALSE)
                aic_model2[j,] <- bestfit2$aic
              }
              kk=which.min(aic_model2)
              bestfit3 <- auto.arima(raw_data, xreg=fourier(raw_data, K=kk), seasonal=FALSE)
              fcst1 <- forecast(bestfit3, xreg = fourier(raw_data, K=kk, h=48)) 
              fcst2 <- stlf(raw_data, h=48, forecastfunction=thetaf)
              fcst3 <- stlf(raw_data, h=48, method = "ets")
              fcst4 <- forecast(tbats(raw_data), h=48)
              fcst5 <- thetaf(raw_data, h=48)
              fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean+fcst4$mean+fcst5$mean)/5
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2]+fcst4$upper[,2]+fcst5$upper[,2])/5
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2]+fcst4$lower[,2]+fcst5$lower[,2])/5
            } else if (pq==25){
              fcst1 <- forecast(nnetar(raw_data),PI=TRUE, h=48)
              fcst2 <- thetaf(raw_data, h=48)
              fcstm <- (fcst1$mean+fcst2$mean)/2
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2])/2
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2])/2
            } else if (pq==26){
              fcst1 <- forecast(nnetar(raw_data),PI=TRUE, h=48)
              fcst2 <- thetaf(raw_data, h=48)
              fcst3 <- forecast(tbats(raw_data), h=48)
              fcstm <- (fcst1$mean+fcst2$mean+fcst3$mean)/3
              fcstu <- (fcst1$upper[,2]+fcst2$upper[,2]+fcst3$upper[,2])/3
              fcstl <- (fcst1$lower[,2]+fcst2$lower[,2]+fcst3$lower[,2])/3
            }
  
}else if (lvq < pqv) { 
  
 
  
  if(lv == 1) {
    fcst <- forecast(tbats(raw_data2), h=48)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==2){
    fcst <- stlf(raw_data2, h=48, forecastfunction=thetaf)
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==3){
    fcst <- stlf(raw_data2, h=48, method = "ets")
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } else if (lv==4){
    fcst <- stlf(raw_data2, h=48, method = "arima")
    fcstm <- fcst$mean
    fcstu <- fcst$upper[,2]
    fcstl <- fcst$lower[,2]
  } 
}   
  
  
  
 c(fcstm, fcstu,fcstl)
 
}
stopCluster(cl)  

  
  fcstm <- yearly_model[,1:48]  
  fcstu <- yearly_model[,49:96]  
  fcstl <- yearly_model[,97:144]  
  
  dir.create(file.path(getwd(), 'output'), showWarnings = FALSE)
  
  write.csv(fcstm,file = "output/hourly-v7.csv") 
  write.csv(fcstu,file = "output/hourly-v7-upper.csv") 
  write.csv(fcstl,file = "output/hourly-v7-lower.csv")  
  
  
  

