### Script for final model
# Script for Daily forecasting
PATH <- "C:/Users/pp9596/Documents/Bitbucket/M4"
setwd(PATH)

# Utility Function

#' @description Function to evaluate SMAPE Error
#' @param outsample: Actual value
#' @param forecasts: Forecasted value
smape_cal <- function(outsample, forecasts){
  #Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts))
  return(smape)
}

#' @description Function to evaluate MASE Error
#' @param insample: Insample data
#' @param outsample: Actual value
#' @param forecasts: Forecasted value
mase_cal <- function(insample, outsample, forecasts){
  #Used to estimate MASE
  frq <- frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- (abs(outsample-forecasts))/masep
  return(mase)
}

# Load packages
require(forecast)
require(forecastHybrid)
require(prophet)

### Script for daily forecast
# Load train and test dataset
load("data/daily_mydata.train.rda")
load("data/daily_mydata.test.rda")

set.seed(12345)      # random seed used in model: seed set.seed(12345)
h.fc  = 14            
freq  = 7        
Nseries <- length(mydata.train)

# Define error matrix
Total_smape = array(NA,dim = c(Nseries, h.fc))
Total_mase = array(NA,dim = c(Nseries, h.fc))
yhatList <- array(NA,dim = c(Nseries, h.fc))

ModelSelected <- NULL
start_time <- Sys.time()
for(s in 1:200){ # Nseries
  cat("processing series - ", s, "\n")
  trn <- mydata.train[[s]]
  tst <- mydata.test[[s]]
  fullsignal <- c(trn, tst )
  
  
  # Build model
  # TBARS with multiple seasonality
   model_tbats <- tbats(fullsignal, seasonal.periods = c(freq))
   yhat_tbats <- forecast(model_tbats, h = h.fc)$mean
  
  # ETS
  model_ets <- ets(fullsignal)    
  yhat_ets <- forecast(model_ets, h = h.fc)$mean
  
  # ARIMA
  model <- auto.arima(trn)
  yhat_arima <- forecast(model, h = h.fc)$mean
  MASE_ARIMA <- mean(mase_cal(trn, tst, yhat_arima), na.rm = T)
  MASE_ARIMA <- if(is.na(MASE_ARIMA) | is.infinite(MASE_ARIMA)) 999 else MASE_ARIMA
  
  # Mean ensemble
  if(MASE_ARIMA<2){
    model <- auto.arima(fullsignal)
    yhat_arima <- forecast(model, h = h.fc)$mean
    yhat_fin_df <- cbind(as.numeric(yhat_tbats), as.numeric(yhat_ets), as.numeric(yhat_arima))  
  } else
  {
    yhat_fin_df <- cbind(as.numeric(yhat_tbats), as.numeric(yhat_ets))  
  }
  
  # Evaluate final values
  yhat_fin <- apply(yhat_fin_df, 1, FUN = mean)
  
  # Evaluate error Aggregated
  # Total_smape[s,] <- smape_cal(tst, yhat_fin)
  # Total_mase[s,] <- mase_cal(trn, tst, yhat_fin)
  yhatList[s, ] <- yhat_fin
}
end_time <- Sys.time()
write.csv(yhatList, file = "submission_daily.csv", row.names = F)