# Initial variables 
fh_yearly <- 6 
fh_quarterly <- 8 
fh_monthly<- 18
fh_weekly <- 13
fh_daily <- 14
fh_hourly <- 48

# Data packages 
library(M4comp2018)
library(ggplot2)
library(forecast)
data(M4)

#---------Initial functions-------#
# function 1 
# Check about seasonality 
# input is just a timeseries and periods per year 
SeasonalityTest <- function(input, ppy){
  #Used for determining whether the time series is seasonal
  tcrit <- 1.645
  if (length(input)<3*ppy){
    test_seasonal <- FALSE
  }else{
    xacf <- acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )
    if (is.na(test_seasonal)==TRUE){ test_seasonal <- FALSE }
  }
  return(test_seasonal)
}

yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)
quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)
monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
weekly_M4 <- Filter(function(l) l$period == "Weekly", M4)
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)
daily_M4 <- Filter(function(l) l$period == "Daily", M4)

# same data.frames for the forecasts and export to csv 
yearly_M4_forecast <- data.frame(matrix(NA, ncol= fh_yearly+1, nrow = length(yearly_M4)))
weekly_M4_forecast <- data.frame(matrix(NA, ncol= fh_weekly+1, nrow = length(weekly_M4)))
quarterly_M4_forecast <- data.frame(matrix(NA, ncol= fh_quarterly+1, nrow = length(quarterly_M4)))
monthly_M4_forecast <- data.frame(matrix(NA, ncol= fh_monthly+1, nrow = length(monthly_M4)))
hourly_M4_forecast <- data.frame(matrix(NA, ncol= fh_hourly+1, nrow = length(hourly_M4)))
daily_M4_forecast <- data.frame(matrix(NA, ncol= fh_daily+1, nrow = length(daily_M4)))

final_dataset <- data.frame(matrix(NA, ncol= 48, nrow = 100000))

datasets<-list(yearly_M4, quarterly_M4, monthly_M4, weekly_M4, daily_M4, hourly_M4)

# Forecasts for yearly time series 
for (j in 1:length(datasets)){
  for (i in 1:length(yearly_M4)){
    
    input <- datasets[[j]][[i]]$x
    # Do decomp if nessesary 
    # Estimate seasonaly adjusted time series
    ppy <- frequency(input) ; ST <- F
    if(j == 1){
      fh <- 6
    }else if(j == 2){
      fh <- 8
    }else if(j == 3){
      fh <- 18
    }else if(j == 4){
      fh <- 13
    }else if(j == 5){
      fh <- 14
    }else {
      fh <- 48
    }
    if (ppy>1){
      ST <- SeasonalityTest(input,ppy)
    }
    if (ST==T){
      Dec <- decompose(input, type="multiplicative")
      des_input <- input/Dec$seasonal
      SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
    }else{
      des_input <- input ; SIout <- rep(1, fh)
    }
    
    # Do box cox for the best l
    lambda<-BoxCox.lambda(des_input, method="loglik", lower=0, upper=1)
    data.bxcx <- BoxCox(des_input, lambda)
    
    # Forecast
    data.forecast <- thetaf(data.bxcx, h=fh)
    
    # Inv boxcox the same as x actually 
    data.forecast.inv<-InvBoxCox(data.forecast$mean, lambda)
    
    #multiply with the respective indices
    if (ST==T){
      data.forecast.ses<-data.forecast.inv*SIout
    }else{
      data.forecast.ses <- data.forecast.inv
    }
    
    
    final_dataset[i,1] <- datasets[[j]][[i]]$st
    final_dataset[i, 2:(fh+1)]<-ifelse(data.forecast.ses[]<0, 0, data.forecast.ses)
  }
}


write.csv(final_dataset, "final_dataset.csv",sep=";") 

