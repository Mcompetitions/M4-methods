#######################################
## AUXILIARY FUNCTIONS AND VARIABLES ##
#######################################
library(forecast)

# Seasonality Test
SeasonalityTest <- function(input, ppy){
  #Used to determine whether a time series is seasonal
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
# SEasonal Adjustment
SeasonalAdjusted <- function(input, frequency, horizon){
  ST <- FALSE
  if (frequency>1){ ST <- SeasonalityTest(input, frequency) }
  if (ST==T){
    Dec <- decompose(input, type="multiplicative")
    des_input <- input/Dec$seasonal
    SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-frequency+1):length(Dec$seasonal)], horizon), horizon)
  }else{
    des_input <- input ; SIout <- rep(1, horizon)
  }
  return(list(des_input = des_input, SIout = SIout))
}
##############
# PREDILAB forecasting function for a single time series
##############
naiveSeasonalM4 <- function(y, origin, horizon, frequency){
  # M4 seasonal naive method
  # REMEMBER y IS A LIST!!!
  # horizon <- y$Horizon
  # frequency <- y$Frequency
  if (frequency == 1){
    forecasts <- naive(y[1 : origin], h = horizon)$mean
  } else {
    forecasts <- head(rep(tail(y[1 : origin], frequency), horizon), horizon)
  }
  return(list(forecasts = forecasts,
              model = NA))
}

Naive2M4 <- function(y, origin, horizon, frequency){
  # M4 Naive2 method
  # REMEMBER y IS A LIST!!!
  # horizon <- y$Horizon
  # frequency <- y$Frequency
  yts = ts(y[1 : origin], frequency = frequency)
  out <- SeasonalAdjusted(yts, frequency, horizon)
  forecasts <- naive(out$des_input, h = horizon)$mean*out$SIout
  return(list(forecasts = forecasts,
              model = NA))
}

SesM4 <- function(y, origin, horizon, frequency){
  # M4 Ses method
  # REMEMBER y IS A LIST!!!
  # horizon <- y$Horizon
  # frequency <- y$Frequency
  yts = ts(y[1 : origin], frequency = frequency)
  out <- SeasonalAdjusted(yts, frequency, horizon)
  forecasts <- ses(out$des_input, h = horizon)$mean*out$SIout
  return(list(forecasts = forecasts,
              model = NA))
}

HoltM4 <- function(y, origin, horizon, frequency){
  # M4 Holt method, no damping
  # REMEMBER y IS A LIST!!!
  # horizon <- y$Horizon
  # frequency <- y$Frequency
  yts = ts(y[1 : origin], frequency = frequency)
  out <- SeasonalAdjusted(yts, frequency, horizon)
  forecasts <- holt(out$des_input, h = horizon, damped = FALSE)$mean*out$SIout
  return(list(forecasts = forecasts,
              model = NA))
}

DampedM4 <- function(y, origin, horizon, frequency){
  # M4 Holt method with damping
  # REMEMBER y IS A LIST!!!
  # horizon <- y$Horizon
  # frequency <- y$Frequency
  yts = ts(y[1 : origin], frequency = frequency)
  out <- SeasonalAdjusted(yts, frequency, horizon)
  forecasts <- holt(out$des_input, h = horizon, damped = TRUE)$mean*out$SIout
  return(list(forecasts = forecasts,
              model = NA))
}

CombM4 <- function(y, origin, horizon, frequency){
  # M4 Ses method
  # REMEMBER y IS A LIST!!!
  # horizon <- y$Horizon
  # frequency <- y$Frequency
  yts = ts(y[1 : origin], frequency = frequency)
  out <- SeasonalAdjusted(yts, frequency, horizon)
  f1 <- ses(out$des_input, h = horizon)$mean*out$SIout
  f2 <- holt(out$des_input, h = horizon, damped = FALSE)$mean*out$SIout
  f3 <- holt(out$des_input, h = horizon, damped = TRUE)$mean*out$SIout
  return(list(forecasts = (f1 + f2 + f3)/3,
              model = NA))
}

ThetaM4 <- function(y, origin, horizon, frequency){
  # M4 Theta method
  # REMEMBER y IS A LIST!!!
  Theta.classic <- function(input, fh){
    #Used to estimate Theta classic
    #Set parameters
    wses <- wlrl<-0.5 ; theta <- 2
    #Estimate theta line (0)
    observations <- length(input)
    xt <- c(1:observations)
    xf <- c((observations+1):(observations+fh))
    train <- data.frame(input=input, xt=xt)
    test <- data.frame(xt = xf)
    estimate <- lm(input ~ poly(xt, 1, raw=TRUE))
    thetaline0In <- as.numeric(predict(estimate))
    thetaline0Out <- as.numeric(predict(estimate,test))
    #Estimate theta line (2)
    thetalineT <- theta*input+(1-theta)*thetaline0In
    sesmodel <- ses(thetalineT, h=fh)
    thetaline2In <- sesmodel$fitted
    thetaline2Out <- sesmodel$mean
    #Theta forecasts
    forecastsIn <- (thetaline2In*wses)+(thetaline0In*wlrl)
    forecastsOut <- (thetaline2Out*wses)+(thetaline0Out*wlrl)
    #Zero forecasts become positive
    for (i in 1:length(forecastsOut)){
      if (forecastsOut[i]<0){ forecastsOut[i]<-0 }
    }

    output=list(fitted = forecastsIn, mean = forecastsOut,
                fitted0 = thetaline0In, mean0 = thetaline0Out,
                fitted2 = thetaline2In, mean2 = thetaline2Out)
    return(output)
  }
  # horizon <- y$Horizon
  # frequency <- y$Frequency
  yts = ts(y[1 : origin], frequency = frequency)
  out <- SeasonalAdjusted(yts, frequency, horizon)
  forecasts <- Theta.classic(out$des_input, horizon)$mean*out$SIout #Theta
  return(list(forecasts = forecasts, model= NA))
}

FourThetaM4<- function(input, origin , fh, frequency){
  # M4 Theta4 method
  # REMEMBER y IS A LIST!!!
  
  #Used to automatically select the best Theta model
  #This code can be used to reproduce the forecasts submitted to the M4 competition for the 4Theta method
  #Authors: E. Spiliotis and V. Assimakopoulos (2017) / Forecasting & Strategy Unit - NTUA
  #Method Description: Generalizing the Theta model for automatic forecasting
  #Method Type: Statistical - Decomposition
  Theta.fit <- function(input, fh, theta, curve, model, seasonality , plot=FALSE){
    #Used to fit a Theta model
    #Check if the inputs are valid
    if (theta<0){ theta <- 2  }
    if (fh<1){ fh <- 1  }
    #Estimate theta line weights
    outtest <- naive(input, h=fh)$mean
    if (theta==0){
      wses <- 0
    }else{
      wses <- (1/theta)
    }
    wlrl <- (1-wses)
    #Estimate seasonaly adjusted time series
    ppy <- frequency(input)
    if (seasonality=="N"){
      des_input <- input ; SIout <- rep(1, fh) ; SIin <- rep(1, length(input))
    }else if (seasonality=="A"){
      Dec <- decompose(input, type="additive")
      des_input <- input-Dec$seasonal
      SIin <- Dec$seasonal
      SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
    }else{
      Dec <- decompose(input, type="multiplicative")
      des_input <- input/Dec$seasonal
      SIin <- Dec$seasonal
      SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
    }
    #If negative values, force to linear model
    if (min(des_input)<=0){ curve <- "Lrl" ; model <- "A"  }
    #Estimate theta line zero
    observations <- length(des_input)
    xs <- c(1:observations)
    xf = xff <- c((observations+1):(observations+fh))
    dat=data.frame(des_input=des_input, xs=xs)
    newdf <- data.frame(xs = xff)
    if (curve=="Exp"){
      estimate <- lm(log(des_input)~xs)
      thetaline0In <- exp(predict(estimate))+input-input
      thetaline0Out <- exp(predict(estimate, newdf))+outtest-outtest
    }else{
      estimate <- lm(des_input ~ poly(xs, 1, raw=TRUE))
      thetaline0In <- predict(estimate)+des_input-des_input
      thetaline0Out <- predict(estimate, newdf)+outtest-outtest
    }
    #Estimete Theta line (theta)
    if (model=="A"){
      thetalineT <- theta*des_input+(1-theta)*thetaline0In
    }else if ((model=="M")&(all(thetaline0In>0)==T)&(all(thetaline0Out>0)==T)){
      thetalineT <- (des_input^theta)*(thetaline0In^(1-theta))
    }else{
      model<-"A"
      thetalineT <- theta*des_input+(1-theta)*thetaline0In
    }
    #forecasting TL2
    sesmodel <- ses(thetalineT, h=fh)
    thetaline2In <- sesmodel$fitted
    thetaline2Out <- sesmodel$mean
    #Theta forecasts
    if (model=="A"){
      forecastsIn <- as.numeric(thetaline2In*wses)+as.numeric(thetaline0In*wlrl)+des_input-des_input
      forecastsOut <- as.numeric(thetaline2Out*wses)+as.numeric(thetaline0Out*wlrl)+outtest-outtest
    }else if ((model=="M")&
              (all(thetaline2In>0)==T)&(all(thetaline2Out>0)==T)&
              (all(thetaline0In>0)==T)&(all(thetaline0Out>0)==T)){
      forecastsIn <- ((as.numeric(thetaline2In)^(1/theta))*(as.numeric(thetaline0In)^(1-(1/theta))))+des_input-des_input
      forecastsOut <- ((as.numeric(thetaline2Out)^(1/theta))*(as.numeric(thetaline0Out)^(1-(1/theta))))+outtest-outtest
    }else{
      model<-"A"
      thetalineT <- theta*des_input+(1-theta)*thetaline0In
      sesmodel <- ses(thetalineT,h=fh)
      thetaline2In <- sesmodel$fitted
      thetaline2Out <- sesmodel$mean
      forecastsIn <- as.numeric(thetaline2In*wses)+as.numeric(thetaline0In*wlrl)+des_input-des_input
      forecastsOut <- as.numeric(thetaline2Out*wses)+as.numeric(thetaline0Out*wlrl)+outtest-outtest
    }
    #Seasonal adjustments
    if (seasonality=="A"){
      forecastsIn <- forecastsIn+SIin
      forecastsOut <- forecastsOut+SIout
    }else{
      forecastsIn <- forecastsIn*SIin
      forecastsOut <- forecastsOut*SIout
    }
    #Zero forecasts become positive
    for (i in 1:length(forecastsOut)){
      if (forecastsOut[i]<0){ forecastsOut[i] <- 0 }
    }
    if (plot==TRUE){
      united <- cbind(input,forecastsOut)
      for (ik in 1:(observations+fh)){ united[ik,1] = sum(united[ik,2],united[ik,1], na.rm = TRUE) }
      plot(united[,1],col="black",type="l",main=paste("Model:",model,",Curve:",curve,",Theta:",theta),xlab="Time",ylab="Values",
           ylim=c(min(united[,1])*0.85,max(united[,1])*1.15))
      lines(forecastsIn, col="green") ; lines(forecastsOut, col="green")
      lines(thetaline2In, col="blue") ; lines(thetaline2Out, col="blue")
      lines(thetaline0In, col="red") ; lines(thetaline0Out, col="red")
    }
    output=list(fitted=forecastsIn,mean=forecastsOut,
                fitted0=thetaline0In,mean0=thetaline0Out,
                fitted2=thetaline2In,mean2=thetaline2Out,
                model=paste(seasonality,model,curve,c(round(theta,2))))
    
    return(output)
  }
  # horizon <- input$Horizon
  # frequency <- input$Frequency
  ini <- 1
  # ini <- max(c(origin - 20 * frequency + 1, 1))
  input = ts(input[ini : origin], frequency = frequency)
  # out <- SeasonalAdjusted(y, frequency, horizon)
  #Scale
  base <- mean(input) ; input <- input/base
  molist <- c("M","A") ; trlist <- c("Lrl","Exp")
  #Check seasonality & Create list of models
  ppy <- frequency(input) ; ST <- F
  if (ppy>1){ ST <- SeasonalityTest(input, ppy) }
  if (ST==T){
    selist <- c("M","A")
    listnames <- c()
    for (i in 1:length(selist)){
      for (ii in 1:length(molist)){
        for (iii in 1:length(trlist)){
          listnames <- c(listnames,paste(selist[i], molist[ii], trlist[iii]))
        }
      }
    }
  }else{
    listnames <- c()
    for (ii in 1:length(molist)){
      for (iii in 1:length(trlist)){
        listnames <- c(listnames, paste("N", molist[ii], trlist[iii]))
      }
    }
  }
  modellist <- NULL
  for (i in 1:length(listnames)){
    modellist[length(modellist)+1] <- list(c(substr(listnames,1,1)[i], substr(listnames,3,3)[i],
                                             substr(listnames,5,7)[i]))
  }
  #Start validation
  errorsin <- c() ; models <- NULL
  #With this function determine opt theta per case
  optfun <- function(x, input, fh, curve, model, seasonality){
    mean(abs(Theta.fit(input=input, fh, theta=x, curve, model, seasonality , plot=FALSE)$fitted-input))
  }
  for (j in 1:length(listnames)){
    optTheta <- optimize(optfun, c(1:3),
                         input=input, fh=fh, curve=modellist[[j]][3], model=modellist[[j]][2],
                         seasonality=modellist[[j]][1])$minimum
    fortheta <- Theta.fit(input=input, fh=fh, theta=optTheta, curve=modellist[[j]][3], model=modellist[[j]][2],
                          seasonality=modellist[[j]][1], plot=F)
    models[length(models)+1] <- list(fortheta)
    errorsin <- c(errorsin, mean(abs(input-fortheta$fitted)))
  }
  #Select model and export
  selected.model <- models[[which.min(errorsin)]]
  description <- selected.model$model
  output <- list(fitted=selected.model$fitted*base,mean=selected.model$mean*base,
                 description=description)
  #Returns the fitted and forecasted values, as well as the model used (Type of seasonality, Type of Model, Type of Trend, Theta coef.)
  return(c(list(forecasts= output$mean, model= NA), output))
}

FourThetaARMAM4 <- function(y, origin, horizon, frequency){
  # General template function for forecasting model
  #      y: one time series (list with features)
  #      origin: forecasting origin
  #      horizon: never used
  # REMEMBER y IS A LIST!!!
  # frequency <- y$Frequency
  n <- length(y)
  # origin <- n - horizon
  # ini <- max(c(origin - 20 * frequency + 1, 1))
  ini <- 1
  # x <- y
  x <- y[ini : origin]
  # y <- y[ini : origin]
  # tsdisplay(x$data)
  origin <- length(x)
  model <- FourThetaM4(x, origin, horizon, frequency)
  e <- x[1 : origin] - model$fitted
  if ((origin > 99 && frequency == 12) || (frequency == 4)){
    # ts.plot(x$data[1 : origin], model$fitted)
    # tsdisplay(e)
    mArima <- auto.arima(e, d = 0, D = 0, stationary = TRUE, allowmean = TRUE)
    # summary(mArima)
    arimaFor <- forecast(mArima, h = horizon)
    # plot(arimaFor)
    # print(arimaFor$mean)
    output <- model$forecasts + arimaFor$mean
  } else {
    # ts.plot(cbind(y[(n-horizon + 1) : n], as.matrix(output), as.matrix(model$forecasts)))
    output <- model$forecasts
  }
  
  return(list(forecasts = output, model = NA, e = e))
}

predilabM4naiveSelect <- function(y, origin, horizon, frequency){
  if (origin < 24){
    forecasts <- FourThetaM4(y, origin, horizon, frequency)$forecasts
    ind <- 1
  } else {
    f1 <- naiveSeasonalM4(y, origin - horizon, horizon, frequency)
    f2 <- Naive2M4(y, origin - horizon, horizon, frequency)
    f3 <- SesM4(y, origin - horizon, horizon, frequency)
    f4 <- HoltM4(y, origin - horizon, horizon, frequency)
    f5 <- DampedM4(y, origin - horizon, horizon, frequency)
    f6 <- ThetaM4(y, origin - horizon, horizon, frequency)
    f7 <- FourThetaARMAM4(y, origin - horizon, horizon, frequency)
    actual <- y[(origin - horizon + 1) : origin]
    # sMAPEs
    m1 <- mean(200*abs(f1$forecasts - actual)/(abs(f1$forecasts) + abs(actual)))
    m2 <- mean(200*abs(f2$forecasts - actual)/(abs(f2$forecasts) + abs(actual)))
    m3 <- mean(200*abs(f3$forecasts - actual)/(abs(f3$forecasts) + abs(actual)))
    m4 <- mean(200*abs(f4$forecasts - actual)/(abs(f4$forecasts) + abs(actual)))
    m5 <- mean(200*abs(f5$forecasts - actual)/(abs(f5$forecasts) + abs(actual)))
    m6 <- mean(200*abs(f6$forecasts - actual)/(abs(f6$forecasts) + abs(actual)))
    m7 <- mean(200*abs(f7$forecasts - actual)/(abs(f7$forecasts) + abs(actual)))
    ind <- order(c(m1, m2, m3, m4, m5, m6, m7))  #, m8))
    forecasts <- rep(0, horizon)
    nmodels <- 3
    for (i in 1 : nmodels){
      if (ind[i] == 1){
        forecastsi <- naiveSeasonalM4(y, origin, horizon, frequency)$forecasts
      } else if (ind[i] == 2){
        forecastsi <- Naive2M4(y, origin, horizon, frequency)$forecasts
      } else if (ind[i] == 3){
        forecastsi <- SesM4(y, origin, horizon, frequency)$forecasts
      } else if (ind[i] == 4){
        forecastsi <- HoltM4(y, origin, horizon, frequency)$forecasts
      } else if (ind[i] == 5){
        forecastsi <- DampedM4(y, origin, horizon, frequency)$forecasts
      } else if (ind[i] == 6){
        forecastsi <- ThetaM4(y, origin, horizon, frequency)$forecasts
        # } else if (ind[i] == 7){
        #   forecastsi <- FourThetaM4(y, origin, horizon, frequency)$forecasts
      } else if (ind[i] == 7){
        forecastsi <- FourThetaARMAM4(y, origin, horizon, frequency)$forecasts
      }
      forecasts <- forecasts + forecastsi
    }
    forecasts <- forecasts / nmodels
  }
  # n <- length(y)
  # plot(y, type = "l")
  # lines((n - horizon + 1) : n, forecasts, col = "red")
  return(list(forecasts = forecasts, model = ind))
}

predilab <- function(y, origin, horizon, frequency){
  # origin = length(y)
  # y <- y[1 : origin]
  # Checking integer values
  if (is.integer(y)){
    dec <- 0
  } else {
    dec <- 16
  }
  # Normalising
  cnst <- 10^(log10(mean(y, na.rm = TRUE)) - 2)
  y <- y / cnst
  # Modelling depending on frequency
  if (frequency == 1){
    model<- FourThetaARMAM4(y, origin, horizon, frequency)
  } else if (frequency == 4){
    model<- FourThetaARMAM4(y, origin, horizon, frequency)
  } else if (frequency == 24){
    ddy = y[169:length(y)]-y[1:(length(y)-168)]
    dy = y[25:length(y)]-y[1:(length(y)-24)]
    if (var(ddy) < var(dy[(length(dy) - length(ddy) + 1) : length(dy)])){
      model<- naiveSeasonalM4(y, origin, horizon, 168)
      browser()
    } else {
      model<- naiveSeasonalM4(y, origin, horizon, frequency)
    }
  } else {
    model <- predilabM4naiveSelect(y, origin, horizon, frequency)
  }
  forecasts<- model$forecasts
  return(list(forecasts= round(forecasts * cnst, dec), model= model))
}
