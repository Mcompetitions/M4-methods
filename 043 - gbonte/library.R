detectSeason<-function(TS,maxs=10){
  seas=0
  trnd=lm(TS ~ seq(TS))$fit
  for (add in c(0,1)){
    if (add==1)
      S<-TS/trnd
    if (add==0)
      S<-TS-trnd
    
    PVS=numeric(maxs)+Inf
    for (s in 2:maxs){
      PV=NULL
      m_S = t(matrix(data = S, nrow = s))
      for (i in 1:s){
        for (j in setdiff(1:s,i)){
          xs=m_S[,i]
          ys=unlist(m_S[,j])
          PV=c(PV,t.test(xs,ys)$p.value)
        }#
        
      }#
      PVS[s]=median(PV)
    }# for s
    
    if (min(PVS)<0.05){
      seas=which.min(PVS)
      return(seas)
    }# if min
  }#
  
  return(seas)
}#



  

detectSeason2<-function(TS,maxs=10){
  seas=0
  PVS=numeric(maxs)+Inf
  for (s in 2:maxs){
    PV=NULL
    m_S = t(matrix(data = TS, nrow = s))
    for (i in 1:s){
      for (j in setdiff(1:s,i)){
        xs=m_S[,i]
        ys=unlist(m_S[,j])
        PV=c(PV,t.test(xs,ys)$p.value)
      }#
      
    }
    PVS[s]=median(PV)
  }
 
  if (min(PVS)<0.05){
    seas=which.min(PVS)
   
  }
  return(seas)
}


removeSeas<-function(TS,seas){
  N=length(TS)
  m_S = t(matrix(data = TS, nrow = seas))
  TSeas0=apply(m_S,2,mean)
  TSeas=rep(TSeas0,length.out=N)
  TSdeseas=TS-TSeas
  return(list(TSdeseas=TSdeseas,TSeas0=TSeas0))

}#


differ<-function(S){
  
  list(D=diff(S),last=S[length(S)])
}

differB<-function(D,last){
  return(last+cumsum(D))
}

library(forecast) #Requires v8.2

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

FourTheta<- function(input, fh){
  #Used to automatically select the best Theta model
  
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
  
  return(output)
  
}

#################################################################################
#In this example let us produce forecasts for 100 randomly generated timeseries
fh <- 6 #The forecasting horizon examined
frq <- 1 #The frequency of the data
data_train = data_test <- NULL #Train and test sample
for (i in 1:100){
  data_all <- 2+ 0.15*(1:20) + rnorm(20) 
  data_train[length(data_train)+1] <- list(ts(head(data_all,length(data_all)-fh),frequency = frq))
  data_test[length(data_test)+1] <- list(tail(data_all,fh))
}
#################################################################################

smape_cal <- function(outsample, forecasts){
  #Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts))
  return(smape)
}

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

naive_seasonal <- function(input, fh){
  #Used to estimate Seasonal Naive
  frcy <- frequency(input)
  frcst <- naive(input, h=fh)$mean 
  if (frcy>1){ 
    frcst <- head(rep(as.numeric(tail(input,frcy)), fh), fh) + frcst - frcst
  }
  return(frcst)
}

Theta.classic <- function(input, fh,theta=2){
  #Used to estimate Theta classic
  
  #Set parameters
  wses <- wlrl<-0.5 ; 
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




StatPredictors1 <- function(input, fh, index=0,verbose=F){
  #Used to estimate the statistical benchmarks of the M4 competition
  
    ##Estimate seasonaly adjusted time series
    if (index>=10)
        return(numeric(fh))
    
    ppy <- frequency(input) ; ST <- F
    if (ppy>1)
    { ST <- SeasonalityTest(input,ppy) }
    if (ST==T){
        Dec <- decompose(input,type="multiplicative")
        des_input <- input/Dec$seasonal
        SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
    }else{
        des_input <- input ; SIout <- rep(1, fh)
    }

    
    

    if (index==1){
        f1 <- naive(input, h=fh)$mean #Naive
        if (verbose)
            print("Naive")
        
        return(as.numeric(f1))
    }#
    
    if (index==2){
        f2 <- naive_seasonal(input, fh=fh) #Seasonal Naive
        if (verbose)
            print("Seas Naive")
        return(as.numeric(f2))
    }#
    
    if (index==3){
        f3 <- naive(des_input, h=fh)$mean*SIout #Naive2
        if (verbose)
            print("Seas Naive2")
        return(as.numeric(f3))
    }#


    if (index==4){
        f4 <- ses(des_input, h=fh)$mean*SIout #Ses
        if (verbose)
            print("Ses")
        return(as.numeric(f4))
    }#

    if (index==5){
        f5 <- holt(des_input, h=fh, damped=F)$mean*SIout #Holt
        if (verbose)
            print("Holt")
        return(as.numeric(f5))
    }#
    
    
    if (index==6){
        f6 <- holt(des_input, h=fh, damped=T)$mean*SIout #Damped
        if (verbose)
            print("Damped")
        return(as.numeric(f6))
    }#
  
    if (index==7){
        f7 <- Theta.classic(input=des_input, fh=fh)$mean*SIout #Theta
        if (verbose)
            print("Theta")
        return(as.numeric(f7))
    }#
    
    
    
    if (index==8){
        f4 <- ses(des_input, h=fh)$mean*SIout #Ses
        f5 <- holt(des_input, h=fh, damped=F)$mean*SIout #Holt
        f6 <- holt(des_input, h=fh, damped=T)$mean*SIout #Damped
        f8 <- (f4+f5+f6)/3 #Comb
        return(f8)
    }#

    if (index==9){
        res <- try(as.numeric(FourTheta(des_input,fh)$mean))
        if(inherits(res, "try-error")){
            ##error handling code, maybe just skip this iteration using
            return(numeric(fh))
        }
    
        
        return(res)
    }#
    


}#
  

