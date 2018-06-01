# The core concept of our forecasting approach is the use of optimum combinations of forecasts... 
# ...for three types of model:s 1-ets (or tbats), 2-auto-arima and... 
# ...3-simple regression for trend and seasonality from the "forecast" package. 
# To find optimum combinations, we used the nmkb (Nelder-Mead) function...
# ... optimization algorithm for derivative-free optimization... 
# ...from the dfoptim package. Weights are defined based on performance on... 
# ...the training time-frame.  Two options are used for different frequencies... 
# ...of data: 1-performance on full sample and 2-combined results of... 
# ...performance on full sample and hold-out sample. Hold-out periods... 
# ...are equal forecasting periods. Models have slightly different... 
# ...specification for different frequencies due to the handling of seasonality,... 
# ...criteria reflecting training options and the handling of "problematic" series etc. 
# We used parallel processing for all calculations. Since the processing... 
# ...time is long and we want to see some intermediate controls,like convergence of... 
# ...optimization. We used intermediate inputs for each frequency of... 
# ...data and then combined the different outputs: forecasts and intervals.
#************References**********************#
# The packages used in this code are:
# 1.	Forecast 8.2 - Forecasting Functions for Time Series and Linear Models: main used package: Author Rob J Hyndman, etc.
# 2.	'dfoptim' version 2016.7-1: Derivative-Free Optimization, Author Ravi Varadhan, Johns Hopkins University, and Hans W. Borchers, ABB Corporate Research. 
# 3.	foreach: simple, scalable parallel programming from Revolution Analytics. Author: Rich Calaway , Microsoft ,Steve Weston .
# 4.	dplyr - A Grammar of Data Manipulation. Author Hadley Wickham, Romain Francois, Lionel Henry, Kirill Müller.
# 5.	doParallel - Foreach Parallel Adaptor for the 'parallel' Package. Author Rich Calaway, Microsoft Corporation, Steve Weston, Dan Tenenbaum 
# We also used customized functions:  'smape_cal' and 'mase_cal' from the M4Competition Update Benchmarks and Evaluation.R, provided by competition orginizers.

#***********Set working directory*********************************************#
setwd("C:/m4comp")#all training data were copied in this folder
#*****************************************************************************#
#Code-block #1, lines 27-67 is used for all series. If series are run at different... 
#...times and R is reopened, code-block #1 should be run each time.
#Load libraries 
library(forecast)
library(dfoptim)
library(foreach)
library(dplyr)
library(doParallel)
#Set-up for parallel processing 
no_cores <- detectCores()-1 #Do not use all cores, thus the use of -1. Computer can become unstable if all cores are used for forecasting.

c1 <- makeCluster(no_cores)
registerDoParallel(c1)
#Connect libraries with clusters.
clusterEvalQ(c1,library(forecast))
clusterEvalQ(c1, library(dfoptim))
#Define a customized short data.frame function. 
df<-data.frame
#Borrow two functions from the "Benchmarked and Evaluations" code provided by organizers.  
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
#End of block 1. This block should be repeated each time you quit and then reopen R.

#Start code for annual frequencies. For annual data only train models on full sample. 
input <- read.csv('Yearly-train.csv')
fh <- 6 #Forecasting horizon. 
frq <- 1 #Data frequency.
start <- Sys.time()#Record the start processing time. 

forres<-foreach(i=1:length(input[,1]))%dopar% {
  #Create specific series for full sample.
  x <- na.omit(ts(t(input[i,2:(length(input[1,]))])))
  
  #Fit series on full sample. 
  
  #Forcast ets.
  
  fitetsf<-ets(x,damped=NULL, alpha=NULL, beta=NULL, gamma=NULL,phi=NULL, 
               opt.crit=c("mae"),ic=c("aic"))
  fitf<-fitted(fitetsf)
  etsfit<-df(fitf)
  
  
  #Forcast arima.
  
  fitarf<-auto.arima(x)
  
  fity<-fitted(fitarf)
  arfit<-df(fity)
  
  # Forcast simple trend.
  
  fitnf<-tslm(formula=x~trend)
  nfit<-fitted(fitnf)
  nfit<-df(nfit)
  #Make optimization function.
  
  fr<-function(z){
    z1<-z[1]
    z2<-z[2]
    z3<-z[3]
    
    
    combfit<-z1*etsfit+z2*arfit+z3*nfit
    
    #Calculate smape and mase for fitting on full sample for combined model.  
    smapef <- mean(smape_cal(x,combfit[,1]))
    masef <- mean(mase_cal(x,x,combfit[,1]))
    #Calculate smape for naive forecast.
    smapenf <- mean(smape_cal(x[2:length(x)],fitted(naive(x))[2:length(x)]))
    #Calculate characteristics, in optimization criteria, for selecting weights for each model.
    #The criteria is a proxy of OWA; we used penalties to avoid significant differences of... 
    #...total weights from 1.This is an alternative to constrained optimization.
    mean(c(smapef/smapenf, masef))+10*abs(z1+z2+z3-1)
  }
#Find optimum weights.  
  opt<-nmkb(c(1/3,1/3,1/3),fr,lower=c(0,0,0),upper=c(1,1,1),control=list(maxfeval=20000))
  
  #Calculate combined forecasts.
  #We used a safety option for vector c(v1,v2,v3) to avoid significant differences between totals of weights and 1.
  #This is in case the penalty in the objective function is not enough. 
  #We used this technique for all series through 100,000 and was found to be significant for only 6 series.
  
  w=cbind(opt$par)
  sump <- sum(opt$par)
  w1<-ifelse(sump>0.95 & sump<1.05,w[1],0.5)
  w2<-ifelse(sump>0.95 & sump<1.05,w[2],0.5)
  w3<-ifelse(sump>0.95 & sump<1.05,w[3],0)
  #This version uses bootstrapping for confidences intervals.
  set.seed(1)
  foretsf <- forecast(fitetsf,h=fh,level=95, bootstrap=TRUE, npaths=5000)
  forarf <-  forecast(fitarf,h=fh,level=95, bootstrap=TRUE, npaths=5000)
  fornf <-  forecast(fitnf,h=fh,level=95)# no bootstrapping for simple trend model
  #Combine forecasts and intervals and make them non-negative.
  meanr <- pmax(0,w1*foretsf$mean+w2*forarf$mean+w3*fornf$mean)
  lowerr <- pmax(0,w1*foretsf$lower+w2*forarf$lower+w3*fornf$lower)
  upperr <- pmax(0,w1*foretsf$upper+w2*forarf$upper+w3*fornf$upper)
  #Keep forecast if all outputs are positive; otherwise replace with naïve forecasts.
  #There were very few cases when replacements needed to be made.
  
  naivef <- naive(x,fh,leve=95) 
  nlower <- pmax(0,naivef$lower)
  
  if (min(meanr)>0) forcombo<-df(meanr,lowerr,upperr)else forcombo <-df(naivef$mean,nlower,naivef$upper) 
  names(forcombo) <- c(names(forarf[4:6]))
  
  res <- df(input[i,1],opt$convergence,opt$value,sump,forcombo)
}
end <- Sys.time()#Record end processing time. 
print(end-start)#Calculate processing time.
resan<-df(t(df(forres)))
write.csv(resan,'annual_f.csv')#write output.
#**************************************************#
#Clean environment before continuing on to the next step.
rm(list=ls()[! ls() %in% c('c1','df','mase_cal','smape_cal')])

#Model specifications for quarterly and monthly data are the same so we can create... 
#...one function to use for both forecasts. If you quit and then reopen R, this first code-block should be run again.
#*************************************************#

#This is the function for forecasts with quarterly and monthly frequencies.

for_qt_m <- function(input,fh,frq){
  #Create specific series for  full sample, short sample, out of sample.
  x <- na.omit(ts(t(input[i,2:(length(input[1,]))]),freq=frq))
  xs <-ts(x[1:(length(x)-fh)],freq=frq)
  xo <- ts(tail(x,fh),freq=frq,start=end(xs)+1)
  #Fit on short sample and forecast for hold-out (hold-out equal forecast period) and fit on full sample.
  
  #For ets.
  fitetss<-ets(xs,damped=NULL, alpha=NULL, beta=NULL, gamma=NULL,phi=NULL, 
               opt.crit=c("mae"),ic=c("aic")) 
  fitetsf<-ets(x,damped=NULL, alpha=NULL, beta=NULL, gamma=NULL,phi=NULL, 
               opt.crit=c("mae"),ic=c("aic"))
  fitf<-fitted(fitetsf)
  etsfit<-df(fitf)
  forsh<-forecast(fitetss,fh)
  etsfors<-df(forsh)
  
  #For arima.
  fitars<-auto.arima(xs)
  fitarf<-auto.arima(x)
  
  fity<-fitted(fitarf)
  arfit<-df(fity)
  forar <-forecast(fitars,fh) 
  arfors <- df(forar)
  #For simple trend plus cycle.
  
  fitnf<-tslm(formula=x~trend+season)
  nfit<-fitted(fitnf)
  nfit<-df(nfit)
  fitns<-tslm(formula=xs~trend+season)
  forn <- forecast(fitns,h=fh)
  nfors <- df(forn)
  fr<-function(z){
    z1<-z[1]
    z2<-z[2]
    z3<-z[3]
    
    combfit<-z1*etsfit+z2*arfit+z3*nfit
    combfors <- z1*etsfors+z2*arfors+z3*nfors 
    #calculate smape and mase for fitting on full sample for combine model 
    smapef <- mean(smape_cal(x,combfit[,1]))
    masef <- mean(mase_cal(x,x,combfit[,1]))
    #Calculate smape and for fitting on full sample for naive model; mase for naive is 1.
    smapenf <- mean(smape_cal(x[(frq+1):length(x)],fitted(snaive(x))[(frq+1):length(x)]))
    
    #Calculate smape and mase for hold-out forecast for our model.  
    smapeh <- mean(smape_cal(xo,combfors[,1]))
    maseh <- mean(mase_cal(xs,xo,combfors[,1]))
    maseh <- ifelse(is.finite(maseh),maseh,0)
    
    #Use calculated characteristics in optimization criteria for selecting weights for each model.
    #The criteria is a proxy of OWA.
    0.5*mean(c(smapef/smapenf, masef))+0.5*mean(c(smapeh/smapenf,maseh))+10*abs(z1+z2+z3-1)
    
  }
  
  opt<-nmkb(c(1/3,1/3,1/3),fr,lower=c(0,0,0),upper=c(1,1,1),control=list(maxfeval=20000))
  
  #Calculate combined forecasts.
  #We used safety option for vector c(w1,w2,w3) to avoid significant differences between totals of weights and 1.
  #This is in case a penalty in objective function is not enough. 
  #We used this for all series through 100,000 and it was significant only for 6 series.
  w=cbind(opt$par)
  sump <- sum(opt$par)
  w1<-ifelse(sump>0.95 & sump<1.05,w[1],0.5)
  w2<-ifelse(sump>0.95 & sump<1.05,w[2],0.5)
  w3<-ifelse(sump>0.95 & sump<1.05,w[3],0)
  set.seed(1951)
  foretsf <- forecast.ets(fitetsf,h=fh,level=95,bootstrap=TRUE, npaths=5000)
  forarf <-  forecast(fitarf,h=fh,level=95,bootstrap=TRUE, npaths=5000)
  fornf <-  forecast(fitnf,h=fh,level=95)
  #Combine forecasts and intervals. 
  meanr <- w1*foretsf$mean+w2*forarf$mean+w3*fornf$mean
  lowerr <- pmax(0,w1*foretsf$lower+w2*forarf$lower+w3*fornf$lower)
  upperr <- pmax(0,w1*foretsf$upper+w2*forarf$upper+w3*fornf$upper)
  #Keep forecast if all outputs are positive; otherwise replace with naïve forecasts.
  #There are very few cases when replacements need to be made.
  naivef <- naive(x,fh,leve=95) 
  nlower <- pmax(0,naivef$lower)
  
  if (min(meanr)>0) forcombo<-df(meanr,lowerr,upperr)else forcombo <-df(naivef$mean,nlower,naivef$upper) 
  names(forcombo) <- c(names(forarf[4:6]))
  
  res <- df(input[i,1],opt$convergence,opt$value,sump,forcombo)
}
#**********************End of function******************************# 
#Read data in and use 'fuction_qt_m' to create quaterly forecasts.

input <- read.csv('Quarterly-train.csv')
start <- Sys.time()#Record start time. 
forres<-foreach(i=1:length(input[,1]))%dopar% {for_qt_m(input,8,4)}
end <- Sys.time()#This line is used for recording the end of the processing time. 
print(end-start)# Subtract the start of processing from the end of processing to get total processing time.
resq<-df(t(df(forres)))
write.csv(resq,'quat_f.csv')#Write output.
rm(resq,forres)

#************End of quaterly forecast****************************#
#Read data in and use fuction_qt_m to create monthly forecasts.

input <- read.csv('Monthly-train.csv')
start <- Sys.time()#just for simple time control
forres<-foreach(i=1:length(input[,1]))%dopar% {for_qt_m(input,18,12)}
end <- Sys.time()
print(end-start)#Get processing time.
resm<-df(t(df(forres)))
write.csv(resm,'month_f.csv')#Write output.
rm(resm,forres)

#Make forecasts for weekly series; trainig only on full sample.
input <- read.csv('Weekly-train.csv')
fh <- 13 #The forecasting horizon 
frq <- 52 #The frequency of the data
start <- Sys.time()

forres<-foreach(i=1:length(input[,1]))%dopar% tryCatch({
 #Make series on full sample.
  x <- na.omit(ts(t(input[i,2:(length(input[1,]))]),freq=frq))
  
  #Use tbats for ets (tbats-very slow).
  
  fitetsf<-tbats(x)
  fitf<-fitted(fitetsf)
  etsfit<-df(fitf)
  
  
  #For arima use fourier for seasonality.
  bestfit <- list(aicc=Inf)
  for(j in 1:25)
  {
    fit <- auto.arima(x, xreg=forecast::fourier(x,K=j),seasonal=FALSE)
    if(fit$aicc < bestfit$aicc)
      bestfit <- fit
    else break;
    #Forecast series just for future use.
    forarf<-forecast(bestfit,xreg=forecast::fourier(x,K=j,h=13),level=95)
 }  
  arfit<-fitted(bestfit,xreg=forecast::fourier(x,K=j))
  arfit <- df(arfit)
  #Forcast naive.
  
  fitnf<-tslm(formula=x~trend+season)
  nfit<-fitted(fitnf)
  nfit<-df(nfit)
  
  fr<-function(z){
    z1<-z[1]
    z2<-z[2]
    z3<-z[3]
    
    
    combfit<-z1*etsfit+z2*arfit+z3*nfit
    
    #Calculate smape and mase for fitting on full sample for combine model. 
    smapef <- mean(smape_cal(x,combfit[,1]))
    masef <- mean(mase_cal(x,x,combfit[,1]))
    #Calculate smape and for fitting on full sample for naive model; mase for naive is 1.
    smapenf <- mean(smape_cal(x[(frq+1):length(x)],fitted(snaive(x))[(frq+1):length(x)]))
    
    
    #Use calculated characteristics in optimization criteria for selecting weights for each model.
    #The criteria is a proxy of OWA
    mean(c(smapef/smapenf, masef))+10*abs(z1+z2+z3-1)
  }
  
  opt<-nmkb(c(1/3,1/3,1/3),fr,lower=c(0,0,0),upper=c(1,1,1),control=list(maxfeval=20000))
  
  #Calculate combined forecasts.
  w=cbind(opt$par)
  w1<-w[1]
  w2<-w[2]
  w3<-w[3]
  foretsf <- forecast(fitetsf,h=fh,level=95)
  #Forecast naive.
  
  fornf <-  forecast(fitnf,h=fh,level=95)
  #Combine forecasts and intervals and make them non-negative.
  meanr <- pmax(0,w1*foretsf$mean+w2*forarf$mean+w3*fornf$mean)
  lowerr <- pmax(0,w1*foretsf$lower+w2*forarf$lower+w3*fornf$lower)
  upperr <- pmax(0,w1*foretsf$upper+w2*forarf$upper+w3*fornf$upper)
  forcombo<- df(meanr,lowerr,upperr)
  names(forcombo) <- c(names(forarf[4:6]))
  res <- df(input[i,1],opt$convergence,opt$value,forcombo)
},error=function(e){print(i)})
end <- Sys.time()
print(end-start)
resweekly<-df(t(df(forres)))
write.csv(resweekly,'weekly_f.csv')

#Create function for daily and hourly forecasts.

for_d_h <- function(input,fh,frq,kl){
  #Create specific series for  full sample, short sample,out of sample.
  x <- na.omit(ts(t(input[i,2:(length(input[1,]))]),freq=frq))
  xs <-ts(x[1:(length(x)-fh)],freq=frq)
  xo <- ts(tail(x,fh),freq=frq)
  
  #Fit on short sample and forecast for hold out (hold out equal forecast period) and fit on full sample.
  
  #Forcast ets.
  fitetss<-tbats(xs)
  fitetsf<-tbats(x)
  fitf<-fitted(fitetsf)
  etsfit<-df(fitf)
  forsh<-forecast(fitetss,fh)
  etsfors<-df(forsh)
  
  #Forcast arima.
  bestfit <- list(aicc=Inf)
  for(j in 1:kl)
  {
    fit <- auto.arima(x, xreg=forecast::fourier(x,K=j),seasonal=FALSE)
    if(fit$aicc < bestfit$aicc)
      bestfit <- fit
    else break;
    #Forecast series just in case for future use.
    
  }
  fitarf<-fitted(bestfit,xreg=forecast::fourier(x,K=j)) 
  arfit <- df(fitarf)
  bestfitf <- list(aicc=Inf)
  for(k in 1:kl)
  {
    fit <- auto.arima(xs, xreg=forecast::fourier(xs,K=k),seasonal=FALSE)
    if(fit$aicc < bestfitf$aicc)
      bestfitf <- fit
    else break;
    #Forecast series just in case for future use.
    
    forar<-forecast(bestfitf,xreg=forecast::fourier(xs,K=k,h=fh))
  } 
  
  arfors <- df(forar)
  #Forcast naive.
  
  fitnf<-tslm(formula=x~trend+season)
  nfit<-fitted(fitnf)
  nfit<-df(nfit)
  fitns<-tslm(formula=xs~trend+season)
  forn <- forecast(fitns,h=fh)
  nfors <- df(forn)
  fr<-function(z){
    z1<-z[1]
    z2<-z[2]
    z3<-z[3]
    
    
    combfit<-z1*etsfit+z2*arfit+z3*nfit
    combfors <- z1*etsfors+z2*arfors+z3*nfors 
    #Calculate smape and mase for fitting on full sample for our model.  
    smapef <- mean(smape_cal(x,combfit[,1]))
    masef <- mean(mase_cal(x,x,combfit[,1]))
    #Calculate smape and for fitting on full sample for naive model; mase for naive is 1.
    smapenf <- mean(smape_cal(x[(frq+1):length(x)],fitted(snaive(x))[(frq+1):length(x)]))
    
    #Calculate smape and mase for hold out forecast for our model.  
    smapeh <- mean(smape_cal(xo,combfors[,1]))
    maseh <- mean(mase_cal(xs,xo,combfors[,1]))
    maseh <- ifelse(is.finite(maseh),maseh,0)
    
    #Use calculated characteristics in optimization criteria for selecting weights for each model.
    #The criteria is a proxy of OWA.
    0.5*mean(c(smapef/smapenf, masef))+0.5*mean(c(smapeh/smapenf,maseh))+10*abs(z1+z2+z3-1)
    
  }
  
  opt<-nmkb(c(1/3,1/3,1/3),fr,lower=c(0,0,0),upper=c(1,1,1),control=list(maxfeval=20000))
  
  #Calculate combined forecasts.
  w=cbind(opt$par)
  sump <- sum(opt$par)

  w1<-ifelse(sump>0.95 & sump<1.05,w[1],0.5)
  w2<-ifelse(sump>0.95 & sump<1.05,w[2],0.5)
  w3<-ifelse(sump>0.95 & sump<1.05,w[3],0)
  foretsf <- forecast(fitetsf,h=fh,level=95)
  forarf <-  forecast(fitarf,h=fh,level=95)
  fornf <-  forecast(fitnf,h=fh,level=95)
  meanr <- w1*foretsf$mean+w2*forarf$mean+w3*fornf$mean
  lowerr <- pmax(0,w1*foretsf$lower+w2*forarf$lower+w3*fornf$lower)
  upperr <- pmax(0,w1*foretsf$upper+w2*forarf$upper+w3*fornf$upper)
  #Keep forecast if all outputs are positive; otherwise replace with naïve forecasts.
  #There are very few cases when replacements need to be made.
  naivef <- snaive(x,fh,leve=95) 
  nlower <- pmax(0,naivef$lower)
  nupper <- df(naivef$upper)
  names(nupper)[1] <- 'upper'
  
  if (min(meanr)>0) forcombo<-df(mean=meanr,lower=lowerr,upper=upperr)else forcombo <-df(mean=naivef$mean,lower=nlower,nupper) 
  
  
  res <- df(input[i,1],opt$convergence,opt$value,sump,forcombo)
}

#Make daily forecast.

input <- read.csv('Daily-train.csv')
start <- Sys.time()#Record start time. 
forres<-foreach(i=1:length(input[,1]))%dopar% {for_d_h(input,14,7,3)}
end <- Sys.time()#Record end time. 
print(end-start)#Calculate processing time.
resdaily <- df(t(df(forres)))
write.csv(resdaily,'daily_f.csv')
rm(resdaily ,forres)

#Make hourly forecast.

input <- read.csv('Hourly-train.csv')
start <- Sys.time()#Record start time.
forres<-foreach(i=1:length(input[,1]))%dopar% {for_d_h(input,48,24,12)}
end <- Sys.time()#Record end time.
print(end-start)#Calculate processing time. 
reshourly <- df(t(df(forres)))
write.csv(reshourly,'hourly_f.csv')

rm(reshourly ,forres)
#########################################################
#Combine all results and format.
#We assume that this part is run separately. 
setwd("C:/m4comp")
library(dplyr)
library(gtools)

df <- data.frame
#Combine all results and format.
anfor <- read.csv('annual_f.csv',stringsAsFactors = F)
qtfor <- read.csv('quat_f.csv',stringsAsFactors = F)
monthfor <- read.csv('month_f.csv',stringsAsFactors = F)
wkfor <- read.csv('weekly_f.csv',stringsAsFactors = F)
dayfor <- read.csv('daily_f.csv',stringsAsFactors = F)
hrfor <- read.csv('hourly_f.csv',stringsAsFactors = F)
#Create outputs for point forecast, upper and lower limits.
#Annual
anmean <- df(filter(anfor,substr(X,1,4)=='mean'))
anupper <- df(filter(anfor,substr(X,1,5)=='upper'))
anlower <- df(filter(anfor,substr(X,1,5)=='lower'))
id <-  filter(anfor,substr(X,1,5)=='input')[,2]

anmeanf <- df(id,anmean[,2:7])
anupperf<- df(id,anupper[,2:7])
anlowerf <- df(id,anlower[,2:7])

#Quaterly
qtmean <- df(filter(qtfor,substr(X,1,4)=='mean'))
qtupper <- df(filter(qtfor,substr(X,1,5)=='upper'))
qtlower <- df(filter(qtfor,substr(X,1,5)=='lower'))
id <-  filter(qtfor,substr(X,1,5)=='input')[,2]

qtmeanf <- df(id,qtmean[,2:9])
qtupperf<- df(id,qtupper[,2:9])
qtlowerf <- df(id,qtlower[,2:9])


#Monthly
monthmean <- df(filter(monthfor,substr(X,1,4)=='mean'))
monthupper <- df(filter(monthfor,substr(X,1,5)=='upper'))
monthlower <- df(filter(monthfor,substr(X,1,5)=='lower'))
id <-  filter(monthfor,substr(X,1,5)=='input')[,2]

monthmeanf <- df(id,monthmean[,2:19])
monthupperf<- df(id,monthupper[,2:19])
monthlowerf <- df(id,monthlower[,2:19])


#Weekly
wkmean <- df(filter(wkfor,substr(X,1,4)=='mean'))
wkupper <- df(filter(wkfor,substr(X,1,5)=='upper'))
wklower <- df(filter(wkfor,substr(X,1,5)=='lower'))
id <-  filter(wkfor,substr(X,1,5)=='input')[,2]

wkmeanf <- df(id,wkmean[,2:14])
wkupperf<- df(id,wkupper[,2:14])
wklowerf <- df(id,wklower[,2:14])

#Daily
daymean <- df(filter(dayfor,substr(X,1,4)=='mean'))
dayupper <- df(filter(dayfor,substr(X,1,5)=='upper'))
daylower <- df(filter(dayfor,substr(X,1,5)=='lower'))
id <-  filter(dayfor,substr(X,1,5)=='input')[,2]

daymeanf <- df(id,daymean[,2:15])
dayupperf<- df(id,dayupper[,2:15])
daylowerf <- df(id,daylower[,2:15])


#Hourly
hrmean <- df(filter(hrfor,substr(X,1,4)=='mean'))
hrupper <- df(filter(hrfor,substr(X,1,5)=='upper'))
hrlower <- df(filter(hrfor,substr(X,1,5)=='lower'))
id <-  filter(hrfor,substr(X,1,5)=='input')[,2]

hrmeanf <- df(id,hrmean[,2:49])
hrupperf<- df(id,hrupper[,2:49])
hrlowerf <- df(id,hrlower[,2:49])

#Combine all
point_for <- bind_rows(anmeanf,qtmeanf,monthmeanf,wkmeanf,daymeanf,hrmeanf)
names(point_for)[2:49] <- paste('F',1:48,sep='')

write.csv(point_for,'alex_point_for.csv',row.names = F)

upper_for <- bind_rows(anupperf,qtupperf,monthupperf,wkupperf,dayupperf,hrupperf)
names(upper_for)[2:49] <- paste('F',1:48,sep='')

write.csv(upper_for,'alex_upper_for.csv',row.names = F)

lower_for <- bind_rows(anlowerf,qtlowerf,monthlowerf,wklowerf,daylowerf,hrlowerf)
names(lower_for)[2:49] <- paste('F',1:48,sep='')

write.csv(lower_for,'alex_lower_for.csv',row.names = F)


