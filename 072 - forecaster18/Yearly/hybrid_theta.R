Forecast_ThetaSm <- function(y, h=NULL, model=NULL, theta=NULL, type=NULL){
  #Applies the forecasting approach described in
  #E. Spiliotis, V. Assimakopoulos & K. Nikolopoulos
  #Forecasting with a hybrid method utilizing data smoothing, a variation of the Theta method and shrinkage of seasonal factors
  #International Journal of Production Economics ; 2018
  
  # 'h' is the forecasting horizon (number of periods for forecasting). The default is h=3.
  
  # 'theta' is the theta parameter of the method. Should be greater than 1. The default is theta=2.
  
  # 'model' is the curve used for identifying the long-term trend of the data. Can be "Lin", "Exp", "Log", "Inv" and "Pow" 
  #for linear, exponential, logarithmic, inverse and power curve, respectively. The default is model="Exp".
  
  # 'type' denotes the decomposition method used for seasonal adjustments. It can be "MillerWilliams", "JamesStein", "LemonKrutchkoff" 
  #or "Classical". The last one does not apply shrinkage of seasonal factors. The default is type="MillerWilliams"
  
  if (is.null(h)){ h<-3 }
  if (is.null(type)){ type<-"MillerWilliams" }
  if (is.null(model)){ model<-"Exp" }
  if (is.null(theta)){ theta<-2 }
  
  tryCatch({SD <- SeasonalDec(y,h,frequency,type) #Apply seasonal decomposition with shrinkage
  SA_ts <- SD[[3]] #This is the seasonal adjusted time series
  Sm_ts <- NLS_Max(SA_ts,limit=1,cut=50) #This is the smoothed time series
  forecasts <- ThetaG(Sm_ts, fh=h, theta, model, plot=FALSE)$mean*SD[[2]] #forecast and re-seasonalize your data
  
  return(forecasts)},error=function(e){return(NA)})
}

JamesStein <- function(insample,frequency){
  
  #Shrink seasonal indexes according to the James-Stein approach
  
  SD <- DecomposeC(insample,frequency)
  SI <- SD$Seasonality[1:frequency]
  SR <- na.omit(SD)
  
  kapas <- NULL
  for (j in 1:frequency){ kapas[[length(kapas)+1]] <- SR[SR$Period==j,]$SR }
  
  V <- 0
  for (j in 1:frequency){
    subsum<-0
    for (k in 1:length(kapas[[j]])){ subsum <- subsum+(kapas[[j]][k]-SI[j])^2 }
    subsum <- subsum/(length(kapas[[j]])*(length(kapas[[j]])-1))
    V <- V+subsum
  }
  V <- V/frequency
  
  A <- 0
  for (i in 1:frequency){ A <- A+(SI[i]-1)^2 }
  A <- max((A/(frequency-1))-V,0)
  W <- ((frequency-3)/(frequency-1))*(V/(V+A)) 
  S <- W+(1-W)*SI
  
  SD$SeasonalityClassic <- SD$Seasonality
  for(i in 1:frequency){ SD[SD$Period==i,]$Seasonality <- S[i] }
  
  return(SD)
}

LemonKrutchkoff <- function(insample,frequency){
  
  #Shrink seasonal indexes according to the Lemon-Krutchkoff approach
  
  SD <- DecomposeC(insample, frequency)
  SI <- SD$Seasonality[1:frequency]
  SR <- na.omit(SD)
  
  kapas <- NULL
  for (j in 1:frequency){ kapas[[length(kapas)+1]] <- SR[SR$Period==j,]$SR }
  
  V <- 0
  for (j in 1:frequency){
    subsum <- 0
    for (k in 1:length(kapas[[j]])){ subsum <- subsum+(kapas[[j]][k]-SI[j])^2 }
    subsum<-subsum/(length(kapas[[j]])*(length(kapas[[j]])-1))
    V <- V+subsum
  }
  sigma <- (V/frequency)^0.5
  
  lamdas = W <- matrix(0,nrow=frequency, ncol=frequency)
  for (i in 1:frequency){
    for(j in 1:frequency){
      lamdas[i,j] <- exp((((SI[i]-SI[j])/sigma)^2)*(-0.5))/(sigma*((2*pi)^0.5))
    }
  }
  
  for(jt in 1:frequency){
    for (j in 1:frequency){
      W[jt,j] <- lamdas[jt,j]/sum(lamdas[jt,])
    }
  }
  
  S <- c()
  for (jt in 1:frequency){
    subsum<-0
    for (j in 1:frequency){
      subsum <- subsum+SI[j]*W[jt,j]
    }
    S <- c(S,subsum)
  }
  
  SD$SeasonalityClassic <- SD$Seasonality
  for(i in 1:frequency){ SD[SD$Period==i,]$Seasonality <- S[i] }
  
  return(SD)
}

MillerWilliams <- function(insample, frequency){
  
  #Shrink seasonal indexes according to the Miller-Williams approach
  
  SD <- DecomposeC(insample, frequency)
  SI <- SD$Seasonality[1:frequency]
  SR <- na.omit(SD)
  
  #Calculate skewness
  sub1=sub2<-0
  for (i in 1:frequency){
    sub1 <- sub1+(SI[i]-mean(SI))^3
    sub2 <- sub2+(SI[i]-mean(SI))^2
  }
  skewness <- (sub1/frequency)/((sub2/frequency)^1.5)
  
  #Calculate James-Stein Weight
  kapas<-NULL
  for (j in 1:frequency){ kapas[[length(kapas)+1]] <- SR[SR$Period==j,]$SR }
  V <- 0
  for (j in 1:frequency){
    subsum<-0
    for (k in 1:length(kapas[[j]])){ subsum <- subsum+(kapas[[j]][k]-SI[j])^2 }
    subsum <- subsum/(length(kapas[[j]])*(length(kapas[[j]])-1))
    V <- V+subsum
  }
  V <- V/frequency
  A <- 0
  for (i in 1:frequency){ A <- A+(SI[i]-1)^2 }
  A <- max((A/(frequency-1))-V,0)
  WJS <- ((frequency-3)/(frequency-1))*(V/(V+A))
  
  #Select proper method
  if ((skewness<0.5)&(WJS<0.2)){
    SD <- DecomposeC(insample, frequency)
  }else if ((skewness<0.5)&(WJS>=0.2)&(WJS<0.5)){
    SD <- JamesStein(insample, frequency)
  }else if ((skewness<0.5)&(WJS>=0.5)){
    SD <- JamesStein(insample, frequency)
  }else if ((skewness>=0.5)&(WJS<0.2)){
    SD <- LemonKrutchkoff(insample, frequency)
  }else if ((skewness>=0.5)&(WJS>=0.2)&(WJS<0.5)){
    SD <- LemonKrutchkoff(insample, frequency)
  }else{
    SD <- JamesStein(insample, frequency)
  }
  
  return(SD)
}

SeasonalityTest <- function( insample, ppy, tcrit){
  
  #Autocorrelation test for deciding whether seasonality is present
  
  if (length(insample)<3*ppy){
    test_seasonal <- FALSE
  }else{
    xacf <- acf(insample, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(insample)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] ) 
    
    if (is.na(test_seasonal)==TRUE){ test_seasonal = FALSE }
  }
  
  return(test_seasonal)
}

SeasonalDec<-function(insample, horizon, frequency, type){
  
  #Performs seasonal decomposition, either Classic or using shrikage, according to the "type" argument used
  #It also returns the seasonal indexes that will be needed latter for re-seasonalizing the time series
  #Returns (1) insample seasonal indexes, (1) outsample seasonal indexes, (1) seasonal adjusted time series
  
  tcrit <- 1.645 #90% confidence interval that seasonality exists
  seasonal <- matrix(1, ncol=1, nrow=horizon)   #seasonality indexes for forecasting (1 if non seasonal)
  Seasonal_Indexes <- matrix(1,ncol=1, nrow=length(insample))  #seasonality indexes for the insample model (1 if non seasonal)
  Des_insample <- insample #Deseasonalized t-s (equals insample if non seasonal)
  if (frequency>1){
    test_seasonal <- SeasonalityTest(insample, frequency, tcrit)
    if (test_seasonal==TRUE){
      if (type=="JamesStein"){
        Seasonal_Indexes <- JamesStein(insample, frequency)$Seasonality
      }else if (type=="LemonKrutchkoff"){
        Seasonal_Indexes <- LemonKrutchkoff(insample, frequency)$Seasonality
      }else if (type=="MillerWilliams"){
        Seasonal_Indexes <- MillerWilliams(insample, frequency)$Seasonality
      }else{
        Seasonal_Indexes <- DecomposeC(insample, frequency)$Seasonality
      }
      Des_insample <- insample/Seasonal_Indexes
      for (s in 1:horizon){  
        seasonal[s] <- Seasonal_Indexes[length(insample)-2*frequency+s]
      }
    }
  }
  return(list(Seasonal_Indexes, seasonal, Des_insample))
}

DecomposeC = function(insample,frequency){
  
  #Classic multiplicative seasonal decomposition by moving averages
  
  frame <- data.frame(matrix(data = NA, nrow = length(insample), ncol = 1, byrow = FALSE,dimnames = NULL))
  colnames(frame)<-c("ID"); frame$Data<-insample
  
  ID <- c(); IDref<-c(1:frequency) # which month is this observation?
  for (i in 1:(length(insample)%/%frequency)){ ID<-c(ID,IDref) }
  ID <- c(ID,head(IDref,(length(insample)%%frequency))) ;frame$ID<-ID
  
  
  if (frequency==1){
    frame$Seasonality <- 1
    frame$kmo <- NA # moving average based on frequency
  }else{
    if (frequency%%2==0){
      frame$kmo <- ma(insample, order=frequency, centre=TRUE)
    }else{
      frame$kmo <- ma(insample, order=frequency, centre=FALSE)
    }
    
  }
  
  #Calculate SR and SI
  SRTable<-1
  if (frequency>1){
    frame$LE<-frame$Data/frame$kmo
    LE<-matrix(data = NA, nrow = frequency, ncol = 2, byrow = FALSE,dimnames = NULL)
    LE<-data.frame(LE); colnames(LE)<-c("ID","LE"); LE$ID<-c(1:frequency)
    
    if (length(frame[ is.na(frame$LE)==FALSE, ]$LE)>=3*frequency){
      for (i in 1:frequency){
        LE$LE[i]<-mean(frame$LE[ (frame$ID==i) & (is.na(frame$LE)==FALSE) & (frame$LE<max(frame$LE[(frame$ID==i)&(is.na(frame$LE)==FALSE)])) & (frame$LE>min(frame$LE[(frame$ID==i)&(is.na(frame$LE)==FALSE)])) ])
        if (is.na(LE$LE[i])){
          LE$LE[i]<-mean(frame$LE[ (frame$ID==i) & (is.na(frame$LE)==FALSE)  ])
        }
      }
    }else{
      for (i in 1:frequency){
        LE$LE[i]<-median(frame$LE[ (frame$ID==i) & (is.na(frame$LE)==FALSE) ])
      }
    }
    
    SRTable<-frame$LE
    
    sndarize=mean(LE$LE) ; LE$LE<-LE$LE/sndarize ;  frame$LE<-NULL; frame$kmo<-NA
    DE<-c(); DEref<-LE$LE
    for (i in 1:(length(insample)%/%frequency)){ DE<-c(DE,DEref) }
    DE<-c(DE,head(DEref,(length(insample)%%frequency))) ; frame$Seasonality<-DE
  }
  
  if (is.na(frame$Seasonality)[1]==TRUE){ frame$Seasonality<-1 }
  frame$Deseasonalized<-frame$Data/frame$Seasonality
  
  #Calculate Randomness
  frame$kmo<-ma(frame$Deseasonalized,order=3,centre=FALSE)
  frame$kmo3<-ma(frame$kmo,order=3,centre=FALSE)
  frame$kmo3[2]<-frame$kmo[2]
  frame$kmo3[1]<-((frame$Deseasonalized[1]+frame$Deseasonalized[2])/2)+(frame$kmo3[2]-frame$kmo3[3])/2  
  frame$kmo3[length(insample)-1]<-frame$kmo[length(insample)-1] 
  frame$kmo3[length(insample)]<-((frame$Deseasonalized[length(insample)]+frame$Deseasonalized[length(insample)-1])/2)+(frame$kmo3[length(insample)-1]-frame$kmo3[length(insample)-2])/2  
  frame$Randomness<-frame$Deseasonalized/frame$kmo3
  frame$kmo3=frame$kmo=frame$ID=LE<-NULL
  
  #Calculate Trend and Cyrcle
  TC<-frame$Deseasonalized/frame$Randomness ; frame$Deseasonalized<-NULL
  xs<-c(1:length(insample))
  frame$Trend<-as.numeric(predict(lm(TC~xs)))  
  frame$Cyrcle<-TC/frame$Trend
  
  frame$SR<-SRTable ; frame$Period<-ID
  
  return(frame)	
}

ThetaG <- function(input, fh, theta, model, plot=FALSE){
  
  #Applies Theta using different curves (model) as TL0 
  
  outtest <- naive(input,h=fh)$mean
  if (theta==0){
    wses <- 0
  }else{
    wses <- 1/theta
  }
  wlrl <- 1-wses
  
  #Estimate and extrapolate TL0 
  observations <- length(input)
  xs <- c(1:observations)
  xf = xff <- c((observations+1):(observations+fh))
  dat <- data.frame(input=input,xs=xs)
  newdf <- data.frame(xs = xff)
  
  if (model=="Exp"){
    estimate <- lm(log(input)~xs)
    thetaline0In <- exp(predict(estimate))+input-input
    thetaline0Out <- exp(predict(estimate,newdf))+outtest-outtest
  }else if (model=="Log"){
    estimate <- lm(input~log(xs))
    thetaline0In <- predict(estimate)+input-input
    thetaline0Out <- predict(estimate,newdf)+outtest-outtest
  }else if (model=="Inv"){
    estimate <- lm(input~ poly((1/xs),1))
    thetaline0In <- predict(estimate)+input-input
    thetaline0Out <- predict(estimate,newdf)+outtest-outtest
  }else if (model=="Pow"){
    estimate <- lm(log(input)~log(xs))
    thetaline0In <- exp(coef(estimate)[1]+log(xs)*coef(estimate)[2])+input-input
    thetaline0Out <- exp(coef(estimate)[1]+log(xff)*coef(estimate)[2])+outtest-outtest
  }else{
    estimate <- lm(input ~ xs)
    thetaline0In <- predict(estimate)+input-input
    thetaline0Out <- predict(estimate,newdf)+outtest-outtest
  }
  
  #Estimate and extrapolate TL2
  thetaline2In <- ses(theta*input+(1-theta)*thetaline0In, h=fh)$fitted
  thetaline2Out <- ses(theta*input+(1-theta)*thetaline0In, h=fh)$mean
  
  #Combine individual forecasts
  forecastsIn <- as.numeric(thetaline2In*wses)+as.numeric(thetaline0In*wlrl)+input-input
  forecastsOut <- as.numeric(thetaline2Out*wses)+as.numeric(thetaline0Out*wlrl)+outtest-outtest
  
  if (plot==TRUE){
    united <- cbind(input,forecastsOut)
    for (ik in 1:(observations+fh)){ united[ik,1] = sum(united[ik,2],united[ik,1], na.rm = TRUE) }
    plot(united[,1],col="black",type="l",main=paste("Model:",model,",Theta:",theta),xlab="Time",ylab="Values",
         ylim=c(min(united[,1])*0.85,max(united[,1])*1.15))
    lines(forecastsIn,col="green") ; lines(forecastsOut,col="green")
    lines(thetaline0In,col="red") ; lines(thetaline0Out,col="red")
    lines(thetaline2In,col="blue") ; lines(thetaline2Out,col="blue")
  }
  
  output <- list(fitted=forecastsIn,mean=forecastsOut,fitted0=thetaline0In,mean0=thetaline0Out,fitted2=thetaline2In,mean2=thetaline2Out,modelest=estimate) 
  
  return(output)
}

NLS_Max = function(input, limit, cut){
  
  #Returs the smoothed time series 
  
  #Apply backcacting
  Observations <- length(input)
  reg_xs <- backcasting <- c(1:Observations)
  begin <- matrix(NA,nrow = 1, ncol = (Observations+2) ) 
  for (i in 1:Observations){ begin[i+1] <- input[i] ; backcasting[i] <- input[Observations-i+1]}
  backcasting <- backcasting+input-input
  begin[1] <- as.numeric(holt(backcasting, damped=TRUE, h=1)$mean)
  begin[Observations+2] <- as.numeric(holt(input,damped=TRUE,h=1)$mean)
  begin <- data.frame(t(begin)) ; colnames(begin) <- "data"
  begin$difs <- c(0)
  begin$dataOr <- begin$data
  
  #Estimate LV
  for (i in 2: (Observations+1) ){ 
    begin$difs[i] <- abs(300*(begin$data[i+1]+begin$data[i-1]-2*begin$data[i])/(begin$data[i+1]+begin$data[i-1]+begin$data[i])) 
  }
  
  #Start smoothing
  if (all(begin$difs[2:(Observations+1)]<limit)){
    sm_insample <- input # If already smooth, end process
  }else{
    # Else continue till smooth enough 
    while ( max(begin$difs[2:(Observations+1)])>=limit ){ 
      beta <- begin$data[which.max(begin$difs)]
      alpha <- (((begin$data[which.max(begin$difs)+1]+begin$data[which.max(begin$difs)-1])/2)-beta)/100
      begin$data[which.max(begin$difs)] <- beta+alpha*cut
      for (i in 2: (Observations+1) ){
        begin$difs[i] <- abs(300*(begin$data[i+1]+begin$data[i-1]-2*begin$data[i])/(begin$data[i+1]+begin$data[i-1]+begin$data[i]))
      }
    }
    sm_insample <- begin$data[-(Observations+2)];sm_insample=sm_insample[-1]+input-input
    
  }
  
  for (i in 1:length(sm_insample)){
    if (sm_insample[i]<=0){
      sm_insample[i]=1
    }
  }
  
  return( sm_insample )
}


# #Load required packages
# library(forecast)
# library(Mcomp)
# #Load M3 monthly time series
# frequency = 12; horizon = 18; data = subset(M3, frequency)
# #Apply the method to the dataset and estimate the average performance
# sMAPE<-c()
# for (tsi in 1:length(data)){
#   insample <- data[[tsi]]$x #training sample
#   outsample <- data[[tsi]]$xx #evaluation sample
#   forecasts <- Forecast_ThetaSm(insample, horizon) #forecast
#   sMAPE<-c(sMAPE,mean(abs(forecasts-outsample)*200/(abs(forecasts)+abs(outsample)))) #estimate sMAPE
# }
# #Average performace
# mean(sMAPE)
