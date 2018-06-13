######################################################################################
# Forecast functions
######################################################################################

#-------------------------------------------------------------------------------------
# Statistical Functions
#-------------------------------------------------------------------------------------

LjungBoxP <- function(residuen, lag) {
  p <- rep(0,lag)
  for(i in 1:length(p)) {
    p[i] <- Box.test(residuen,i,type="Ljung-Box")$p.value
  }
  return(p)
}

smape_cal <- function(outsample, forecasts){

  #Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts)*2)/(abs(outsample)+abs(forecasts))
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


#-------------------------------------------------------------------------------------
# ARIMA-Functions
#-------------------------------------------------------------------------------------

arimasim <- function(x,yld) {
  tryCatch({
    return(arima(yld,order=c(x[1],0,x[2])))
  },warning = function(w) {
    # do nothing
  }, error = function(e) {
    # do nothing
  })
  
}

arimastat <- function(y.arima,fcst) {
  v <- rep(NA,4)
  if(!is.null(y.arima)) {
    lb <- LjungBoxP(y.arima$residuals,fcst)
    v[1] <- mean(lb>=0.05)
    v[2] <- mean(lb)
    
    ac <- acf(y.arima$residuals, plot = F, main = "ACF of Residuals", na.action = na.pass)$acf
    v[3] <- mean(abs(ac)[-1]<=0.05)
    v[4] <- mean(0-abs(ac)[-1])
  }
  
  return(v)
  
}

arimasimCV <- function(arma, yld, lower,fcst) {
  
  res <- c(NA,NA,NA)
  
  v <- sapply(lower,cv,yld=yld,arma=arma,fcst=fcst)

  crit <- apply(apply(v,1,is.na),2,sum)/ncol(v)<=0.5 #0.632
  
  res[crit] <- apply(v[crit,],1,mean,na.rm=T)
  
  return(res)
  
}

cv <- function(lo, yld,arma,fcst) {
  
  res <- c(NA,NA,NA)
  tryCatch({
    
    yldTrain <- yld[1:(lo-1)]
    yldTest <- yld[lo:(lo+(fcst-1))]
    #cat(lo[j]-1," ",lo[j]," ",lo[j]+12,"\n")
    
    y.arima<-arima(yldTrain,order=c(arma[1],0,arma[2]))
    y.for<-predict(y.arima,n.ahead=fcst)
    
    res[1] <- mean((yldTest-y.for$pred)^2)
    res[2] <- mean(smape_cal(yldTest, y.for$pred))
    res[3] <- mean(mase_cal(yldTrain, yldTest, y.for$pred))
    
  }, warning = function(w) {
    
    #print("warning")
    
  }, error = function(e) {
    
    #print("error")
    
  })
  
  return(res)
}


#-------------------------------------------------------------------------------------
# Main Function
#-------------------------------------------------------------------------------------

pred <- function(y,fcst,typ) {
  
  id <- y[1] # save the index-number
  y <- unname(y[-1])
  y <- y[!is.na(y)]
  
  y <- tail(y,2000)
  
  zz <- file(paste("output/log/",typ,id,".log",sep=""), "w") #log-file
  cat("Start time;",as.character(Sys.time()),"\n",file=zz)

  ly <- log(y)
  yld <- diff(ly) # calc log returns
  
  armaTable <- expand.grid(1:10,1:10) # arima models combinations

  s <- apply(armaTable,1,arimasim,yld=yld)
  
  armaTable <- cbind(armaTable,matrix(unlist(lapply(s,arimastat,fcst=fcst)),ncol=4,byrow = T),rep(0,100),rep(0,100),rep(0,100))

  colnames(armaTable) <- c("AR","MA","LB_out","LB_height","ACF_in","ACF_mean","cv_mse","cv_smape","cv_mase")
  
  i <- c(1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34)
  aTable <- armaTable[i,] # my teacher said, ARMA(4,4) or below are good enough
  aTable <- aTable[!is.na(apply(aTable[,3:6],1,sum)),]
  aTable <- aTable[aTable[,3]>0.5 & aTable[,5]>0.5,]
  
  armaTable <- armaTable[-i,]
  aTable <- rbind(armaTable[order(apply(armaTable[,3:6],1,sum),decreasing = T),][1:3,],aTable)
  
  upper <- length(yld)-(fcst-1)
  lower <- seq(upper-fcst*9,upper,by=fcst)
  if(lower[1]<length(yld)/2) {
    lower <- seq(upper-fcst,upper,by=1)
    cat("To short for big step CV;true","\n",file=zz)
  } else {
    cat("To short for big step CV;false","\n",file=zz)
  }
  
  aTable[,7:9] <- t(apply(aTable[,1:2],1,arimasimCV,yld=yld,lower=lower,fcst=fcst))
  
  if(sum(aTable[,8]& aTable[,9],na.rm = T)>=2) { # check if we have numeric cv values
    cat("CV possible;true","\n",file=zz)
    o <- apply(scale(aTable[,8:9],center=F),1,mean) # generate official measurement
    aTable <- aTable[order(o),]
  } else {
    cat("CV possible;false","\n",file=zz)
  }
    
  p<-predict(s[[as.numeric(rownames(aTable)[1])]],n.ahead=fcst)$pred
  
  write.csv2(round(aTable,7),paste("output/aTable/",typ,id,".csv",sep=""),row.names = F)

  yp <- exp(diffinv(c(yld,p),xi=ly[1]))
  export <- t(tail(yp,fcst))
  rownames(export) <- paste(typ,id,sep="")
  
  write.csv(export,paste("output/fcst/",typ,id,".csv",sep=""))
  cat("End time;",as.character(Sys.time()),"\n",file=zz)
  close(zz)
  
  return(c(aTable[1,1],aTable[1,2]))
}