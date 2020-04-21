
# install.packages("devtools")
## devtools::install_github("carlanetto/M4comp2018")

start_time <- Sys.time()


library(parallel)
library(snow)
library(M4comp2018)
library(forecast)
library(forecTheta)
library(dplyr)

data(M4)


##########################################################################################
################    Basic functions    ###################################################
##########################################################################################

ets_forec <- function(x,h,level=0, model = "ZZZ"){
	forecast(ets(x, model=model),h=h, level=level)
}

arima_forec <- function(x,h,level=0, model=NULL){
	if(is.null(model))
		forecast(auto.arima(x),h=h,level=level)
	else
		forecast(Arima(y=x,model=model),h=h,level=level)
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

naive2 <- function(y,h){
	input = y
	fh = h
	ppy <- frequency(input) ; ST <- F
	if (ppy>1){ ST <- SeasonalityTest(input,ppy) }
	if (ST==T){
		Dec <- decompose(input,type="multiplicative")
		des_input <- input/Dec$seasonal
		SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
	}else{
		des_input <- input ; SIout <- rep(1, fh)
	}

	list( mean = naive(des_input, h=fh)$mean*SIout )
}
##########################################################################################
##########################################################################################


##########################################################################################
##############   compute groe for OWA metric   ###########################################
##########################################################################################
groe_owa <- function(y, forecFunction, n1=length(y)-10, m=5, H=length(y)-n1, p=1+floor((length(y)-n1)/m), ...){	
	if(n1>=length(y)){ stop("Error in groe function: n1>=length(y)") }
	if(n1<4){ stop("Error in groe function: n1<4") }
	if(m<1){ stop("Error in groe function: m<1") }
	if(H<1){ stop("Error in groe function: H<1") }
	if(p > 1+floor((length(y)-n1)/m)){ stop("ERROR in groe function: p > 1+floor((length(y)-n1)/m)") }
	if(p <= 0){ stop("ERROR in groe function: p <= 0") }
	#if(!any( g==c("AE","SE","APE","sAPE") )) stop("Error in lossFunction: this g function has not been implemented.")

	n <- length(y)
	fq <- frequency(y)
	time_y <- time(y)
	
	predictionErrors <- function(i){
		ni = n1+(i-1)*m
		n_pred = min(H,n-ni)
		train <- window(y,start=time_y[1],end=time_y[ni])
		test <- window(y,start=time_y[ni+1],end=time_y[ni+n_pred])
		
		mean_diff <- mean(abs(diff(train, lag = fq))) # used to compute MASE metric
		
		if(mean_diff > 0){
			predic_naive2 <- naive2( train, h=n_pred )$mean 	
			errors_sAPE_naive2 <- errorMetric(obs=test, forec=predic_naive2, type="sAPE", statistic="N")
			errors_AE_naive2 <- errorMetric(obs=test, forec=predic_naive2, type="AE", statistic="N")
			errors_ASE_naive2 <- errors_AE_naive2 / mean_diff
			sMAPE_naive2 <- mean(errors_sAPE_naive2)
			MASE_naive2 <- mean(errors_ASE_naive2)
			
			if(sMAPE_naive2 > 0 && MASE_naive2 > 0){
				prediction <- forecFunction( train, h=n_pred)$mean 	
				errors_sAPE <- errorMetric(obs=test, forec=prediction, type="sAPE", statistic="N")
				errors_AE <- errorMetric(obs=test, forec=prediction, type="AE", statistic="N")
				errors_ASE <- errors_AE / mean_diff
				
				errors <- 0.5*errors_sAPE/sMAPE_naive2 + 0.5*errors_ASE/MASE_naive2
			}else{
				errors <- 0
			}
		}else{
			errors <- 0
		}
		
		if( i < p && n1+i*m < n)
		  errors <- c( errors, predictionErrors(i+1) )
			
		return(errors)
	}

	errors <- predictionErrors(i=1)
	
	errors <- replace(errors,errors==Inf,0)
	
	return( sum( errors ) )
}
##########################################################################################
##########################################################################################


##########################################################################################
#######  Run the cross-validation process for all models #################################
##########################################################################################
runModels <- function(M_i){
	h <- M_i$h
	p <- 6
	m <- trunc(h/p)
	n1 <- M_i$n - M_i$h
	if(n1 < 5){n1 <- 5;}
	
	out_dotm <- dotm(M_i$x, M_i$h, level=c(95,95))
	out_otm <- otm(M_i$x, M_i$h, level=c(95,95))
	out_ets <- ets(M_i$x)
	out_arima <- auto.arima(M_i$x)
	arima_order <- arimaorder(out_arima)
	arima_seasonal <- c(0,0,0)
	if(length(arima_order) > 3){arima_seasonal <- arima_order[4:6]; arima_order <- arima_order[1:3]}
	
	criterions <- matrix(NA,4,4,dimnames=list(c("DOTM","OTM", "ETS", "ARIMA"), c("AIC","AICc","BIC","GROE")))
	
	criterions[1,4] <- groe_owa( y=M_i$x, forecFunction=dotm, n1=n1, m=m, H=h, p=p, level=NULL )
	criterions[2,4] <- groe_owa( y=M_i$x, forecFunction=otm, n1=n1, m=m, H=h, p=p, level=NULL )
	criterions[3,4] <- groe_owa( y=M_i$x, forecFunction=ets_forec, n1=n1, m=m, H=h, p=p, model=paste0(out_ets$components[1:3], collapse='') )
	criterions[4,4] <- groe_owa( y=M_i$x, forecFunction=arima_forec, n1=n1, m=m, H=h, p=p, model=out_arima )

	criterions[1,1:3] = summary(out_dotm)$informationCriterions
	criterions[2,1:3] = summary(out_otm)$informationCriterions
	criterions[3,1:3] = unlist(out_ets[c('aic','aicc','bic')])
	criterions[4,1:3] = unlist(out_arima[c('aic','aicc','bic')])
			
	out_ets = forecast(out_ets, M_i$h, level=95)
	out_arima = forecast(out_arima, M_i$h, level=95)
	
	Rank = apply(criterions,FUN=rank,MARGIN=2)
	
	list(DOTM=out_dotm, OTM=out_otm, ETS=out_ets, ARIMA=out_arima, Criterions = criterions, Rank=Rank )
}

tryRunModels <- function(M_i){ 
	return(tryCatch(runModels(M_i), error=function(e) NULL)); 
}
##########################################################################################
##########################################################################################


##########################################################################################
############ run all models for all time series from M4  #################################
##########################################################################################


yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)
quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)
monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
weekly_M4 <- Filter(function(l) l$period == "Weekly", M4)
daily_M4 <- Filter(function(l) l$period == "Daily", M4)
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)

cl<-makeCluster(detectCores()-1)

clusterEvalQ(cl, library(M4comp2018))
clusterEvalQ(cl, library(forecast))
clusterEvalQ(cl, library(forecTheta))
clusterEvalQ(cl, library(Mcomp))

clusterExport(cl, "runModels")
clusterExport(cl, "arima_forec")
clusterExport(cl, "groe_owa")
clusterExport(cl, "SeasonalityTest")
clusterExport(cl, "naive2")
clusterExport(cl, "ets_forec")

clusterExport(cl, "yearly_M4")
clusterExport(cl, "quarterly_M4")
clusterExport(cl, "monthly_M4")
clusterExport(cl, "weekly_M4")
clusterExport(cl, "daily_M4")
clusterExport(cl, "hourly_M4")


start_time_yearly <- Sys.time()
yearly_M4.out = parLapply( cl = cl, x=yearly_M4, fun=tryRunModels )
end_time_yearly <- Sys.time()
save.image("M4-yearly.Rdata")
rm(yearly_M4.out)

start_time_quarterly <- Sys.time()
quarterly_M4.out = parLapply( cl = cl, x=quarterly_M4, fun=tryRunModels )
end_time_quarterly <- Sys.time()
save.image("M4-quarterly.Rdata")
rm(quarterly_M4.out)

start_time_monthly <- Sys.time()
monthly_M4.out = parLapply( cl = cl, x=monthly_M4, fun=tryRunModels )
end_time_monthly <- Sys.time()
save.image("M4-monthly.Rdata")
rm(monthly_M4.out)


start_time_weekly <- Sys.time()
weekly_M4.out = parLapply( cl = cl, x=weekly_M4, fun=tryRunModels )
end_time_weekly <- Sys.time()
save.image("M4-weekly.Rdata")
rm(weekly_M4.out)

start_time_daily <- Sys.time()
daily_M4.out = parLapply( cl = cl, x=daily_M4, fun=tryRunModels )
end_time_daily <- Sys.time()
save.image("M4-daily.Rdata")
rm(daily_M4.out)

start_time_hourly <- Sys.time()
hourly_M4.out = parLapply( cl = cl, x=hourly_M4, fun=tryRunModels )
end_time_hourly <- Sys.time()
save.image("M4-hourly.Rdata")
rm(hourly_M4.out)

stopCluster(cl)			

##########################################################################################
##########################################################################################


##########################################################################################
#############    models combination and CSV files   ######################################
##########################################################################################
modelsCombination <- function(series, outModels){
	
	nn <- length(series)
	if(nn != length(outModels)){
		stop("nn != length(outModels)")
		return()
	}
	
	nomeLinhas <- rep(NA, nn)
	forec <- matrix(NA, nrow=nn, ncol=48)
	lower95 <- matrix(NA, nrow=nn, ncol=48)
	upper95 <- matrix(NA, nrow=nn, ncol=48)
	
	weight <- matrix(NA, nrow=nn, ncol=4)
	colnames(weight) <- c('DOTM','OTM','ETS','ARIMA')
	
	for(i in 1:nn){
		models <- outModels[[i]]
		nomeLinhas[i] <- series[[i]]$st
		h <- series[[i]]$h
		GROE <- models$Criterions[,'GROE']
		if( !is.null(models) && all(!is.na(GROE)) && all(GROE>0) ){
			if(!all( near(outModels[[i]]$DOTM$y ,  series[[i]]$x, 0.01) )){
				stop(paste0("\nERRO na série i = ",i,"\n"))
			}
		
			Score <- 1/GROE
			pesos <- Score / sum(Score)
			forec[i,1:h] =   pesos["DOTM"]*models[["DOTM"]]$mean +      pesos["OTM"]*models[["OTM"]]$mean +      pesos["ETS"]*models[["ETS"]]$mean +      pesos["ARIMA"]*models[["ARIMA"]]$mean
			lower95[i,1:h] = pesos["DOTM"]*models[["DOTM"]]$lower[,1] + pesos["OTM"]*models[["OTM"]]$lower[,1] + pesos["ETS"]*models[["ETS"]]$lower[,1] + pesos["ARIMA"]*models[["ARIMA"]]$lower[,1]
			upper95[i,1:h] = pesos["DOTM"]*models[["DOTM"]]$upper[,1] + pesos["OTM"]*models[["OTM"]]$upper[,1] + pesos["ETS"]*models[["ETS"]]$upper[,1] + pesos["ARIMA"]*models[["ARIMA"]]$upper[,1]			
			weight[i,] = pesos
		}else{
			cat("\nSerie = ",series[[i]]$st," , ", "i = ",i,"\n")
			
			out_naive <- snaive(y=series[[i]]$x, h=series[[i]]$h, level=95)
			
			forec[i,1:h] = out_naive$mean
			lower95[i,1:h] = out_naive$lower[,1]
			upper95[i,1:h] = out_naive$upper[,1]
		}
	}
			
	forec[forec < 0] <- 0
	lower95[lower95 < 0] <- 0
	upper95[upper95 < 0] <- 0
	
	colnames(forec) <- colnames(lower95) <- colnames(upper95) <- c(paste0("F",1:48))
	rownames(forec) <- rownames(lower95) <- rownames(upper95) <- rownames(weight) <- nomeLinhas

	write.table(round(forec,3), file = "forec_4groec.csv", append = TRUE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = FALSE)
			
	write.table(round(lower95,3), file = "lower95_4groec.csv", append = TRUE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = FALSE)
			
	write.table(round(upper95,3), file = "upper95_4groec.csv", append = TRUE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = FALSE)
			
	write.table(round(weight,3), file = "weight_4groec.csv", append = TRUE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = FALSE)		
}


load("M4-yearly.Rdata")
modelsCombination(series=yearly_M4, outModels=yearly_M4.out)
rm(yearly_M4.out)

load("M4-quarterly.Rdata")
modelsCombination(series=quarterly_M4, outModels=quarterly_M4.out)
rm(quarterly_M4.out)

load("M4-monthly.Rdata")
modelsCombination(series=monthly_M4, outModels=monthly_M4.out)
rm(monthly_M4.out)

load("M4-weekly.Rdata")
modelsCombination(series=weekly_M4, outModels=weekly_M4.out)
rm(weekly_M4.out)

load("M4-daily.Rdata")
modelsCombination(series=daily_M4, outModels=daily_M4.out)
rm(daily_M4.out)

load("M4-hourly.Rdata")
modelsCombination(series=hourly_M4, outModels=hourly_M4.out)
rm(hourly_M4.out)
##########################################################################################
##########################################################################################

end_time <- Sys.time()




total_time <- (end_time_yearly - start_time_yearly) +
(end_time_quarterly - start_time_quarterly) +
(end_time_monthly - start_time_monthly) +
(end_time_weekly - start_time_weekly) +
(end_time_daily - start_time_daily) +
(end_time_hourly - start_time_hourly)

### Computer:
# DELL OPTIPLEX 7050, Windows 10 64bits, equipped with processor i7-7700 and 16GB of RAM Memory, R version 3.4.3

total_time/3600 ## hours
# Time difference of 61.63763

total_time/(3600*24) ## days
#Time difference of 2.568235

total_time / 100000 ## mean time per series
#Time difference of 2.218955 secs

