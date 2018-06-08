devtools::install_github("carlanetto/M4comp2018")
devtools::install_github("robjhyndman/forecast")
devtools::install_github("alsabtay/ATAforecasting")

library(forecast)
library(M4comp2018)
library(ATAforecasting)
library(foreach)
library(doSNOW)

cl = makeCluster(4, type = "SOCK")  # makeCluster(detectCores()-1, type = "SOCK") 
registerDoSNOW(cl)
	
ATA_M4_Full <- function(m){
	ATA_forecast <- rep(NA,49)
	ATA_lower <- rep(NA,49)
	ATA_upper <- rep(NA,49)	
	M4name <- M4[[m]]$st
	M4period <- as.character(M4[[m]]$period)
	if (M4period=="Yearly") {
		ATA_M4_Data <- ts(M4[[m]]$x, frequency=1) 
		ATA_M4_h <- M4[[m]]$h
    model1 <- ATA(X = ATA_M4_Data, parQ = 1, h = ATA_M4_h, start.phi = 0.6, end.phi = 1, size.phi = 0.01, model.type = "A", seasonal.model = "none", seasonal.test=FALSE, plot.out = FALSE, negative.forecast = FALSE)
		ATA_forecast[1] <- M4name
		ATA_forecast[2:7] <- model1$forecast
		ATA_lower[1] <- M4name
		ATA_lower[2:7] <- model1$forecast.lower
		ATA_upper[1] <- M4name
		ATA_upper[2:7] <-model1$forecast.upper
	}else if (M4period=="Quarterly"){
		ATA_M4_Data <- ts(M4[[m]]$x, frequency=4) 
		ATA_M4_h <- M4[[m]]$h
		model1<-ATA(X = ATA_M4_Data, parQ = 0, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), negative.forecast = FALSE, plot.out = FALSE)
		model2<-ATA(X = ATA_M4_Data, parQ = 1, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), negative.forecast = FALSE, plot.out = FALSE)	
		ATA_forecast[1] <- M4name
		ATA_forecast[2:9] <-(model1$forecast+model2$forecast)/2
		ATA_lower[1] <- M4name
		ATA_lower[2:9] <- (model1$forecast.lower+model2$forecast.lower)/2
		ATA_upper[1] <- M4name
		ATA_upper[2:9] <- (model1$forecast.upper+model2$forecast.upper)/2
	}else if (M4period=="Monthly"){
		ATA_M4_Data <- ts(M4[[m]]$x, frequency=12) 
		ATA_M4_h <- M4[[m]]$h		
		model1<-ATA(X = ATA_M4_Data, parQ = 0, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), negative.forecast = FALSE, plot.out = FALSE)
		model2<-ATA(X = ATA_M4_Data, parQ = 1, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), negative.forecast = FALSE, plot.out = FALSE)	
		ATA_forecast[1] <- M4name
		ATA_forecast[2:19] <- (model1$forecast+model2$forecast)/2
		ATA_lower[1] <- M4name
		ATA_lower[2:19] <- (model1$forecast.lower+model2$forecast.lower)/2
		ATA_upper[1] <- M4name
		ATA_upper[2:19] <- (model1$forecast.upper+model2$forecast.upper)/2
	}else if (M4period=="Weekly"){
		ATA_M4_Data <- ts(M4[[m]]$x, frequency=52) 
		ATA_M4_h <- M4[[m]]$h
		if (length(ATA_M4_Data)>=104){
			model1<-ATA(X = ATA_M4_Data, parQ = 0, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "FALSE", negative.forecast = FALSE, plot.out = FALSE)
			model2<-ATA(X = ATA_M4_Data, parQ = 1, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "FALSE", negative.forecast = FALSE, plot.out = FALSE)	
		}else {
			model1<-ATA(X = ATA_M4_Data, parQ = 0, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, model.type = "A", seasonal.model = "none", seasonal.test = "FALSE", negative.forecast = FALSE, plot.out = FALSE)
			model2<-ATA(X = ATA_M4_Data, parQ = 1, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, model.type = "A", seasonal.model = "none", seasonal.test = "FALSE", negative.forecast = FALSE, plot.out = FALSE)	
		}		
		ATA_forecast[1] <- M4name
		ATA_forecast[2:14] <- (model1$forecast+model2$forecast)/2
		ATA_lower[1] <- M4name
		ATA_lower[2:14] <- (model1$forecast.lower+model2$forecast.lower)/2
		ATA_upper[1] <- M4name
		ATA_upper[2:14] <- (model1$forecast.upper+model2$forecast.upper)/2
	}else if (M4period=="Daily"){
		ATA_M4_Data <- ts(M4[[m]]$x, frequency=7) 
		ATA_M4_h <- M4[[m]]$h
		model1<-ATA(X = ATA_M4_Data, parQ = 0, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), negative.forecast = FALSE, plot.out = FALSE)
		model2<-ATA(X = ATA_M4_Data, parQ = 1, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), negative.forecast = FALSE, plot.out = FALSE)	
		ATA_forecast[1] <- M4name
		ATA_forecast[2:15] <- (model1$forecast+model2$forecast)/2
		ATA_lower[1] <- M4name
		ATA_lower[2:15] <- (model1$forecast.lower+model2$forecast.lower)/2
		ATA_upper[1] <- M4name
		ATA_upper[2:15] <- (model1$forecast.upper+model2$forecast.upper)/2
	}else if (M4period=="Hourly"){
		ATA_M4_Data <- ts(M4[[m]]$x, frequency=168) 
		ATA_M4_h <- M4[[m]]$h
		model1<-ATA(X = ATA_M4_Data, parQ = 0, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "FALSE", negative.forecast = FALSE, plot.out = FALSE)
		model2<-ATA(X = ATA_M4_Data, parQ = 1, h = ATA_M4_h, start.phi = 1, end.phi = 1, size.phi = 1, seasonal.type = "M", model.type = "A", seasonal.model = "decomp", seasonal.test = "FALSE", negative.forecast = FALSE, plot.out = FALSE)	
		ATA_forecast[1] <- M4name
		ATA_forecast[2:49] <- forecast_m4
		ATA_lower[1] <- M4name
		ATA_lower[2:49] <- (model1$forecast.lower+model2$forecast.lower)/2
		ATA_upper[1] <- M4name
		ATA_upper[2:49] <- (model1$forecast.upper+model2$forecast.upper)/2
	}else {
	}
	ATA_M4 <- t(cbind(ATA_forecast,ATA_lower,ATA_upper))
	gc()
}
	ptm <- proc.time() 
	ATA_M4 <- foreach (m=1:10, .combine='rbind', .packages=c('M4comp2018', 'ATAforecasting')) %dopar% ATA_M4_Full(m)
	stopCluster(cl)
	proc.time() - ptm 
	write.csv(ATA_M4[rownames(ATA_M4)=="ATA_forecast",], "C:/ATA_M4_forecast.csv")
	write.csv(ATA_M4[rownames(ATA_M4)=="ATA_lower",], "C:/ATA_M4_lower.csv")
	write.csv(ATA_M4[rownames(ATA_M4)=="ATA_upper",], "C:/ATA_M4_upper.csv")
	