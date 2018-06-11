devtools::install_github("carlanetto/M4comp2018")
devtools::install_github("robjhyndman/forecast")
devtools::install_github("alsabtay/ATAforecasting")

library(forecast)
library(ATAforecasting)
library(M4comp2018)
Result_forecast <- matrix(NA,nrow=100000, ncol=49)
Result_lower <- matrix(NA,nrow=100000, ncol=49)
Result_upper <- matrix(NA,nrow=100000, ncol=49)
for (m in 1:100000){
  if (M4[[m]]$period=="Yearly") {
    model1<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.05, end.phi = 1, size.phi = 0.05, 
                model.type = "A", seasonal.test = FALSE,
                negative.forecast = FALSE, seasonal.model = "none", plot.out = TRUE)
    
    for (i in 1:M4[[m]]$h){
      Result_forecast[m,1]<-M4[[m]]$st
      Result_forecast[m,i+1]<-(model1$forecast[i])
      Result_lower[m,1]<-M4[[m]]$st
      Result_lower[m,i+1]<-(model1$forecast.lower[i])
      Result_upper[m,1]<-M4[[m]]$st
      Result_upper[m,i+1]<-(model1$forecast.upper[i])
    }
  }else if (M4[[m]]$period=="Quarterly"){
    model1<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "A", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
    model2<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "M", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
    model3<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "A", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)
    model4<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "M",  seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)		
    for (i in 1:M4[[m]]$h){
      Result_forecast[m,1]<-M4[[m]]$st
      Result_forecast[m,i+1]<-(model1$forecast[i]+model2$forecast[i]+model3$forecast[i]+model4$forecast[i])/4
      Result_lower[m,1]<-M4[[m]]$st
      Result_lower[m,i+1]<-(model1$forecast.lower[i]+model2$forecast.lower[i]+model3$forecast.lower[i]+model4$forecast.lower[i])/4
      Result_upper[m,1]<-M4[[m]]$st
      Result_upper[m,i+1]<-(model1$forecast.upper[i]+model2$forecast.upper[i]+model3$forecast.upper[i]+model4$forecast.upper[i])/4
    }
  }else if (M4[[m]]$period=="Monthly"){
    model1<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "A", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
    model2<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "M", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
    model3<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "A", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)
    model4<-ATA(M4[[m]]$x, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "M",  seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)	
    for (i in 1:M4[[m]]$h){
      Result_forecast[m,1]<-M4[[m]]$st
      Result_forecast[m,i+1]<-(model1$forecast[i]+model2$forecast[i]+model3$forecast[i]+model4$forecast[i])/4
      Result_lower[m,1]<-M4[[m]]$st
      Result_lower[m,i+1]<-(model1$forecast.lower[i]+model2$forecast.lower[i]+model3$forecast.lower[i]+model4$forecast.lower[i])/4
      Result_upper[m,1]<-M4[[m]]$st
      Result_upper[m,i+1]<-(model1$forecast.upper[i]+model2$forecast.upper[i]+model3$forecast.upper[i]+model4$forecast.upper[i])/4
    }
  }else if (M4[[m]]$period=="Weekly"){
    if (length(M4[[m]]$x)>=104){
      weeklydata = ts(M4[[m]]$x, frequency=52)
      model1<-ATA(weeklydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                  seasonal.type = "M", model.type = "A", seasonal.test = "FALSE",
                  negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
      model2<-ATA(weeklydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                  seasonal.type = "M", model.type = "M", seasonal.test = "FALSE",  
                  negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
      model3<-ATA(weeklydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                  seasonal.type = "M", model.type = "A", seasonal.test = "FALSE",  
                  negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)
      model4<-ATA(weeklydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                  seasonal.type = "M", model.type = "M",  seasonal.test = "FALSE",  
                  negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)	
      for (i in 1:M4[[m]]$h){
        Result_forecast[m,1]<-M4[[m]]$st
        Result_forecast[m,i+1]<-(model1$forecast[i]+model2$forecast[i]+model3$forecast[i]+model4$forecast[i])/4
        Result_lower[m,1]<-M4[[m]]$st
        Result_lower[m,i+1]<-(model1$forecast.lower[i]+model2$forecast.lower[i]+model3$forecast.lower[i]+model4$forecast.lower[i])/4
        Result_upper[m,1]<-M4[[m]]$st
        Result_upper[m,i+1]<-(model1$forecast.upper[i]+model2$forecast.upper[i]+model3$forecast.upper[i]+model4$forecast.upper[i])/4
      }
      
    }
    else  {
    weeklydata = ts(M4[[m]]$x, frequency=52)
    model1<-ATA(weeklydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "A", seasonal.test = "FALSE",
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "none", plot.out = FALSE)
    model2<-ATA(weeklydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "M", seasonal.test = "FALSE",  
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "none", plot.out = FALSE)
    model3<-ATA(weeklydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "A", seasonal.test = "FALSE",  
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "none", plot.out = FALSE)
    model4<-ATA(weeklydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "M",  seasonal.test = "FALSE",  
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "none", plot.out = FALSE)	
      for (i in 1:M4[[m]]$h){
        Result_forecast[m,1]<-M4[[m]]$st
        Result_forecast[m,i+1]<-(model1$forecast[i]+model2$forecast[i]+model3$forecast[i]+model4$forecast[i])/4
        Result_lower[m,1]<-M4[[m]]$st
        Result_lower[m,i+1]<-(model1$forecast.lower[i]+model2$forecast.lower[i]+model3$forecast.lower[i]+model4$forecast.lower[i])/4
        Result_upper[m,1]<-M4[[m]]$st
        Result_upper[m,i+1]<-(model1$forecast.upper[i]+model2$forecast.upper[i]+model3$forecast.upper[i]+model4$forecast.upper[i])/4
      }}
  }else if (M4[[m]]$period=="Daily"){
    dailydata = ts(M4[[m]]$x, frequency=7)
    model1<-ATA(dailydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "A", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
    model2<-ATA(dailydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "M", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
    model3<-ATA(dailydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "A", seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)
    model4<-ATA(dailydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "M",  seasonal.test = "TRUE", seasonal.test.attr = ata.seasonal.attr(s.tcrit = 1.28), 
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)	
for (i in 1:M4[[m]]$h){
  Result_forecast[m,1]<-M4[[m]]$st
  Result_forecast[m,i+1]<-(model1$forecast[i]+model2$forecast[i]+model3$forecast[i]+model4$forecast[i])/4
  Result_lower[m,1]<-M4[[m]]$st
  Result_lower[m,i+1]<-(model1$forecast.lower[i]+model2$forecast.lower[i]+model3$forecast.lower[i]+model4$forecast.lower[i])/4
  Result_upper[m,1]<-M4[[m]]$st
  Result_upper[m,i+1]<-(model1$forecast.upper[i]+model2$forecast.upper[i]+model3$forecast.upper[i]+model4$forecast.upper[i])/4
}
  }else if (M4[[m]]$period=="Hourly"){
    hourlydata = ts(M4[[m]]$x, frequency=168)
    model1<-ATA(hourlydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "A", seasonal.test = "FALSE", 
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
    model2<-ATA(hourlydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05, 
                seasonal.type = "M", model.type = "M", seasonal.test = "FALSE", 
                negative.forecast = FALSE,level.fixed = FALSE, seasonal.model = "decomp", plot.out = FALSE)
    model3<-ATA(hourlydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "A", seasonal.test = "FALSE", 
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)
    model4<-ATA(hourlydata, h = M4[[m]]$h, parQ = 1, start.phi = 0.85, end.phi = 1, size.phi = 0.05,
                seasonal.type = "M", model.type = "M",  seasonal.test = "FALSE", 
                negative.forecast = FALSE,level.fixed = TRUE, seasonal.model = "decomp", plot.out = FALSE)
for (i in 1:M4[[m]]$h){
  Result_forecast[m,1]<-M4[[m]]$st
  Result_forecast[m,i+1]<-(model1$forecast[i]+model2$forecast[i]+model3$forecast[i]+model4$forecast[i])/4
  Result_lower[m,1]<-M4[[m]]$st
  Result_lower[m,i+1]<-(model1$forecast.lower[i]+model2$forecast.lower[i]+model3$forecast.lower[i]+model4$forecast.lower[i])/4
  Result_upper[m,1]<-M4[[m]]$st
  Result_upper[m,i+1]<-(model1$forecast.upper[i]+model2$forecast.upper[i]+model3$forecast.upper[i]+model4$forecast.upper[i])/4
}
  } else{
    
  }
}
proc.time()
write.csv(Result_forecast, "D:/ATA_Damped_forecast.csv")
write.csv(Result_lower, "D:/ATA_Damped_lower.csv")
write.csv(Result_upper, "D:/ATA_Damped_upper.csv")
