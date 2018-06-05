################################################################################
# Calculates forecasts for small subset of M4 data set.
################################################################################

# To recalculate out forecasts for a subset of M4 data set:
#   1. Source this file to populate the environment
#   2. Use forecast.M4.demo on a subset of M4 labels. See the end of this file 
#      for example usage.

source("../M4/M4tools.R")
source("../M4/final/M4final.yearly.R")
source("../M4/final/M4final.quarterly.R")
source("../M4/final/M4final.monthly.R")
source("../M4/final/M4final.weekly.R")
source("../M4/final/M4final.daily.R")
source("../M4/final/M4final.hourly.R")


# Calculates forecasts for a given subset of M4 series labels.
forecast.M4.demo = function(labels) {
  M4subset = Filter(function(x) x$st %in% labels, M4)
  
  M4.yearly.sample = Filter(function(s) s$period == "Yearly", M4subset)
  M4.quarterly.sample = Filter(function(s) s$period == "Quarterly", M4subset)
  M4.monthly.sample = Filter(function(s) s$period == "Monthly", M4subset)
  M4.weekly.sample = Filter(function(s) s$period == "Weekly", M4subset)
  M4.daily.sample = Filter(function(s) s$period == "Daily", M4subset)
  M4.hourly.sample = Filter(function(s) s$period == "Hourly", M4subset)
  
  M4.yearly.fc = final.forecast.yearly(M4.yearly.sample)
  M4.quarterly.fc = final.forecast.quarterly(M4.quarterly.sample)
  M4.monthly.fc = final.forecast.monthly(M4.monthly.sample)
  M4.weekly.fc = final.forecast.weekly(M4.weekly.sample)
  M4.daily.fc = final.forecast.daily(M4.daily.sample)
  M4.hourly.fc = final.forecast.hourly(M4.hourly.sample)
  
  seriess = c(M4.yearly.sample, M4.quarterly.sample, M4.monthly.sample, 
              M4.weekly.sample, M4.daily.sample, M4.hourly.sample)
  preds = c(M4.yearly.fc, M4.quarterly.fc, M4.monthly.fc, 
            M4.weekly.fc, M4.daily.fc, M4.hourly.fc)
  result = list()
  for (i in 1:length(labels)) {
    result[[seriess[[i]]$st]] = preds[[i]]
  }
  return(result)
}

stop("This stop prevents recalculating forecasts by accident.")

# Example usage
indices = sample(1:length(M4), 10, replace = F)
labels = sapply(indices, function(i) M4[[i]]$st)
labels

forecasts = forecast.M4.demo(labels)
forecasts
