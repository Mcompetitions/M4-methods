################################################################################
# Calculates and saves forecasts for all M4 series. THIS TAKES A VERY LONG TIME!
################################################################################

# A very long time means probably several days. You can greatly speed up the
# process by utilizing cl argument in test.models and precals.preds functions.
# Note that using multicore computations takes a lot more memory.
# It's best to split this into sereval parts (e.g. do different periods 
# separately).

source("../M4/M4tools.R")
source("../M4/final/M4final.yearly.R")
source("../M4/final/M4final.quarterly.R")
source("../M4/final/M4final.monthly.R")
source("../M4/final/M4final.weekly.R")
source("../M4/final/M4final.daily.R")
source("../M4/final/M4final.hourly.R")

prepare.M4.fcst.df = function(data, fcsts) {
  m = matrix(unlist(fcsts), nrow = length(data), byrow = T)
  m = cbind(m, matrix(nrow = length(data), ncol = 48 - data[[1]]$h))
  d = data.frame(m, row.names = sapply(data, function(x) x$st))
  return(d)
}

# read data
M4.yearly = Filter(function(s) s$period == "Yearly", M4)
M4.quarterly = Filter(function(s) s$period == "Quarterly", M4)
M4.monthly = Filter(function(s) s$period == "Monthly", M4)
M4.weekly = Filter(function(s) s$period == "Weekly", M4)
M4.daily = Filter(function(s) s$period == "Daily", M4)
M4.hourly = Filter(function(s) s$period == "Hourly", M4)

# calculate forecasts
M4.yearly.fc = final.forecast.yearly(M4.yearly)
M4.quarterly.fc = final.forecast.quarterly(M4.quarterly)
M4.monthly.fc = final.forecast.monthly(M4.monthly)
M4.weekly.fc = final.forecast.weekly(M4.weekly)
M4.daily.fc = final.forecast.daily(M4.daily)
M4.hourly.fc = final.forecast.hourly(M4.hourly)

# save to .csv
M4.yearly.d = prepare.M4.fcst.df(M4.yearly, M4.yearly.fc)
M4.quarterly.d = prepare.M4.fcst.df(M4.quarterly, M4.quarterly.fc)
M4.monthly.d = prepare.M4.fcst.df(M4.monthly, M4.monthly.fc)
M4.weekly.d = prepare.M4.fcst.df(M4.weekly, M4.weekly.fc)
M4.daily.d = prepare.M4.fcst.df(M4.daily, M4.daily.fc)
M4.hourly.d = prepare.M4.fcst.df(M4.hourly, M4.hourly.fc)

col.names = c("id", sapply(1:48, function(i) paste("F", i, sep = "")))
path = file.path(M4.final.path.future, "M4.forecasts.prologistica.csv")
write.table(matrix(ncol = 49, nrow = 0), 
            file = path, col.names = col.names, quote = F, sep = ",")

for (t in list(M4.yearly.d, M4.quarterly.d, M4.monthly.d, 
               M4.weekly.d, M4.daily.d, M4.hourly.d)) {
  write.table(t, file = path, quote = F, col.names = F, sep = ",", dec = ".",
              append = T)
}

