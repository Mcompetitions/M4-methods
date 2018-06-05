################################################################################
# Criteria used in M4 competition and in our experiments.
################################################################################

# Everywhere but in crit.mase, insample argument serves only compatibility 
# reasons and has no effect.

crit.smape = function(insample = NULL, outsample, forecasts) {
  outsample = as.numeric(outsample)
  forecasts = as.numeric(forecasts)
  smape = (abs(outsample - forecasts) * 2) / (abs(outsample) + abs(forecasts))
  return(smape)
}

# can be undefined if insample is too short
crit.mase = function(insample, outsample, forecasts) {
  frq = frequency(insample)
  if (length(insample) <= frq) {
    return(rep(NA, length(outsample)))
  }
  forecastsNaiveSD = as.numeric(head(insample, length(insample) - frq))
  masep = mean(abs(tail(insample, length(insample) - frq) - forecastsNaiveSD))
  outsample = as.numeric(outsample)
  forecasts = as.numeric(forecasts)
  mase = abs(outsample - forecasts)
  res = mase / masep
  res[which(mase == 0)] = 0
  if (masep == 0) { # we have to do something in this irrelevant case
    res = rep(0, length(res))
  }
  return(res)
}

crit.mse = function(insample = NULL, outsample, forecasts) {
  outsample = as.numeric(outsample)
  forecasts = as.numeric(forecasts)
  return((outsample - forecasts)^2)
}