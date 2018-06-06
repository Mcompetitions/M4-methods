################################################################################
# Calculate forecasts for Daily data
################################################################################

source("../M4/M4tools.R")

cat("Preparing window matrices for Daily data...")
M4.daily = Filter(function(s) s$period == "Daily", M4)
window.daily = 2 * M4.daily[[1]]$h
wms.daily = prepare.window.matrices(window.daily, M4.daily)
cat(" Done!", fill = T)

final.forecast.daily = function(data) {
  if (length(data) == 0) {
    return(list())
  }
  
  classes = classify.all(data)
  preds = precalc.preds(data, classes, model.choice = ModelChoice.M4)
  
  test = test.models(
    seriess = data,
    seriess.info = classes,
    num_samples = 8, # N parameter (number of prefixes to consider)
    aggregation = 0.5, # f function (exponential weights with base 0.5)
    model.choice = ModelChoice.M4
  )
  
  weights = combine.models(test, weightsfn = weighting.squared) # g function (square weights)
  future.preds = predict.models(data, weights, preds)
  
  # Set naive forecast for "stable" series
  stable = which(pbsapply(data, check.naive.tail) < 0.05)
  
  for (i in stable) {
    future.preds[[i]] = preds[[i]]["naive",]
  }
  
  # Set correlation-based forecast where a good match is found
  cor.future.predict.res = pblapply(
    data, function(x) cor.predict(x, M4.daily, wms.daily, threshold = 0.99))
  
  cor.future.preds = lapply(cor.future.predict.res, function(x) x$fcst)
  correlated.future = sapply(cor.future.predict.res, function(x) x$correlated)
  
  for (i in which(correlated.future == 1)) {
    future.preds[[i]] = cor.future.preds[[i]]
  }
  
  # Set negative values to 0
  future.preds = lapply(future.preds, negative.to.zero)
  
  return(future.preds)
}
