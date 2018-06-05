################################################################################
# Calculate forecasts for Hourly data
################################################################################

source("../M4/M4tools.R")

cat("Preparing window matrices for Hourly data...")
M4.hourly = Filter(function(s) s$period == "Hourly", M4)
window.hourly = 2 * M4.hourly[[1]]$h
wms.hourly = prepare.window.matrices(window.hourly, M4.hourly)
cat(" Done!", fill = T)

final.forecast.hourly = function(data) {
  if (length(data) == 0) {
    return(list())
  }
  
  classes = classify.all(data)
  preds = precalc.preds(data, classes, model.choice = ModelChoice.M4)
  
  test = test.models(
    seriess = data,
    seriess.info = classes,
    num_samples = 24,
    aggregation = "mean",
    model.choice = ModelChoice.M4
  )
  
  weights = combine.models(test, weightsfn = weighting.inverse)
  future.preds = predict.models(data, weights, preds)
  
  # Set correlation-based forecast where a good match is found
  cor.future.predict.res = pblapply(
    data, function(x) cor.predict(x, M4.hourly, wms.hourly, threshold = 0.995))
  
  cor.future.preds = lapply(cor.future.predict.res, function(x) x$fcst)
  correlated.future = sapply(cor.future.predict.res, function(x) x$correlated)
  
  for (i in which(correlated.future == 1)) {
    future.preds[[i]] = cor.future.preds[[i]]
  }
  
  # Set negative values to 0
  future.preds = lapply(future.preds, negative.to.zero)
  
  return(future.preds)
}
