################################################################################
# Calculate forecasts for Yearly data
################################################################################

source("../M4/M4tools.R")

final.forecast.yearly = function(data) {
  if (length(data) == 0) {
    return(list())
  }
  
  classes = classify.all(data)
  preds = precalc.preds(data, classes, model.choice = ModelChoice.M4)
  
  test = test.models(
    seriess = data,
    seriess.info = classes,
    num_samples = 3,
    aggregation = 0.3,
    model.choice = ModelChoice.M4
  )
  
  weights = combine.models(test, weightsfn = weighting.squared)
  future.preds = predict.models(data, weights, preds)
  
  # Set negative values to 0
  future.preds = lapply(future.preds, negative.to.zero)
  
  return(future.preds)
}
