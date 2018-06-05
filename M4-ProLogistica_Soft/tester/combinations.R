################################################################################
# Calculation of weights for the final forecasts from scores of the models.
################################################################################

library(pracma)


# Take the result of test.models and calculate weights for included models.
# test.results - output of 'test.models' function from tester.R
# crit --------- string representing the name of criterion to serve as a base
#                for weights calculation. It must be available in test.results.
#                Default is "sMAPE".
# weightsfn ---- weighting scheme. See below for possible values.
combine.models = function(test.results, crit = "sMAPE", weightsfn) {
  results = list(
    models = test.results$models,
    model.names = list(),
    weights = list()
  )
  
  for (i in 1:length(test.results$errors)) {
    errs_i = test.results$errors[[i]]
    num.methods = nrow(errs_i)
    steps = dim(errs_i)[[3]]
    if (crit == "OWA") {
      prep_errs_i = test.results$errors.owa[[i]]
    } else {
      prep_errs_i = errs_i[, crit,]
    }
    if (steps > 1) {
      prep_errs_i = Reshape(prep_errs_i, num.methods, steps)
    }
    results$weights[[i]] = weightsfn(prep_errs_i)
    results$model.names[[i]] = rownames(errs_i)
  }
  return(results)
}


# Weighting schemes. Not intended to be directly called by the user. Pass these
# functions as 'weightsfn' argument to combine.models.

# Weights are inverted scores
weighting.inverse = function(errs) {
  return(1 / (errs + 1e-30))
}

# Square weights
weighting.squared = function(errs) {
  return(1 / (errs^2 + 1e-30))
}

# Equal weights
weighting.naive = function(errs) {
  len.weights = ifelse(is.vector(errs), length(errs), ncol(errs))
  return(rep(1, len.weights))
}

# Exponential weights
weighting.exp = function(errs) {
  errs = 1 / (errs + 1e-30)
  if (is.vector(errs)) {
    errs = errs - max(errs)
  } else {
    errs = t(t(errs) - apply(errs, MARGIN = 2, FUN = max))
  }
  return(exp(errs))
}

# Selects a single best model
weighting.single = function(errs) {
  if (is.vector(errs)) {
    weights = rep(0, length(errs))
    weights[[which.min(errs)]] = 1
  } else {
    weights = zeros(nrow(errs), ncol(errs))
    min.inds = apply(weights, MARGIN = 2, FUN = which.min)
    weights[cbind(min.inds, 1:ncol(errs))] = 1
  }
  return(weights)
}