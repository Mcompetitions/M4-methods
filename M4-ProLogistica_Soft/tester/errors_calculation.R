################################################################################
# This file contains helper functions for calculating errors.
# Functions defined here are not intended to be directly called by the user.
################################################################################

# Calculates aggregated criteria values for one series
errors.one = function(series, horizon, num_samples, back_by, models, criteria, 
                      aggregation = "mean", old.res.raw = NULL) {
  len = length(series)
  
  # stops if the series is too short
  min_length = len - horizon - (num_samples - 1) * back_by
  stopifnot(min_length > 0)
  
  third.axis.len = 1
  result = array(dim = c(length(models), length(criteria), third.axis.len))
  colnames(result) = names(criteria)
  rownames(result) = names(models)
  
  result.raw = list()
  
  for (model in names(models)) {
    start_from = 1
    idx = len - horizon
    
    if (!is.null(old.res.raw[[model]])) {
      num_all = max(nrow(old.res.raw[[model]]), num_samples)
      num_existing = nrow(old.res.raw[[model]])
      
      model_res = array(dim = c(num_all, length(criteria), third.axis.len))
      
      model_res[1:num_existing,,] = old.res.raw[[model]]
      start_from = start_from + num_existing
      idx = idx - num_existing * back_by
    } else {
      model_res = array(dim = c(num_samples, length(criteria), third.axis.len))
    }
    
    colnames(model_res) = names(criteria)
    
    if (start_from <= num_samples) {
      for (i in start_from:num_samples) {
        train = head(series, idx)
        test = series[(idx + 1):(idx + horizon)]
        errors = errors.model(models[[model]], train, test, horizon, criteria)
        stopifnot(length(errors) == length(model_res[i,,])) # just in case
        model_res[i,,] = errors
        idx = idx - back_by
      }
    }
    result.raw[[model]] = model_res
    
    model_res = errors.aggregate(
      model_res[1:num_samples,,, drop = F], aggregation)
    stopifnot(length(model_res) == length(result[model,,]))
    result[model,,] = model_res
  }
  
  for (model in names(old.res.raw)) {
    if (!(model %in% names(result.raw))) {
      result.raw[[model]] = old.res.raw[[model]]
    }
  }
  
  return(list(result = result, result.raw = result.raw))
}

# Calculates criteria values for model on series_train
errors.model = function(model, series_train, series_test, horizon, criteria) {
  errors = array(dim = c(length(criteria), 1))
  rownames(errors) = names(criteria)
  fcst = model(series_train, horizon)
  for (crit in names(criteria)) {
    # passing series_train is necessary for MASE
    err = mean(criteria[[crit]](series_train, series_test, as.numeric(fcst)))
    errors[crit,] = err
  }
  return(errors)
}

# Aggregates errors
# errors_arr  - array of errors, dim = (#samples, #criteria, horizon)
#               The most recent one is at the top.
# aggreg_type - method of aggregation, should be "mean", "max", "min", 
#               "median", or a base number for exponential weights.
errors.aggregate = function(errors_arr, aggreg_type) {
  if (aggreg_type == "mean") {
    colMeans(errors_arr)
  } else if (aggreg_type == "max") {
    apply(errors_arr, MARGIN = c(2,3), FUN = max)
  } else if (aggreg_type == "min") {
    apply(errors_arr, MARGIN = c(2,3), FUN = min)
  } else if (aggreg_type == "median") {
    apply(errors_arr, MARGIN = c(2,3), FUN = median)
  } else if (is.numeric(aggreg_type)) {
    # 1 - a, bo w aplikacji jest odwrotnie
    coeffs = expCoeffs(nrow(errors_arr), 1 - aggreg_type)
    colSums(errors_arr * coeffs)
  } else {
    stop("Incorrect aggreg_type!")
  }
}

# Calculates exponential weights
expCoeffs = function(n, b) { # b: base
  coeffs = Reduce('*', rep(b, n), accumulate = T)
  coeffs / sum(coeffs)
}