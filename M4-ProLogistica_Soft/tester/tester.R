################################################################################
# Calculation of scores for models based on one-step-ahead forecasts of prefixes
################################################################################

library(pbapply)
library(robustbase)
library(sets)
library(dplyr)
library(abind)


source("classifier.R")
source("errors_calculation.R")
source("combinations.R")
source("predictions.R")
source("correlation_predict.R")
source("tools.R")
source("criteria.R")


# This function calculates quality of one-step-ahead forecasts for prefixes of
# series, for a passed set of models. This result is later used to determine
# weights for forecasts combination.
# Returns a structure with the following fields:
#   classes ------- types of passed series
#   classes.couts - counts for each type of series
#   settings ------ set of parameters used in test.models call
#   models -------- a list of all models used
#   errors -------- a list, where each element is a array of scores of chosen
#                   models for a single series
#   raw ----------- a list, where each element contains errors of one-step-ahead
#                   forecasts (before their summarization) for a single series.
#                   This is kept so that changing some parameters later does not
#                   require repeating heavy calculations.

# seriess --------- a list of time series in competition format (e.g. M4[1:100])
# forecast.length - if you want to use k-steps-ahead forecasts to score the 
#                   models, set this to k. The value we used is a default k=1.
# num_samples ----- numer of prefixes for a given series used in calculating
#                   model scores. 
# back_by --------- stride between prefixes used in scoring model performance.
#                   Default is 1.
# criteria -------- a named list of criteria to calculate values of. You can
#                   safely use a default value (only sMAPE).
# models.global --- if not NULL, it should be a named list of models. This 
#                   setting overrides models chosen by model.choice. Models 
#                   included in models.global are used for all series. 
# seriess.info ---- output of classify.all(seriess). Must be provided if
#                   model.choice != NULL
# aggregation ----- a method of summarization the one-step-ahead errors into a
#                   single score for a model. One of "mean", "max", "median",
#                   "min" or a number in (0, 1) representing the base for
#                   exponential weights.
# old.result ------ an output of another test.models call, for updating results
# model.choice ---- a function returning a list of models for a given type of
#                   series. See ModelChoice.M4 defined in model_choice_M4.R.
#                   To replicate our result, use model.choice = ModelChoice.M4.
#                   Can't be NULL unless models.global is provided.
# cl -------------- if not NULL, it should be a result of parallel::makeCluster.
#                   See M4makecluster.R for instructions.
# num.portions ---- When using cl != NULL, this should be set to roughly the 
#                   number of CPU cores. It automatically chooses a batch size 
#                   to maximize performance while still having a useful progress
#                   bar. 
test.models = function(seriess, forecast.length = 1, num_samples, back_by = 1, 
                       criteria = list(sMAPE = crit.smape), 
                       models.global = NULL, seriess.info = NULL, 
                       aggregation = "mean", old.result = NULL, 
                       model.choice = NULL, cl = NULL, num.portions = NULL) {
  if (is.null(model.choice) && is.null(models.global)) {
    stop("Provide at least one of (model.choice, models.global)!")
  }
  if (!is.null(old.result)) {
    seriess.info = old.result$classes
    if (!test.models.validate.args(forecast.length, num_samples, back_by, 
                                   criteria, aggregation, old.result)) {
      stop("Passed old.result was calculated for different settings!")
    }
  }
  
  if (!is.null(cl) && is.null(num.portions)) {
    stop(paste("When using cl!=NULL, you should specify num.portions to",
               "greatly speed up the computations. Sensible value is the",
               "number of CPU cores. You can try to find it with",
               "parallel::detectCores()."))
  }
  
  # seriess are classified unless classes are provided
  if (is.null(seriess.info)) {
    seriess.info = classify.all(seriess)
  }
  
  class_counts = data.frame(data.frame(seriess.info) %>% 
                              dplyr::count(period, trend, seasonal))
  print(class_counts)
  
  sclasses = unique(seriess.info)
  sclasses.models = list()
  for (i in 1:nrow(sclasses)) {
    if (!is.null(models.global)) {
      sclasses.models[[i]] = models.global
    } else {
      sclasses.models[[i]] = model.choice(sclasses[i,])
    }
  }
  
  result = list(
    classes = seriess.info,
    classes.counts = class_counts,
    errors = list(),
    settings = list(
      forecast.length = forecast.length,
      num_samples = num_samples,
      back_by = back_by,
      aggregation = aggregation,
      criteria = criteria
    )
  )
  
  if (is.null(num.portions)) {
    parallel.errors.fn = function(idx) {
      return(errors.one(
        seriess[[idx]]$x, forecast.length, num_samples, back_by,
        sclasses.models[[i]], criteria, aggregation,
        old.res.raw = old.result$raw[[idx]]))
    }
  } else {
    parallel.errors.fn = function(inds) {
      return(lapply(inds, function(idx) errors.one(
        seriess[[idx]]$x, forecast.length, num_samples, back_by, 
        sclasses.models[[i]], criteria, aggregation,
        old.res.raw = old.result$raw[[idx]])))
    }
  }
  
  cat("\nCalculating forecasts...", fill = T)
  for (i in 1:nrow(sclasses)) {
    seriess_i_inds = which(
      apply(seriess.info, FUN = function(c) all(c == sclasses[i,]), MARGIN = 1))
    
    print(sclasses[i,])
    
    if (is.null(num.portions)) {
      errone.res = pblapply(seriess_i_inds, parallel.errors.fn, cl = cl)
    } else {
      batches = batch.data(seriess_i_inds, num.portions)
      errone.res = 
        Reduce(c, pblapply(batches, parallel.errors.fn, cl = cl), c())
    }
    
    for (j in 1:length(seriess_i_inds)) {
      idx = seriess_i_inds[[j]]
      result$errors[[idx]] = errone.res[[j]]$result
      result$raw[[idx]] = errone.res[[j]]$result.raw
    }
  }
  
  result$models = {
    res = list()
    for (ms in sclasses.models) {
      for (name in names(ms)) {
        res[[name]] = ms[[name]]
      }
    }
    res
  }
  
  return(result)
}


# A convenient wrapper for test.models, for the case when old.result != NULL.
# Use this to quickly test different values of num_samples and aggregation.
# Set model.choice to ModelChoice.M4 to repeat our results.
test.models.update = function(seriess, old.result, models.global = NULL,
                              num_samples = NULL, aggregation = NULL,
                              model.choice = NULL) {
  test.models(
    seriess = seriess,
    seriess.info = NULL, # classes are read from old.result$classes anyway
    forecast.length = old.result$settings$forecast.length,
    num_samples = ifelse(is.null(num_samples),
                         old.result$settings$num_samples, num_samples),
    back_by = old.result$settings$back_by,
    criteria = old.result$settings$criteria,
    models.global = models.global,
    aggregation = ifelse(is.null(aggregation),
                         old.result$settings$aggregation, aggregation),
    old.result = old.result,
    model.choice = model.choice
  )
}

#####################################################################

# Helper function for validating arguments in test.models. 
# Not intended to be called explicitly.
test.models.validate.args = function(forecast.length, num_samples, back_by, 
                                     criteria, aggregation, old.result) {
  if (forecast.length != old.result$settings$forecast.length ||
      back_by != old.result$settings$back_by) {
    return(FALSE)
  }
  
  crit.names = names(criteria)
  crit.names.old = names(old.result$settings$criteria)
  if ((length(crit.names) != length(crit.names.old)) ||
      !all(sort(crit.names) == sort(crit.names.old))) {
    return(FALSE)
  }
  
  return(TRUE)
}

#####################################################################
# Defined below are convenience functions used in experiments.

# Merges test.models results for different sets of models into one object
# The series data, as well as $classes and $settings fields for each argument 
# should be the same!
# Feed the function with one or more return values of test.models function.
test.models.merge = function(...) {
  to.merge = list(...)
  stopifnot(length(to.merge) > 0)
  result = to.merge[[1]]
  
  if (length(to.merge) > 1) {
    for (res in to.merge[-1]) {
      for (i in 1:length(to.merge[[1]]$errors)) {
        models.used.raw = names(result$raw[[i]])
        models.new.raw = setdiff(names(res$raw[[i]]), models.used.raw)
        raw.new = lapply(models.new.raw, function(name) res$raw[[i]][[name]])
        names(raw.new) = models.new.raw
        result$raw[[i]] = c(result$raw[[i]], raw.new)
        
        models.used.errors = rownames(result$errors[[i]])
        models.new.errors = setdiff(rownames(res$errors[[i]]), 
                                     models.used.errors)
        errors.new = res$errors[[i]][models.new.errors,,, drop = F]
        result$errors[[i]] = abind(result$errors[[i]], errors.new, along = 1)
      }
      
      models.used = names(result$models)
      models.new.names = setdiff(names(res$models), models.used)
      models.new = lapply(models.new.names, function(name) res$models[[name]])
      names(models.new) = models.new.names
      result$models = c(result$models, models.new)
    }
  }
  return(result)
}

# Creates new structure by extracting specific indices from a test.models result
# test.result - return value of test.models function
# indices     - a vector of indices to extract
test.models.extract = function(test.result, indices) {
  stopifnot(max(indices) <= length(test.result$errors))
  classes.new = test.result$classes[indices,, drop = F]
  return(list(
    classes = classes.new,
    classes.counts = data.frame(data.frame(classes.new) %>% 
                                  dplyr::count(period, trend, seasonal)),
    errors = test.result$errors[indices],
    settings = test.result$settings,
    raw = test.result$raw[indices],
    models = test.result$models
  ))
}
