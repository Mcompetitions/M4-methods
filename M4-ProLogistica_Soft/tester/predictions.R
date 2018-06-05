################################################################################
# Calculating final forecasts
################################################################################

library(sets)
library(dplyr)

source("tools.R")


# Takes the output of combine.models (see combinations.R) and forecasts for
# single models precalculated with precalc.preds (see below), and produces
# final forecasts (the weighted average of single-model propositions).
# Returns a list, where each element is a forecast for a single series.

# seriess ------- a list of time series in competition format (e.g. M4[1:1000])
# comb.results -- output of combine.models function
# precalculated - output of precalc.preds. Forecasts of seriess for different
#                 single models.
predict.models = function(seriess, comb.results, precalculated) {
  predictions = list()
  
  cat("Calculating future forecasts...", fill = T)
  pb = txtProgressBar(max = length(seriess), width = 50, style = 3)
  for (i in 1:length(seriess)) {
    model.names = comb.results$model.names[[i]]
    for (name in model.names) {
      if (!(name %in% rownames(precalculated[[i]]))) {
        stop(paste("Model ", name, " not in precalculated[[", i, "]]!", 
                   sep = ""))
      }
    }
    fcsts = precalculated[[i]][model.names,, drop = F]
    weights = comb.results$weights[[i]]
    predictions[[i]] = colSums(fcsts * weights) / sum(weights)
    setTxtProgressBar(pb, i)
  }
  cat("  Done!", fill = T)
  return(predictions)
}


# Calculate forecasts of provided models for a given list of time series. Can
# be greatly sped up by using cl != NULL to enable parallel computations.
# Returns a list of matrices, where each matrix contains the forecasts of a 
# single series for the chosen models.

# seriess ------- a list of time series in competition format (e.g. M4[1:1000])
# seriess.info -- output of classify.all(seriess). Must be provided if
#                 model.choice != NULL
# model.choice -- a function returning a list of models for a given type of
#                 series. See ModelChoice.M4 defined in model_choice_M4.R.
#                 To replicate our result, use model.choice = ModelChoice.M4.
#                 Can't be NULL unless models.global is provided.
# models.global - if not NULL, it should be a named list of models. This setting
#                 overrides models chosen by model.choice. Models included
#                 in models.global are used for all series.
# cl ------------ if not NULL, it should be an output of parallel::makeCluster.
#                 See M4makecluster.R for instructions.
# num.portions -- When using cl != NULL, this should be set to roughly the 
#                 number of CPU cores. It automatically chooses a batch size 
#                 to maximize performance while still having a useful progress
#                 bar. 
precalc.preds = function(seriess, seriess.info = NULL,
                         model.choice = NULL, models.global = NULL,
                         cl = NULL, num.portions = NULL) {
  if (is.null(model.choice) && is.null(models.global)) {
    stop("Please, provide at least one of (model.choice, models.global).")
  }
  
  if (is.null(models.global) && is.null(seriess.info)) {
    stop("seriess.info is required unless using global models.")
  }
  
  if (!is.null(cl) && is.null(num.portions)) {
    stop(paste("When using cl!=NULL, you should specify num.portions to",
               "greatly speed up the computations. Sensible value is the",
               "number of CPU cores. You can try to find it with",
               "parallel::detectCores()."))
  }
  
  cat("Precalculating future forecasts...", fill = T)
  
  preds.fn = function(i) {
    h = seriess[[i]]$h
    if (!is.null(models.global)) {
      models = models.global
    } else {
      models = model.choice(seriess.info[i,])
    }
    fcsts = matrix(nrow = length(models), ncol = h)
    for (j in 1:length(models)) {
      fcsts[j,] = models[[j]](seriess[[i]]$x, h)
    }
    rownames(fcsts) = names(models)
    return(fcsts)
  }
  
  parallel.preds.fn = function(inds) {
    return(lapply(inds, preds.fn))
  }
  
  if (is.null(num.portions)) {
    predictions = pblapply(1:length(seriess), parallel.preds.fn, cl = cl)
    predictions = lapply(predictions, function(x) x[[1]])
  } else {
    batches = batch.data(1:length(seriess), num.portions)
    predictions = 
      Reduce(c, pblapply(batches, parallel.preds.fn, cl = cl), c())
  }
  
  cat("  Done!", fill = T)
  return(predictions)
}


# Convenience function to merge precalculated predictions for different sets of
# models into one object. The series data used in calculating each argument must
# be the same. Feed this function with one or more outputs of precalc.preds.
precalc.preds.merge = function(...) {
  to.merge = list(...)
  stopifnot(length(to.merge) > 0)
  fh = ncol(to.merge[[1]][[1]])
  num.series = length(to.merge[[1]])
  result.all = list()
  
  for (x in to.merge) {
    stopifnot(length(x) == num.series)
    for (y in x) {
      stopifnot(ncol(y) == fh)
    }
  }
  
  for (i in 1:num.series) {
    models.used = c()
    to.bind = list()
    for (j in 1:length(to.merge)) {
      x = to.merge[[j]][[i]]
      models.left = setdiff(rownames(x), models.used)
      models.used = c(models.used, models.left)
      if (length(models.left)) {
        to.bind[[j]] = x[models.left,, drop = F]
      }
    }
    result.all[[i]] = do.call(rbind, to.bind)
  }
  return(result.all)
}
