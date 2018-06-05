################################################################################
# Example experiment on the holdout set for M4 Yearly data
################################################################################

source("../M4/M4tools.R")

# prepare train-test split for Yearly data
M4split = splitM4()
M4.test.yearly = Filter(function(s) s$period == "Yearly", M4split)
# E.g. for series "Y1":
# M4.test.yearly[[1]]$xx is a holdout tail (of length M4.test.yearly[[1]]$h)
# M4.test.yearly[[1]]$x is the remaining prefix

# choose a subset (to make example calculations faster)
M4.yearly.short = which(sapply(M4.test.yearly, function(x) length(x$x)) < 12)
M4.test.yearly = M4.test.yearly[-M4.yearly.short][1:200]

# classify series
M4.test.yearly.classes = classify.all(M4.test.yearly)

# use the same set of models for every series
M4.models.global.yearly = list(
  thetaf = model.thetaf,
  ses = model.ses,
  naive = model.naive,
  naive2 = model.naive2, # in case of yearly series, this is equivalent to naive
  ets.zzz.damped = model.ets.damped("ZZZ")
)

# determine which criteria values we are interested in
M4.criteria = list(sMAPE = crit.smape, MASE = crit.mase)

# calculate forecasts of the holdout part for all models and all series
M4.test.yearly.preds = precalc.preds(
  M4.test.yearly, M4.test.yearly.classes, 
  models.global = M4.models.global.yearly)

# calculate errors of one-step-ahead forecasts of prefixes of training part
M4.yearly.test.result = test.models(
  seriess = M4.test.yearly,
  seriess.info = M4.test.yearly.classes,
  # we calculate errors for 6 prefixes
  num_samples = 6,
  # we are going to need sMAPE and MASE
  criteria = M4.criteria,
  # errors are summarized using a simple mean
  aggregation = "mean",
  # use a global list of models for each series
  models.global = M4.models.global.yearly,
  # to allow for different model sets for different types of series, 
  # we could provide model.choice instead, e.g.
  # models.global = NULL, model.choice = ModelChoice.M4
  model.choice = NULL
)

{
  # Determine naive2 error for OWA calculation
  M4.yearly.naive2preds = lapply(
    M4.test.yearly.preds, function(x) x["naive2",, drop = F])
  M4.yearly.naive2res = checkM4err(
    M4.yearly.naive2preds, M4.test.yearly, M4.criteria)
  cat("Naive2 errors:", fill = T)
  M4.yearly.naive2res$errors.mean
}

# Calculate errors of an ensemble created from chosen methods and weights
measure.performance = function() { 
  # Transform summarized one-step-ahead forecasts into weights
  M4.test.yearly.weights = combine.models(
    M4.yearly.test.result, crit = "sMAPE", weightsfn = weighting.squared)
  # Use those weights to calcualate final forecasts
  M4.test.yearly.preds.weighted = predict.models(
    M4.test.yearly, M4.test.yearly.weights, M4.test.yearly.preds)
  # Show error values on a holdout set
  M4.yearly.err = checkM4err(
    M4.test.yearly.preds.weighted, M4.test.yearly, M4.criteria,
    owa = T, naive2res = M4.yearly.naive2res$errors.mean)
  cat("Errors on M4 yearly sample:", fill = T)
  print(M4.yearly.err$errors.mean)
  return(list(
    weights = M4.test.yearly.weights, 
    preds = M4.test.yearly.preds.weighted,
    error = M4.yearly.err
  ))
}

################################################################################
# Here we have everything we need to start experiments. We can choose different
# subsets of previously chosen set of models, modify num_samples and aggregation
# parameters, and try different weighting schemes.
# First, let's see how we scored.

perf = measure.performance()

# It should say
# sMAPE      MASE       OWA 
# 0.1175957 3.2537655 0.8068279
#
# These are errors obtained with an ensemble of all models in
# M4.models.global.yearly, with num_samples = 6 and aggregation = "mean".
# We can now modify the parameters and see if we can do better.
#
# test.models.update will update M4.yearly.test.result with new values.
# The M4.yearly.test.result$raw field remains unchanged to allow going back to
# previous parameter values. If num_samples is higher than previously set, or if
# models set is larger, the missing values will be calculated and added to $raw.
#
# For example, let's try to restrict ourselves to two models:
#   models.global = only thetaf and ets.zzz.damped

M4.yearly.test.result = test.models.update(
  M4.test.yearly, M4.yearly.test.result, 
  models.global = M4.models.global.yearly[c(1,5)],
  num_samples = 6,
  aggregation = "mean"
)

perf = measure.performance()

# Now we got
# sMAPE      MASE       OWA 
# 0.1129833 2.9405836 0.7533806 
#
# The new configuration worked much better.
#
# We can go further, like lower the number of historical results we consider:
#   num_samples = 3

M4.yearly.test.result = test.models.update(
  M4.test.yearly, M4.yearly.test.result, 
  models.global = M4.models.global.yearly[c(1,5)],
  num_samples = 3,
  aggregation = "mean"
)

perf = measure.performance()

# We improved further:
# sMAPE      MASE       OWA 
# 0.1120100 2.9286798 0.7484682
#
# Finally, let's try different summarization function fror historical errors:
# aggregation = 0.3 (exponential weights with base 0.3)

M4.yearly.test.result = test.models.update(
  M4.test.yearly, M4.yearly.test.result, 
  models.global = M4.models.global.yearly[c(1,5)],
  num_samples = 3,
  aggregation = 0.3
)

perf = measure.performance()

# The score is even better:
# sMAPE      MASE       OWA 
# 0.1117517 2.9138008 0.7457875

# There is no problem quickly going back to initial values if we wanted to:
M4.yearly.test.result = test.models.update(
  M4.test.yearly, M4.yearly.test.result, 
  models.global = M4.models.global.yearly,
  num_samples = 6,
  aggregation = "mean"
)

perf = measure.performance()

# Once again we get
# sMAPE      MASE       OWA 
# 0.1175957 3.2537655 0.8068279



########## BONUS ##########

# print independent forecasts of each model for 17th series
M4.test.yearly.preds[[17]]

# print final forecast
perf$preds[[17]]

# print chosen weights
perf$weights$weights[[17]]

{ # Calculate errors for each method separately
  M4.yearly.ers = list()
  M4.yearly.nms = names(M4.models.global.yearly)
  for (i in 1:length(M4.yearly.nms)) {
    name = M4.yearly.nms[[i]]
    pred = lapply(M4.test.yearly.preds, function(x) x[name,, drop = F])
    M4.yearly.ers[[name]] = checkM4err(pred, M4.test.yearly, M4.criteria)
  }
}

{ # Print average errors on all data for separate models
  cat("MODEL\tsMAPE\tMASE", fill = T)
  for (i in 1:length(M4.yearly.ers)) {
    er = M4.yearly.ers[[i]]$errors.mean
    cat(names(M4.yearly.ers)[[i]], "\t", er[["sMAPE"]], 
        "\t", er[["MASE"]], fill = T)
  }
}

{ # Print errors for a single series
  series.number = 17
  cat("MODEL\tsMAPE\tMASE", fill = T)
  for (i in 1:length(M4.yearly.ers)) {
    name = names(M4.yearly.ers)[[i]]
    if (name %in% names(M4.models.global.yearly)) {
      cat(name, "\t",
          rowMeans(M4.yearly.ers[[name]]$errors[series.number,,]), fill = T)
    }
  }
}
