################################################################################
# Various M4-related functions
################################################################################

# Enter a path to the 'tester' directory here
# setwd("path/to/the/'tester'/directory/")


library(M4comp2018)

data(M4)

source("tester.R")
source("../M4/model_choice_M4.R")

# Calculates M4 error on the test set.
# predictions - a list of forecasts
# seriess ----- a sublist of splitM4 output
# criteria ---- a named list of criteria to calculate values of
# owa --------- whether or not to also calculate OWA
# naive2res --- results of naive2 benchmark on seriess, required if owa == T
checkM4err = function(predictions, seriess, criteria, 
                      owa = F, naive2res = NULL) {
  errs = array(
    dim = c(length(seriess), length(criteria), length(predictions[[1]])))
  colnames(errs) = names(criteria)
  
  cat("Calculating test errors...", fill = T)
  pb = txtProgressBar(max = length(seriess), width = 50, style = 3)
  for (i in 1:length(seriess)) {
    for (name in names(criteria)) {
      errs[i, name, ] = criteria[[name]](seriess[[i]]$x, seriess[[i]]$xx, 
                                       as.numeric(predictions[[i]]))
    }
    setTxtProgressBar(pb, i)
  }
  cat("  Done!", fill = T)
  result = list(
    errors = errs,
    errors.mean.per.step = t(colMeans(errs))
  )
  result$errors.mean = colMeans(result$errors.mean.per.step)
  
  if (owa) {
    stopifnot(!is.null(naive2res))
    stopifnot(all(c("sMAPE", "MASE") %in% names(criteria)))
    stopifnot(all(c("sMAPE", "MASE") %in% names(naive2res)))
    smape.res = result$errors.mean[["sMAPE"]] / naive2res[["sMAPE"]]
    mase.res = result$errors.mean[["MASE"]] / naive2res[["MASE"]]
    result$errors.mean[["OWA"]] = mean(c(smape.res, mase.res))
  }
  return(result)
}

# Splits each series in M4 data into train-test parts.
splitM4 = function() {
  data(M4)
  cat("Splitting M4 into train-test...", fill = T)
  pb = txtProgressBar(max = length(M4), width = 50, style = 3)
  for (i in 1:length(M4)) {
    M4[[i]]$xx = tail(M4[[i]]$x, M4[[i]]$h)
    M4[[i]]$x = head(M4[[i]]$x, -M4[[i]]$h)
    M4[[i]]$n = M4[[i]]$n - M4[[i]]$h
    setTxtProgressBar(pb, i)
  }
  cat("  Done!", fill = T)
  return(M4)
}
