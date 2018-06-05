################################################################################
# Series classification with respect to existence of trend and/or seasonality.
################################################################################

library(trend)


# Seasonality test
classify.seasonal = function(series) {
  freq = frequency(series)
  if (freq == 1) {
    res = FALSE
  } else {
    r = acf(series, lag.max = freq, plot = F)$acf[-1]
    res = abs(r[freq]) > 
      (1.645 * sqrt((1 + 2 * sum(r[1:(freq - 1)]^2)) / length(series)))
    
    if (is.na(res)) {
      res = FALSE
    }
  }
  return(res)
}

# Seasonality test used in M4 benchmarks
classify.seasonal.benchmark = function(input) {
  tcrit = 1.645
  ppy = frequency(input)
  if ((ppy == 1) || (length(input) < 3 * ppy)) {
    test_seasonal = FALSE
  } else {
    xacf = acf(input, lag.max = ppy, plot = F)$acf[-1, 1, 1]
    clim = tcrit / sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal = abs(xacf[ppy]) > clim[ppy]
    
    if (is.na(test_seasonal)) {
      test_seasonal = FALSE
    }
  }
  return(test_seasonal)
}

# Mann-Kendall trend test
# isseasonal - logical, determines whether to use Seasonal MK test
classify.trend = function(series, isseasonal, confidence = .95) {
  if (!isseasonal) {
    test = mk.test(series)
  } else {
    test = smk.test(series)
  }
  istrend = test$p.value < 1 - confidence
  if (is.na(istrend)) {
    istrend = F
  }
  return(istrend)
}

# Classify a single series in competition format
classify = function(series) {
  isseasonal = classify.seasonal(series$x)
  list(
    period = as.character(series$period),
    trend = classify.trend(series$x, isseasonal),
    seasonal = isseasonal
  )
}

# Classify a list of time series in competition format. Result is a matrix.
classify.all = function(seriess) {
  cat("Classifying series...", fill = T)
  seriess.info = pblapply(seriess, classify)
  seriess.info.names = names(seriess.info[[1]])
  seriess.info = matrix(
    unlist(seriess.info), ncol = length(seriess.info[[1]]), byrow = T)
  colnames(seriess.info) = seriess.info.names
  cat("Done!", fill = T)
  return(seriess.info)
}