################################################################################
# Models used in our experiments. Mostly wrappers for forecast 8.2 functions.
################################################################################

library(forecast)
library(pracma)

source("modifiers.R")
source("thetaf_fixed.R")
source("otm_fixed.R")

# Each model takes a series s and horizon fh, and returns forecast for fh steps.

# Naive
model.naive = function(s, fh) naive(s, h = fh)$mean

# Naive2 benchmark from M4 used in OWA calculation
model.naive2 = mod.seasonalize.benchmark(model.naive)

# Simple exponential smoothing
model.ses = function(s, fh) ses(s, h = fh)$mean

# Exponential smoothing with and without trend dampening
# We used mostly model = "ZZZ" for automatic model choice
model.ets = function(model) {
  function(s, fh) forecast(ets(s, model = model), h = fh)$mean}
model.ets.damped = function(model) {
  function(s, fh) forecast(ets(s, model = model, damped = T), h = fh)$mean}

# Arima with automatic model choice
model.auto.arima = function(s, fh) forecast(auto.arima(s), h = fh)$mean

# Theta model
# see thetaf_fixed.R for explanation why it's "fixed"
model.thetaf = function(s, fh) thetaf.fixed(s, h = fh)$mean

# Optimized Theta Method from forecTheta 2.2
# see otm_fixed.R
model.otm = function(s, fh) {
  fcst = otm.fixed(s, ifelse(fh == 1, 2, fh), level = NULL)$mean
  return(head(fcst, fh))
}

# Econometric models (wrappers for 'econometric' function defined below)
model.econometric.one = function(s, fh) econometric(s, fh, NULL)
model.econometric.t = function(s, fh) econometric(s, fh, c(identity))
model.econometric.logt = function(s, fh) econometric(s, fh, c(log))
model.econometric.tlogt = function(s, fh) econometric(s, fh, c(identity, log))

# General econometric model. Linear regression is applied to various trend
# functions. fns argument defines regressors. A constant is always considered.
# For example, fns = c(indentity, log) results in regressors 1, t, log(t), 
# where t = 1:length(x) is time.
econometric = function(x, fh, fns) {
  t = 1:length(x)
  if (length(fns)) {
    factors = sapply(fns, function(f) f(t))
    factors = Reshape(factors, length(x), length(fns))
    
    t.future = (length(x) + 1):(length(x) + fh)
    factors.future = sapply(fns, function(f) f(t.future))
    
    termlabels = sapply(1:length(fns), 
                        function(i) paste("factors[,", i, "]", sep = ""))
    formula = reformulate(termlabels = termlabels, response = "x")
  } else {
    formula = as.formula("x ~ 1")
  }
  model = lm(formula)
  coeffs = model$coefficients
  
  predictions = rep(coeffs[[1]], fh)
  
  if (length(fns)) {
    factors.future = Reshape(factors.future, fh, length(fns))
    predictions = predictions + colSums(coeffs[-1] %*% t(factors.future))
  }
  return(predictions)
}