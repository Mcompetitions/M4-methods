################################################################################
# Decorators for changing behavior of models defined in models.R
################################################################################

library(pracma)

source("tools.R")


# Changes a model f to a new model that accounts for seasonality.
# E.g. mod.seasonalize(model.naive) results in a model, where the series is
# first deseasonalized, then the naive forecast in created, after which the
# seasonality is brought back.
mod.seasonalize = function(f, type = "multiplicative") {
  function(x, fh) {
    freq = frequency(x)
    notseasonal = (freq == 1) || (length(x) < 2 * freq)
    
    if (!notseasonal) {
      seasonal_decomp = decompose(x, type = type)$seasonal
      if (type == "multiplicative") {
        x = x / seasonal_decomp
      } else {
        x = x - seasonal_decomp
      }
    }
    
    xf = f(x, fh)
    
    if (!notseasonal) {
      v = as.vector(tail(seasonal_decomp, freq))
      v = rep_len(v, fh)
      if (type == "multiplicative") {
        xf = xf * v
      } else {
        xf = xf + v
      }
    }
    return(xf)
  }
}

# Same as mod.seasonalize, but uses slightly different seasonality test
# (the one used in M4 Naive2 benchmark).
mod.seasonalize.benchmark = function(f) {
  function(x, fh) {
    isseasonal = classify.seasonal.benchmark(x)
    freq = frequency(x)
    
    if (isseasonal) {
      seasonal_decomp = decompose(x, type = "multiplicative")$seasonal
      x = x / seasonal_decomp
    }
    
    xf = f(x, fh)
    
    if (isseasonal) {
      v = as.vector(tail(seasonal_decomp, freq))
      v = rep_len(v, fh)
      xf = xf * v
    }
    return(xf)
  }
}

# Sets negative values in the result of f to zeros
mod.noneg = function(f) {
  function(x, fh) {
    xf = f(x, fh)
    xf[xf < 0] = 0
    return(xf)
  }
}

# Changes frequency of a series before applying a model
mod.change.freq = function(f, new.freq) {
  function(x, fh) {
    return(f(ts(x, frequency = new.freq), fh))
  }
}

# Cut a series down to tail.length before applying a model
mod.trim = function(f, tail.length) {
  function(x, fh) {
    return(f(tail(x, tail.length), fh))
  }
}
