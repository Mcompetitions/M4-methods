################################################################################
# Defines a multicore cluster for M4 calculations.
################################################################################

library(parallel)

source("tester.R")

# set the number of cores
num.cores = detectCores()

# M4cluster should be used as a cl argument in test.models and precalc.preds.
# You also need to put your data (e.g. M4.daily object) to clusterExport.
# This takes a lot of memory, so you probably want to calculate different
# periods separtely.
stopCluster(M4cluster)
M4cluster = makeCluster(getOption("cl.cores", num.cores))
clusterExport(M4cluster, list(
  # Data sets need to be here
  # e.g. "M4.daily",
  # Functions required by the tester
  "errors.one", 
  "errors.model",
  "errors.aggregate", 
  "seasadj",
  "snaive", 
  "naive",
  "expCoeffs",
  "ses",
  "ets", 
  "thetaf.fixed",
  "otm.fixed",
  "twoTL.fixed",
  "forecast",
  "auto.arima",
  "econometric",
  "classify.seasonal.benchmark",
  "Reshape",
  "mod.seasonalize",
  "mod.trim",
  "mod.change.freq",
  "ceil",
  # Models
  "model.naive",
  "model.naive2",
  "model.ses",
  "model.ets",
  "model.ets.damped",
  "model.auto.arima",
  "model.thetaf",
  "model.otm",
  "model.econometric.one",
  "model.econometric.t",
  "model.econometric.logt",
  "model.econometric.tlogt"))
