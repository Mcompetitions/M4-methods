################################################################################
# This file determines which models are consider for which type of series.
################################################################################

source("classifier.R")
source("models.R")


# Given a type of series, return a list of models.
# sclass - type of series, result of 'classify' function from classifier.R
ModelChoice.M4 = function(sclass) {
  pe = sclass[["period"]]
  se = as.logical(sclass[["seasonal"]])
  tr = as.logical(sclass[["trend"]])
  
  # Yearly
  if (pe == "Yearly") {
    if (!tr) {
      list(
        naive = model.naive,
        thetaf = model.thetaf,
        otm = model.otm,
        ets.azn = model.ets("AZN"),
        ets.znz = model.ets("ZNZ"),
        ets.zzz.damped = model.ets.damped("ZZZ")
      )
    } else if (tr) {
      list(
        otm = model.otm,
        ets.azn = model.ets("AZN"),
        ets.zzz.damped = model.ets.damped("ZZZ"),
        auto.arima = model.auto.arima,
        econometric.tlogt = model.econometric.tlogt
      )
    }
  }
  
  # Quarterly
  else if (pe == "Quarterly") {
    if (!se && !tr) {
      list(
        naive = model.naive,
        ses = model.ses,
        thetaf = model.thetaf,
        otm = model.otm,
        ets.zzz = model.ets("ZZZ")
      )
    } else if (!se && tr) {
      list(
        auto.arima = model.auto.arima,
        thetaf = model.thetaf,
        otm = model.otm,
        ets.zzz = model.ets("ZZZ"),
        ets.zzz.damped = model.ets.damped("ZZZ"),
        econometric.one = model.econometric.one
      )
    } else if (se && !tr) {
      list(
        otm = model.otm,
        ets.znz =  model.ets("ZNZ"),
        ets.zzz = model.ets("ZZZ"),
        ets.zzz.damped = model.ets.damped("ZZZ"),
        auto.arima = model.auto.arima,
        naive = model.naive
      )
    } else if (se && tr) {
      list(
        thetaf = model.thetaf,
        otm = model.otm,
        auto.arima = model.auto.arima,
        ets.zzz = model.ets("ZZZ"),
        ets.znz =  model.ets("ZNZ"),
        ets.zzz.damped = model.ets.damped("ZZZ"),
        econometric.one = model.econometric.one,
        econometric.t = model.econometric.t
      )
    }
  }
  
  # Monthly
  else if (pe == "Monthly") {
    if (!se && !tr) {
      list(
        thetaf = model.thetaf,
        otm = model.otm,
        auto.arima = model.auto.arima,
        ets.zzz.damped = model.ets.damped("ZZZ"),
        ets.znz =  model.ets("ZNZ"),
        econometric.one = model.econometric.one,
        econometric.t = model.econometric.t,
        econometric.logt = model.econometric.logt,
        econometric.logt_seasonalize = mod.seasonalize(model.econometric.logt),
        econometric.tlogt = model.econometric.tlogt,
        econometric.tlogt_seasonalize = mod.seasonalize(model.econometric.tlogt)
      )
    } else if (!se && tr) {
      list(
        ses = model.ses,
        auto.arima = model.auto.arima,
        ets.zzz.damped = model.ets.damped("ZZZ"),
        ets.znz =  model.ets("ZNZ"),
        naive = model.naive,
        naive2 = model.naive2,
        econometric.one = model.econometric.one,
        econometric.one_seasonalize = mod.seasonalize(model.econometric.one),
        econometric.logt = model.econometric.logt,
        econometric.logt_seasonalize = mod.seasonalize(model.econometric.logt),
        econometric.tlogt_seasonalize = mod.seasonalize(model.econometric.tlogt)
      )
    } else if (se && !tr) {
      list(
        thetaf = model.thetaf,
        otm = model.otm,
        ses = model.ses,
        auto.arima = model.auto.arima,
        ets.zzz = model.ets("ZZZ"),
        ets.zzz.damped = model.ets.damped("ZZZ"),
        ets.znz =  model.ets("ZNZ"),
        naive = model.naive,
        naive2 = model.naive2,
        econometric.logt_seasonalize = mod.seasonalize(model.econometric.logt),
        econometric.tlogt = model.econometric.tlogt,
        econometric.tlogt_seasonalize = mod.seasonalize(model.econometric.tlogt)
      )
    } else if (se && tr) {
      list(
        otm = model.otm,
        auto.arima = model.auto.arima,
        ets.zzz = model.ets("ZZZ"),
        ets.zzz.damped = model.ets.damped("ZZZ"),
        naive = model.naive,
        econometric.one_seasonalize = mod.seasonalize(model.econometric.one),
        econometric.logt_seasonalize = mod.seasonalize(model.econometric.logt),
        econometric.tlogt_seasonalize = mod.seasonalize(model.econometric.tlogt)
      )
    }
  }
  
  # Hourly
  else if (pe == "Hourly") {
    list(
      thetaf_f168 = mod.change.freq(model.thetaf, 168),
      naive = model.naive,
      econometric.one_f168.seasonalize = 
        mod.change.freq(mod.seasonalize(model.econometric.one), 168),
      econometric.t_seasonalize = mod.seasonalize(model.econometric.t),
      econometric.t_f168.seasonalize = 
        mod.change.freq(mod.seasonalize(model.econometric.t), 168),
      econometric.logt_f168.seasonalize = 
        mod.change.freq(mod.seasonalize(model.econometric.logt), 168),
      econometric.tlogt_seasonalize = mod.seasonalize(model.econometric.tlogt),
      econometric.tlogt_f168.seasonalize = 
        mod.change.freq(mod.seasonalize(model.econometric.tlogt), 168)
    )
  }
  
  # Weekly
  else if (pe == "Weekly") {
    list(
      thetaf_f52 = mod.change.freq(model.thetaf, 52),
      thetaf_f52.t260 = mod.trim(mod.change.freq(model.thetaf, 52), 260),
      otm_f52.t260 = mod.change.freq(mod.trim(model.otm, 260), 52),
      auto.arima_t156 = mod.trim(model.auto.arima, 156)
    )
  }
  
  # Daily
  else if (pe == "Daily") {
    list(
      thetaf_f7 = mod.change.freq(model.thetaf, 7),
      ses = model.ses,
      ets.zzz.damped_t182 = mod.trim(model.ets.damped("ZZZ"), 182),
      naive = model.naive,
      naive2_f7 = mod.change.freq(model.naive2, 7),
      ets.zzz_t60 = mod.trim(model.ets("ZZZ"), 60),
      thetaf_t30 = mod.trim(model.thetaf, 30),
      ets.zzz.damped_t30 = mod.trim(model.ets.damped("ZZZ"), 30),
      econometric.one_t30 = mod.trim(model.econometric.one, 30),
      econometric.t_t30 = mod.trim(model.econometric.t, 30),
      econometric.logt_t30 = mod.trim(model.econometric.logt, 30),
      econometric.tlogt_t30 = mod.trim(model.econometric.tlogt, 30),
      econometric.one_t3 = mod.trim(model.econometric.one, 3),
      econometric.one_t5 = mod.trim(model.econometric.one, 5),
      econometric.one_t10 = mod.trim(model.econometric.one, 10)
    )
  }
  
  else {
    stop("Series type not implemented.")
  }
}
