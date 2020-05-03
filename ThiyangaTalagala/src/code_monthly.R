## ---- load-pkgs
library(tidyverse)
library(forecast)
library(Mcomp)
library(forecTheta)
# devtools::install_github("thiyangt/seer")
library(seer)
# devtools::install_github("robjhyndman/tsfeatures")
library(tsfeatures)
library(foreach)

## ---- load-data
M1_monthly <- subset(M1, "monthly")
M3_monthly <- subset(M3, "monthly")
data(M4)
M4_monthly <- subset(M4, "monthly")

## ---- load-rmConstantSeries
M4_monthly_constant_train <- sapply(M4_monthly, function(temp){
	ts1 <- temp$x
	training <- head_ts(ts1, h=18)
	if (is.constant(training)==TRUE){print(temp$st)}
})
## for monthly data there is no series with constant values

## ---- extract series number

sn <- sapply(M4_monthly, function(temp){temp$st})
set.seed("27-4-2018")
index_test <- sample(sn, 1000)
M4_monthly_training <- M4_monthly[ !names(M4_monthly) %in% index_test]
length(M4_monthly_training) # 47000
save(M4_monthly_training, file="data/monthly/M4_monthly_training.rda")

M4_monthly_test <- M4_monthly[names(M4_monthly) %in% index_test]
length(M4_monthly_test) #1000
save(M4_monthly_test, file="data/monthly/M4_monthly_test.rda")

# ---- simulation
data(M4)
m4_monthly <- subset(M4, "monthly")
set.seed(8)
M4MAS <- lapply(m4_monthly, sim_etsbased, Future=TRUE, Nsim=5, extralength=18, Combine=FALSE)

M4MES <- lapply(m4_monthly, sim_arimabased, Future=TRUE, Nsim=5, extralength=18, Combine=FALSE)


# ---- classlabel
## M1-monthly
classlabelM1M <- fcast_accuracy(monthly_m1, 
                                models=c("ets", "arima", "rw", "rwd", "wn",
                                         "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                                database="Mcomp",
                                h=18, accuracyFun=cal_m4measures, length_out = 2)

## M3-monthly
classlabelM3M <- fcast_accuracy(monthly_m3, 
                                models=c("ets", "arima", "rw", "rwd", "wn",
                                         "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                                database="Mcomp",
                                h=18, accuracyFun=cal_m4measures, length_out = 2)

## M4-monthly-training
M4Mtraining_fcast_accuracy <- fcast_accuracy(M4_monthly_training, 
                        models=c("ets", "arima", "rw", "rwd", "wn",
                                 "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                        database="other",
                        h=18, accuracyFun=cal_m4measures, length_out = 2)


## M4-monthly_simulate based on ARIMA
M4MAS_fcast_accuracy <- lapply(M4MAS, fcast_accuracy, 
                 models=c("ets", "arima", "rw", "rwd", "wn",
                          "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                 database="other",
                 h=18, accuracyFun=cal_m4measures, length_out=2)

## M4-monthly_simulate based on ETS
m4_monthly_training <- load("M4/data/M4MES.rda") # length 48000
M4MES_fcast_accuracy  <- lapply(data, fcast_accuracy, 
                 models=c("ets", "arima", "rw", "rwd", "wn",
                          "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                 database="other",
                 h=18, accuracyFun=cal_m4measures, length_out=2)

# ---- features
## M1-monthly
featuresM1M  <- cal_features(M1_monthly, seasonal=TRUE, h=18, m=12, lagmax=13L, database="M1", highfreq = FALSE)

## M3-monthly
featuresM3M  <- cal_features(M3_monthly, seasonal=TRUE, h=18, m=12, lagmax=13L, database="M3", highfreq = FALSE)

## M4-monthly(training)
M4M_training <- lapply(M4_monthly_training, function(temp){temp$x})
featuresM4M_training <- cal_features(M4M_training, seasonal=TRUE, h=18, m=12, lagmax=13L, database="other", highfreq = FALSE)

# calculate features on simulated data - simulated based on ARIMA
featuresM4MSA  <- lapply(M4MAS, function(temp){
	lapply(temp, cal_features,seasonal=TRUE, h=18, m=12, lagmax=13L, database="other", highfreq=FALSE)})

# calculate features on simulated data - simulated based on ETS
featuresM4MSE  <- lapply(M4MES, function(temp){
	lapply(temp, cal_features,seasonal=TRUE, h=18, m=12, lagmax=13L, database="other", highfreq=FALSE)})


# processing class labels for monthly data

# M1 series
classlabelM1M_ms <- cal_medianscaled(classlabelM1M)
m1m_df <- prepare_trainingset(accuracy_set = classlabelM1M_ms, featuresM1M)
m1m_df_training <- m1m_df$training

# M3 series
classlabelM3M_ms <- cal_medianscaled(classlabelM3M)
m3m_df <- prepare_trainingset(accuracy_set = classlabelM3M_ms, featuresM3M)
m3m_df_training <- m3m_df$training

# M4-training set
M4Mtraining_fcast_accuaracy_ms <- cal_medianscaled(M4Mtraining_fcast_accuaracy)
features_training <- featuresM4M_training[1:44650, ]
M4M_trainingset <-  prepare_trainingset(accuracy_set=M4Mtraining_fcast_accuaracy_ms, 
																				feature_set=features_training)
M4M_training <- M4M_trainingset$trainingset

# M4 - simulate based on ARIMA
M4MAS_fcast_accuaracy_ms <- cal_medianscaled(M4MAS_fcast_accuaracy)
features_M4MSA <- lapply(featuresM4MSA, function(temp){
	do.call(rbind, temp)
})
features_M4MSA_DF <- do.call(rbind, features_M4MSA)
M4MAS_rfset_arima <-  prepare_trainingset(accuracy_set=M4MAS_fcast_accuaracy_ms, 
																		feature_set=features_M4MSA_DF)

M4MAS_rfset <- M4MAS_rfset_arima$trainingset

# training set 
monthly_training <- dplyr::bind_rows(m1m_df_training, m3m_df_training)
monthly_training <- dplyr::bind_rows(monthly_training,M4M_training)
monthly_training <- dplyr::bind_rows(monthly_training, M4MAS_rfset)
save(monthly_training, file="data/monthly_training.rda")


