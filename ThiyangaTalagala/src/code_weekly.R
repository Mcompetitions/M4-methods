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
data(M4)
M4_weekly <- subset(M4, "weekly")

## ---- load-rmConstantSeries
M4_weekly_constant_train <- sapply(M4_weekly, function(temp){
	ts1 <- temp$x
	training <- head_ts(ts1, h=13)
	if (is.constant(training)==TRUE){print(temp$st)}
})
## for weekly data there is no series with constant values

# split the M4 series into training and test
names_m4_use_w <- names(M4_weekly)
set.seed(8)
index_test_w <- sample(names_m4_use_w, 100)
save(index_test_w, file="data/weekly/index_test_w.rda")
M4_training_weekly <- M4_weekly[!names(M4_weekly) %in% index_test_w]
length(M4_training_weekly) # 259
save(M4_training_weekly, file="data/weekly/M4_training_weekly.rda")
M4_test_weekly <- M4_weekly[names(M4_weekly) %in% index_test_w]
length(M4_test_weekly) #100
save(M4_test_weekly, file="data/weekly/M4_test_weekly.rda")


# ---- simulation
M4Wmstl <- lapply(M4_hourly, sim_mstlbased, Future=TRUE, Nsim=10, extralength=13, Combine=FALSE)

# ---- classlabel
## M4-weekly
data_train_m4W <- lapply(M4_training_weekly, function(temp){temp$x})
classlabelM4W <- fcast_accuracy(data_train_m4W, 
																models=c("arima", "rw", "rwd", "wn",
																				 "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
																database="other",
																h=13, accuracyFun=cal_m4measures)

## M4-weekly_simulate based on mstl
naomit_list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
M4Wmstl_nafree <- naomit_list(M4Wmstl) 
classlabelM4WSmstl  <- lapply(M4Wmstl_nafree, fcast_accuracy, 
															models=c("ets", "arima", "rw", "rwd", "wn",
																			 "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
															database="other",
															h=13, accuracyFun=cal_m4measures,length_out = 2)

# ---- features
M4W_train <- lapply(M4_training_weekly, function(temp){temp$x})
featuresM4W_training <- cal_features(M4W_train, seasonal=TRUE, h=13, m=52, lagmax=53L, database="other", highfreq=FALSE)

## M4-weekly_simulate based on mstlETS
featuresM4Wmstl  <- lapply(M4Wmstl_nafree, function(temp){
	lapply(temp, cal_features,seasonal=TRUE, h=13, m=52, lagmax=53L, database="other", highfreq=FALSE)})
features_M4WS <- lapply(featuresM4Wmstl, function(temp){
	do.call(rbind, temp)
})
features_M4WS_DF <- do.call(rbind, features_M4WS)
features_M4WS_DF <- within(features_M4WS_DF, rm(hwalpha, hwbeta, hwgamma))

# Random forest preparation
# M4 data
classlabelM4W_ms <- cal_medianscaled(classlabelM4W)
classlabelM4W_ms$ETS <- rep("ets", 259)
featuresM4W_training <-  within(featuresM4W_training, rm(hwalpha, hwbeta, hwgamma,  sediff_seacf1))
M4W_rfset <-  prepare_trainingset(accuracy_set=classlabelM4W_ms, 
																		feature_set=featuresM4W_training)

# simulated data
acc <- lapply(classlabelM4WSmstl, function(temp){temp$accuracy})
acc_mat <- do.call(rbind, acc)
w_arima <- lapply(classlabelM4WSmstl, function(temp){temp$ARIMA})
arima_mat <- do.call(rbind, w_arima)
w_ets <- lapply(classlabelM4WSmstl, function(temp){temp$ETS})
ets_mat <- do.call(rbind, w_ets)
acc_info <- list(accuracy=acc_mat, ARIMA=arima_mat, ETS=ets_mat)
classlabelM4WSmstl_ms <- cal_medianscaled(acc_info)
classlabelM4WSmstl_ms$ETS <- rep("ets", 2940)
M4WS_rfset <- prepare_trainingset(accuracy_set=classlabelM4WSmstl_ms, 
										feature_set=features_M4WS_DF)

weekly_training <- dplyr::bind_rows(M4W_rfset$trainingset, M4WS_rfset$trainingset)
dim(weekly_training)

