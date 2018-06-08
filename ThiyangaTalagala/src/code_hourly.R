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
M4_hourly <- subset(M4, "hourly")

## --- convert the time series into suitable msts object
M4_hourly_msts <- lapply(M4_hourly, function(temp){
	temp$x <- convert_msts(temp$x, "hourly")
	return(temp)
})

## ---- load-rmConstantSeries
M4_hourly_constant_train <- sapply(M4_hourly_msts, function(temp){
	ts1 <- temp$x
	training <- head_ts(ts1, h=48)
	if (is.constant(training)==TRUE){print(temp$st)}
})
# No hourly series with constant values

# split the M4 hourly series into training and test
names_m4_use_h <- names(M4_hourly_msts)
set.seed(8)
index_test_h <- sample(names_m4_use_h, 64)
M4_training_hourly <- M4_hourly_msts[!names(M4_hourly_msts) %in% index_test_h]
M4_test_hourly <- M4_hourly_msts[names(M4_hourly_msts) %in% index_test_h]


# simulation 
set.seed(8)
M4Hmstlets <- lapply(M4_hourly_msts, sim_mstlbased, Future=TRUE, Nsim=10, extralength=48, Combine=FALSE, mtd="ets")
set.seed(8)
M4Hmstlarima <- lapply(M4_hourly_msts, sim_mstlbased, Future=TRUE, Nsim=10, extralength=48, Combine=FALSE, mtd="arima")

# convert simulate ts to msts
M4Hmstlarima_msts <- lapply(M4Hmstlarima, function(temp){
	lapply(temp, function(temp){convert_msts(temp, "hourly")})})

# features
M4H_train <- lapply(M4_training_hourly, function(temp){temp$x})
featuresM4H_training <- cal_features(M4H_train, seasonal=TRUE, h=48, m=24, lagmax=25L, database="other", highfreq=TRUE)

features_m4hmstlets <- lapply(M4Hmstlarima_msts, function(temp){
	lapply(temp, cal_features,seasonal=TRUE, h=48, m=24, lagmax=25L, database="other", highfreq=TRUE)})
features_M4H_mstl <- lapply(features_m4hmstlets, function(temp){
	do.call(rbind, temp)
})
features_M4Hmstl_DF <- do.call(rbind, features_M4H_mstl)

# Class label
data_train <- lapply(M4_training_hourly, function(temp){temp$x})
M4Htraining_label <- fcast_accuracy(data_train, 
                        models=c("rw", "rwd", "wn", "stlar", "nn", "snaive", "mstlets","mstlarima", "tbats"),
                        database="other",
                        h=48, accuracyFun=cal_m4measures, length_out=2)
m0 <- M4Htraining_label$accuracy
ARIMA <- rep("arima", 350)
ETS <- rep("ets", 350)
acc_list <- list(accuracy=m0, ARIMA=ARIMA, ETS=ETS)
M4H_ms <- cal_medianscaled(acc_list)
M4H_training <- prepare_trainingset(accuracy_set=M4H_ms, 
																		feature_set=featuresM4H_training)

# simulated series
m4hets  <- lapply(M4Hmstlets, fcast_accuracy, 
                 models=c("rw", "rwd", "wn",
                          "theta", "stlar", "nn", "snaive", "mstlets","mstlarima", "tbats"),
                 database="other",
                 h=48, accuracyFun=cal_m4measures, length_out=2)
accuracy <- m4hets$accuracy
ARIMA <- rep("arima", 4140)
ETS <- rep("ets", 4140)
accsim_list <- list(accuracy=accuracy, ARIMA=ARIMA, ETS=ETS)
M4H_msim <- cal_medianscaled(accsim_list)
M4H_training_sim <- prepare_trainingset(accuracy_set=M4H_msim, 
																		feature_set=features_M4Hmstl_DF)


# combine dataframes
hourly_training <- dplyr::bind_rows(M4H_training$trainingset, M4H_training_sim$trainingset)
save(hourly_training, file="data/hourly_training.rda")

