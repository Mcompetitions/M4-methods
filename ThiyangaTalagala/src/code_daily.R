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
M4_daily <- subset(M4, "daily")

## --- convert the time series into suitable msts object
M4_daily_msts <- lapply(M4_daily, function(temp){
	temp$x <- convert_msts(temp$x, "daily")
	return(temp)
})

## ---- load-rmConstantSeries
M4_daily_constant_train <- sapply(M4_daily_msts, function(temp){
	ts1 <- temp$x
	training <- head_ts(ts1, h=14)
	if (is.constant(training)==TRUE){print(temp$st)}
})
# D2085

# split the M4 daily series into training and test
names_m4_use_d <- names(M4_daily_rm)
set.seed(8)
index_test_d <- sample(names_m4_use_d, 226)
save(index_test_d, file="data/daily/index_test_d.rda")
M4_training_daily <- M4_daily_rm[!names(M4_daily_rm) %in% index_test_d]
length(M4_training_daily) # 4000
save(M4_training_daily, file="data/daily/M4_training_daily.rda")
M4_test_daily <- M4_daily_rm[names(M4_daily_rm) %in% index_test_d]
length(M4_test_daily) #100
save(M4_test_daily, file="data/daily/M4_test_daily.rda")


# simulation 
set.seed(8) # 
M4Dmstlets <- lapply(M4_daily_msts, sim_mstlbased, Future=TRUE, Nsim=10, extralength=14, Combine=FALSE, mtd="ets")

set.seed(8)# 
M4Dmstlarima <- lapply(M4_daily_msts, sim_mstlbased, Future=TRUE, Nsim=10, extralength=14, Combine=FALSE, mtd="arima")


# convert to msts object
set.seed(8)
M4Dets_msts <- lapply(M4_daily_msts, sim_mstlbased, Future=TRUE, Nsim=5, extralength=14, Combine=FALSE, mtd="ets")
M4simets_daily_msts <- lapply(M4Dets_msts, function(temp){
	lapply(temp, convert_msts, category="daily")})


set.seed(8)
M4Darima_msts <- lapply(M4_daily_msts, sim_mstlbased, Future=TRUE, Nsim=5, extralength=14, Combine=FALSE, mtd="arima")
M4simarima_daily_msts <- lapply(M4Darima_msts, function(temp){
	lapply(temp, convert_msts, category="daily")})


# Calculate features: M4-training
M4D_train <- lapply(M4_training_daily, function(temp){temp$x})
featuresM4D_training <- cal_features(M4D_train, seasonal=TRUE, h=14, m=7, lagmax=8L, database="other", highfreq=TRUE)
# labels M4-training
data_train <- lapply(M4_training_daily, function(temp){temp$x})
m4daily_train <- fcast_accuracy(data_train, 
                        models=c("rw", "rwd", "wn",
                                 "theta", "stlar", "nn", "snaive", "mstlets","mstlarima", "tbats"),
                        database="other",
                        h=14, accuracyFun=cal_m4measures, length_out=2)
m0 <- m4daily_train$accuracy
ARIMA <- rep("arima", 4000)
ETS <- rep("ets", 4000)
acc_list <- list(accuracy=m0, ARIMA=ARIMA, ETS=ETS)
M4D_ms <- cal_medianscaled(acc_list)
M4D_training <- prepare_trainingset(accuracy_set=M4D_ms, 
																		feature_set=featuresM4D_training)
m4dtraining <- M4D_training$trainingset

# classlabel for simulated data
M4Dmstlets  <- lapply(M4simets_daily_msts, fcast_accuracy, 
                 models=c("rw", "rwd", "wn",
                          "theta", "stlar", "nn", "snaive", "mstlets","mstlarima", "tbats"),
                 database="other",
                 h=14, accuracyFun=cal_m4measures, length_out=2)
m4d_accuracy <- lapply(M4Dmstlets, function(temp){temp$accuracy})
m4d_mat <- do.call(rbind, m4d_accuracy)
accuracy <- m4d_mat
ARIMA <- rep("arima", 42270)
ETS <- rep("ets", 42270)
accsim_list_sim <- list(accuracy=accuracy, ARIMA=ARIMA, ETS=ETS)
M4D_msim <- cal_medianscaled(accsim_list_sim)

# features - simulated data
M4D_ets <- lapply(M4Dmstlets, function(temp){
  lapply(temp, function(temp){convert_msts(temp, "daily")})})
features_M4Dets  <-lapply(M4D_ets, function(temp){
  lapply(temp, cal_features,seasonal=TRUE, h=14, m=7, lagmax=8L, database="other", highfreq=TRUE)})
features_M4DS <- lapply(features_M4Dets, function(temp){
	do.call(rbind, temp)
})
featuresM4DS <- data.table::rbindlist(features_M4DS, use.names = TRUE, fill = TRUE)
featuresM4DS <- as.data.frame(featuresM4DS)
dim(featuresM4DS) # 42270 27
featuresM4DS$seasonal_strength1[is.na(featuresM4DS$seasonal_strength1)==TRUE] =
	featuresM4DS$seasonality[is.na(featuresM4DS$seasonality)==FALSE]
featuresM4DS$seasonal_strength2[is.na(featuresM4DS$seasonal_strength2)==TRUE]=0
dim(featuresM4DS)

featuresM4DS <- featuresM4DS %>% dplyr::select(-dplyr::one_of("seasonality"))
featuresM4DS <- featuresM4DS %>% dplyr::select(-dplyr::one_of("seas_pacf"))

M4Dsim_rf <- prepare_trainingset(accuracy_set=M4D_msim, 
																				feature_set=featuresM4DS)

# combine the data frames for daily series
daily_training <- dplyr::bind_rows(m4dtraining, M4D_training_sim)
daily_training <- daily_training %>% dplyr::select(-dplyr::one_of("seas_pacf"))
save(daily_training, file="data/daily_training.rda")

