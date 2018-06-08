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
M1_yearly <- subset(M1, "Yearly")
M3_yearly <- subset(M3, "Yearly")
data(M4)
M4_yearly <- subset(M4, "Yearly")

## ---- load-rmConstantSeries
M4_yearly_constant_train <- sapply(M4_yearly, function(temp){
	ts1 <- temp$x
	training <- head_ts(ts1, h=6)
	if (is.constant(training)==TRUE){print(temp$st)}
})

# Y12146
# Y21168

M4_yearly_rm <- M4_yearly[-c(12146, 21168)]
length(M4_yearly_rm) # 22998

# split the M4 series into training and test
names_m4_use_y <- names(M4_yearly_rm)
set.seed(8)
index_test_y <- sample(names_m4_use_y, 1000)
M4_training_yearly <- M4_yearly_rm[!names(M4_yearly_rm) %in% index_test_y]
M4_test_yearly <- M4_yearly_rm[names(M4_yearly_rm) %in% index_test_y]


# simulate- arima based
set.seed(8)
M4YSimARIMA <- lapply(M4_yearly, sim_arimabased, Future=TRUE, Nsim=10, extralength=6, Combine=FALSE)
length(M4YSimARIMA) # 23000

# simulate- ets based
set.seed(8)
M4YSimETS <- lapply(M4_yearly, sim_etsbased, Future=TRUE, Nsim=10, extralength=6, Combine=FALSE)
length(M4YSimETS)

## ---- load-calculate forecast accuracy measures 
classlabelM1Y <- fcast_accuracy(M1_yearly, 
																models=c("ets","arima", "rw", "rwd", "wn", "theta", "nn"),
																database="Mcomp",
																h=6, accuracyFun=cal_m4measures, length_out = 2)

## ---- load-classlabelM3Y
classlabelM3Y <- fcast_accuracy(M3_yearly, 
																models=c("ets","arima", "rw", "rwd", "wn", "theta", "nn"),
																database="Mcomp",
																h=6, accuracyFun=cal_m4measures, length_out=2)

## ---- load - classlabelM4Y-training
data_train_m4y <- lapply(M4_training_yearly, function(temp){temp$x})
classlabelM4Y_train <- fcast_accuracy(data_train_m4y, 
																models=c("ets","arima", "rw", "rwd", "wn", "theta", "nn"),
																database="other",
																h=6, accuracyFun=cal_m4measures, length_out=2)


## ---- load - simulatebased on ARIMA (run on Monash Computer Cluster)
classlabelM4YSA <- lapply(M4YSimARIMA, fcast_accuracy, 
								 models=c("ets","arima", "rw", "rwd", "wn", "theta", "nn"),
								 database="other",
								 h=6, accuracyFun=cal_m4measures)

## ---- load - simulatebased on ETS (run on Monash Computer Cluster)
classlabelM4YSE <- lapply(M4YSimETS, fcast_accuracy, 
													models=c("ets","arima", "rw", "rwd", "wn", "theta", "nn"),
													database="other",
													h=6, accuracyFun=cal_m4measures)


## ---- calculate features
# M1Y
featuresM1Y  <- cal_features(M1_yearly,h=6,database="M1", highfreq = FALSE)

# M3Y
featuresM3Y  <- cal_features(M3_yearly, h=6,database="M3", highfreq = FALSE)

# M4Y - training
M4Y_train <- lapply(M4_training_yearly, function(temp){temp$x})
featuresM4Y_training <- cal_features(M4Y_train, h=6, database="other", highfreq = FALSE)

# M4Y - simulate based on ETS
featuresM4YSE  <- lapply(M4YSimETS, function(temp){
	lapply(temp, cal_features, h=6, database="other", highfreq=FALSE)})

# M4Y - simulate based on ARIMA
featuresM4YSA  <- lapply(M4YSimARIMA, function(temp){
	lapply(temp, cal_features, h=6, database="other", highfreq=FALSE)})


# prepare data for random forest training
## M1
classlabel_m1y <- cal_meadianscaled(classlabelM1Y)
m1_df <- prepare_trainingset(classlabel_m1y, featuresM1Y)
m1_df_training <- m1_df$trainingset

## M3
classlabel_m3y <- cal_meadianscaled(classlabelM3Y)
m3_df <- prepare_trainingset(classlabel_m3y, M3yearly_features)
m3_df_training <- m3_df$trainingset


## M4 - training
classlabel_m4y <- cal_meadianscaled(classlabelM4Y_train)
m4_df <- prepare_trainingset(classlabel_m4y, featuresM4Y_training)
m4_df_training <- m4_df$trainingset

## m4 simulation-arima based
features_M4YSA <- lapply(featuresM4YSA, function(temp){
	do.call(rbind, temp)
})
features_M4YSA_DF <- do.call(rbind, features_M4YSA)
accuracym4ysa <- lapply(classlabelM4YSA, function(temp){
	ACCURACY <- temp$accuracy
})
accuracym4ysa_mat <- do.call(rbind, accuracym4ysa)
arimam4ysa <- lapply(classlabelM4YSA, function(temp){
	temp$ARIMA
})
arimam4ysa <- unlist(arimam4ysa)
etsm4ysa <- lapply(classlabelM4YSA, function(temp){
	temp$ETS
})
etsm4ysa <- unlist(etsm4ysa)
accuracy_m4ysa <- list(accuracy=accuracym4ysa_mat, ARIMA=arimam4ysa,
											 ETS=etsm4ysa)
classlabel_m4ysa <- cal_meadianscaled(accuracy_m4ysa)
m4ysa_df <- prepare_trainingset(classlabel_m4ysa, features_M4YSA_DF)
m4ysa_df_training <- m4ysa_df$trainingset

## m4-ets based
# the places with non-matrix
M4_yearly_ets <- sapply(classlabelM4YSE, function(temp){
	if (class(temp$accuracy)!="matrix"){print(parent.frame()$i[])}
})
# Y21312
# remove Y21312 from features and classlabels
featuresM4YSE2 <- featuresM4YSE[-21312]
length(featuresM4YSE) # 23000
length(featuresM4YSE2) #22999
classlabelM4YSE2 <- classlabelM4YSE[-21312]
length(classlabelM4YSE2) # 22999
length(classlabelM4YSE) # 23000

# print matrix with NA
features_M4YSE <- lapply(featuresM4YSE2, function(temp){
	do.call(rbind, temp)
})
features_M4YSE_DF <- do.call(rbind, features_M4YSE)
accuracym4yse <- lapply(classlabelM4YSE2, function(temp){
	ACCURACY <- temp$accuracy
})
accuracym4yse_mat <- do.call(rbind, accuracym4yse)
arimam4yse <- lapply(classlabelM4YSE2, function(temp){
	temp$ARIMA
})
arimam4yse <- unlist(arimam4yse)
etsm4yse <- lapply(classlabelM4YSE2, function(temp){
	temp$ETS
})
etsm4yse <- unlist(etsm4yse)
accuracy_m4yse <- list(accuracy=accuracym4yse_mat, ARIMA=arimam4yse,
											 ETS=etsm4yse)
classlabel_m4yse <- cal_meadianscaled(accuracy_m4yse)
m4yse_df <- prepare_trainingset(classlabel_m4yse, features_M4YSE_DF)
m4yse_df_training <- m4yse_df$trainingset

# Combine dataframes---------------------------------
yearly_training <- dplyr::bind_rows(m1_df_training, m3_df_training)
yearly_training <- dplyr::bind_rows(yearly_training,m4_df_training)
yearly_training <- dplyr::bind_rows(yearly_training, m4ysa_df_training)
yearly_training <- dplyr::bind_rows(yearly_training, m4yse_df_training)
dim(yearly_training) #482813    26
table(yearly_training$classlabels)
save(yearly_training, file="data/yearly_training.rda")
