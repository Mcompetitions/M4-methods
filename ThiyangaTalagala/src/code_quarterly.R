## ---- load-pkgs
library(tidyverse)
library(Mcomp)
library(forecast)
library(forecTheta)
# devtools::install_github("thiyangt/seer")
library(seer)
# devtools::install_github("robjhyndman/tsfeatures")
library(tsfeatures)

## ---- load-data
M1_quarterly <- subset(M1, "Quarterly")
M3_quarterly <- subset(M3, "Quarterly")
data(M4)
M4_quarterly <- subset(M4, "Quarterly")

# ---- summary of length of series
M4_length <- sapply(M4_quarterly, function(temp){length(temp$x)})
table(M4_length) # min-16 and max - 866

length_16 <- as.vector(which(M4_length==16))

## ---- load-rmConstantSeries
M4_quarterly_constant_train <- sapply(M4_quarterly, function(temp){
	ts1 <- temp$x
	training <- head_ts(ts1, h=8)
	if (is.constant(training)==TRUE){print(temp$st)}
})

# constant series: Q5619
# plot(M4[["Q5619"]]$x)
M4_quarterly_rm <- M4_quarterly[-c(5619, length_16)]
length(M4_quarterly_rm) # 23998

# split the M4 series into training and test
names_m4_use_q <- names(M4_quarterly_rm)
set.seed(8)
index_test_q <- sample(names_m4_use_q, 1000)
M4_training_quarterly <- M4_quarterly_rm[!names(M4_quarterly_rm) %in% index_test_q]
M4_test_quarterly <- M4_quarterly_rm[names(M4_quarterly_rm) %in% index_test_q]

# Simulate based on ARIMA
set.seed(8)
M4QAS <- lapply(data, sim_arimabased, Future=TRUE, Nsim=5, extralength=8, Combine=FALSE)

# Simulate based on ETS
set.seed(8)
M4QES <- lapply(data, sim_etsbased, Future=TRUE, Nsim=5, extralength=8, Combine=FALSE)


## ---- load-classlabelM1Q
classlabelM1Q <- fcast_accuracy(quarterly_m1, 
                                models=c("ets", "arima", "rw", "rwd", "wn",
                                         "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                                database="Mcomp",
                                h=8, accuracyFun=cal_m4measures, length_out=2)

## ---- load-classlabelM3Q
classlabelM3Q <- fcast_accuracy(quarterly_m3, 
                                models=c("ets", "arima", "rw", "rwd", "wn",
                                         "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                                database="Mcomp",
                                h=8, accuracyFun=cal_m4measures, length_out=2)

## --- class label M4Q(training)
data_train <- lapply(M4_training_quarterly, function(temp){temp$x})
M4Qtraininglab <- fcast_accuracy(data_train, 
                        models=c("ets", "arima", "rw", "rwd", "wn",
                                 "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                        database="other",
                        h=8, accuracyFun=cal_m4measures)

## ---- load-classlabelM4QAS
M4Qtraining_fcast_accuaracy  <- lapply(M4QAS, fcast_accuracy, 
                 models=c("ets", "arima", "rw", "rwd", "wn",
                          "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                 database="other",
                 h=8, accuracyFun=cal_m4measures, length_out=2)

## ---- load-classlabelM4QES
M4QES_fcast_accuaracy  <- lapply(M4QES, fcast_accuracy, 
                 models=c("ets", "arima", "rw", "rwd", "wn",
                          "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
                 database="other",
                 h=8, accuracyFun=cal_m4measures, length_out=2)

## ---- load-classlabelM4Q(test)
data_test <- lapply(M4_test_quarterly, function(temp){temp$x})
M4Q_test_label <- fcast_accuracy(data_test, 
							 models=c("ets", "arima", "rw", "rwd", "wn",
							 				 "theta", "stlar", "nn", "snaive", "mstl", "tbats"),
							 database="other",
							 h=8, accuracyFun=cal_m4measures, length_out=2)

## ---- calculate features
# M1Q
featuresM1Q  <- cal_features(M1_quarterly, seasonal=TRUE, h=8, m=4, lagmax=5L, database="M1")

# M3Q
featuresM3Q  <- cal_features(M3_quarterly, seasonal=TRUE, h=8, m=4, lagmax=5L, database="M1")

# M4Q - training
M4Q_train <- lapply(M4_training_quarterly, function(temp){temp$x})
featuresM4Q_taining <- cal_features(M4Q_train, seasonal=TRUE, h=8, m=4, lagmax=5L, database="other")

# M4Q - test
M4Q_test <- lapply(M4_test_quarterly, function(temp){temp$x})
featuresM4Q_test <- cal_features(M4Q_test, seasonal=TRUE, h=8, m=4, lagmax=5L, database="other")

# M4Q - simulate based on ETS-features
featuresM4QSE  <- lapply(M4QES, function(temp){
	lapply(temp, cal_features,seasonal=TRUE, h=8, m=4, lagmax=5L, database="other")})

# M4Q - simulate based on ARIMA-featues
featuresM4QSA  <- lapply(M4QAS, function(temp){
	lapply(temp, cal_features,seasonal=TRUE, h=8, m=4, lagmax=5L, database="other")})


# Random forest data preparation
## based on m1
classlabelM1Q_ms <- cal_medianscaled(classlabelM1Q)
m1q_df <- prepare_trainingset(accuracy_set = classlabelM1Q_ms, featuresM1Q)
m1q_df_training <- m1q_df$training

## based on m3
classlabelM3Q_ms <- cal_medianscaled(classlabelM3Q)
m3q_df <- prepare_trainingset(classlabelM3Q_ms, featuresM3Q)
m3q_df_training <- m3q_df$training

# M4 training
M4Qtraining_fcast_accuaracy_ms <- cal_medianscaled(M4Qtraining_fcast_accuaracy)
M4Q_trainingset <-  prepare_trainingset(accuracy_set=M4Qtraining_fcast_accuaracy_ms, 
                                        feature_set=featuresM4Q_taining)
M4Q_training <- M4Q_trainingset$trainingset


# M4- arima based
M4QAS_fcast_accuaracy_ms <- cal_medianscaled(M4QAS_fcast_accuaracy)
features_M4QSA <- lapply(featuresM4QSA, function(temp){
	do.call(rbind, temp)
})
features_M4QSA_DF <- do.call(rbind, features_M4QSA)
M4QAS_rfset <-  prepare_trainingset(accuracy_set=M4QAS_fcast_accuaracy_ms, 
																				feature_set=features_M4QSA_DF)
M4QAS_training <- M4QAS_rfset$trainingset


# M4 ets based
M4QES_fcast_accuaracy_ms <- cal_medianscaled(M4QES_fcast_accuaracy)
features_M4QSE <- lapply(featuresM4QSE, function(temp){
	do.call(rbind, temp)
})
features_M4QSE_DF <- do.call(rbind, features_M4QSE)
M4QES_rfset <-  prepare_trainingset(accuracy_set=M4QES_fcast_accuaracy_ms, 
																		feature_set=features_M4QSE_DF)
M4QES_training <- M4QES_rfset$trainingset

# Combine dataframes---------------------------------
quarterly_training <- dplyr::bind_rows(m1q_df_training, m3q_df_training)
quarterly_training <- dplyr::bind_rows(quarterly_training,M4Q_training)
quarterly_training <- dplyr::bind_rows(quarterly_training, M4QAS_training)
quarterly_training <- dplyr::bind_rows(quarterly_training, M4QES_training)
dim(quarterly_training) #263957  31 (120000*2+75+22998+203)
table(quarterly_training$classlabels)
save(quarterly_training, file="data/quarterly_training.rda")

