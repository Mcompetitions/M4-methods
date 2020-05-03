# Note ###########################################################
# 
# The R package: seer is built to run the all codes. The seer 
# package is available at: https://github.com/thiyangt/seer
#
#
# Note: download the data folder from: https://github.com/thiyangt/M4Competition/tree/master/data
# Preparation of these training files ae computationally expensive hence file are uploaded 
# to the above mentioned git repository. Further, codes to construct this training data files are
# available at: src floder of the git repository at: https://github.com/thiyangt/M4Competition
#
# Before you start download the data folder from:
# https://github.com/thiyangt/M4Competition/tree/master/data
#
# To generate the files according to the format specify in the competition download the 
# template folder at: https://github.com/thiyangt/M4Competition
##################################################################

#---- load and install necessary packages
library(tidyverse)
library(forecast)
library(Mcomp)
library(forecTheta)
library(foreach)
library(readr)
devtools::install_github("robjhyndman/tsfeatures")
library(tsfeatures)
devtools::install_github("thiyangt/seer")
library(seer)

#---- load data
## load M4 competition data
data(M4)
yearly_m4 <- subset(M4, "yearly")
quarterly_m4 <- subset(M4, "quaterly")
monthly_m4 <- subset(M4, "monthly")
weekly_m4 <- subset(M4, "weekly")
daily_m4 <- subset(M4, "daily")
hourly_m4 <- subset(M4, "hourly")

# load training data files
y_train <- load(file="data/yearly_training.rda") 
q_train <- load(file="data/quarterly_training.rda")
m_train <- load(file="data/monthly_training.rda")
w_train <- load(file="data/weekly_training.rda")
d_train <- load(file="data/daily_training.rda")
h_train <- load(file="data/hourly_training.rda")

#---- generate foecasts for yearly series
features_M4Y<- seer::cal_features(yearly_m4, database="M4", highfreq = FALSE)
yearly_rcp <- seer::build_rf(yearly_training, features_M4Y, rf_type="rcp", ntree=1000, seed=1)
predictions_yearlym4_rcp <- yearly_rcp$predictions
m4yearly_forecast <- seer::rf_forecast(predictions_yearlym4_rcp,
                                 yearly_m4, "M4", "cal_MASE", h=6,
                                 accuracy = FALSE)

na_y <- matrix(NA, ncol=42, nrow=23000)
ymean <- cbind(m4yearly_forecast$mean, na_y)
ylower <- cbind(m4yearly_forecast$lower, na_y)
yupper <- cbind(m4yearly_forecast$upper, na_y)


#---- generate foecasts for quarterly series
features_M4Q<- seer::cal_features(quarterly_m4, seasonal=TRUE, m=4,lagmax=5L, 
                            database="M4", highfreq = FALSE)
quarterly_rcp <- seer::build_rf(quarterly_training, features_M4Q, rf_type="rcp", ntree=1000, seed=1)
predictions_quarterlym4_rcp <- quarterly_rcp$predictions
m4quarterly_forecast <- rf_forecast(predictions_quarterlym4_rcp,
                                    quarterly_m4, "M4", "cal_MASE", h=8,
                                    accuracy = FALSE)

na_q <- matrix(NA, ncol=40, nrow=24000)
qmean <- cbind(m4quarterly_forecast$mean, na_q)
qlower <- cbind(m4quarterly_forecast$lower, na_q)
qupper <- cbind(m4quarterly_forecast$upper, na_q)

#---- generate foecasts for monthly series
features_M4M<- seer::cal_features(monthly_m4, seasonal=TRUE, m=12,lagmax=13L, 
                            database="M4", highfreq = FALSE)
monthly_rcp <- seer::build_rf(monthly_training, features_M4M, rf_type="rcp", ntree=1000, seed=1)
predictions_monthlym4_rcp <- monthly_rcp$predictions
m4monthly_forecast <- seer::rf_forecast(predictions_monthlym4_rcp, monthly_m4, "M4", "cal_MASE", h=18,
                     accuracy = FALSE)

na_m <- matrix(NA, ncol=30, nrow=48000)
mmean <- cbind(m4monthly_forecast$mean, na_m)
mlower <- cbind(m4monthly_forecast$lower, na_m)
mupper <- cbind(m4monthly_forecast$upper, na_m)

#---- generate foecasts for weekly series
features_M4W <- seer::cal_features(weekly_m4, seasonal=TRUE, m=52,lagmax=53L, 
                             database="M4", highfreq = FALSE)
weekly_rcp <- seer::build_rf(weekly_training, features_M4W, rf_type="rcp", ntree=1000, seed=1)
predictions_weeklym4_rcp <- weekly_rcp$predictions
m4weekly_forecast <- seer::rf_forecast(predictions_weeklym4_rcp,
                                 weekly_m4, "M4", "cal_MASE", h=13,
                                 accuracy = FALSE)
na_w <- matrix(NA, ncol=35, nrow=359)
wmean <- cbind(m4weekly_forecast$mean, na_w)
wlower <- cbind(m4weekly_forecast$lower, na_w)
wupper <- cbind(m4weekly_forecast$upper, na_w)


#---- generate foecasts for daily series
## convert data into msts object
dailym4_msts <- lapply(daily_m4, function(temp){
  temp$x <- convert_msts(temp$x, "daily")
  return(temp)
})
features_M4D <- seer::cal_features(dailym4_msts, seasonal=TRUE, m=7,lagmax=8L, 
                             database="M4", highfreq = TRUE)
daily_rcp <- seer::build_rf(daily_training, features_M4D, rf_type="rcp", ntree=1000, seed=1)
predictions_dailym4_rcp <- daily_rcp$predictions
m4daily_forecast <- seer::rf_forecast(predictions_dailym4_rcp, dailym4_msts, "M4", "cal_MASE", h=14,
                     accuracy = FALSE)

na_d <- matrix(NA, ncol=34, nrow=4227)
dmean <- cbind(m4daily_forecast$mean, na_d)
dlower <- cbind(m4daily_forecast$lower, na_d)
dupper <- cbind(m4daily_forecast$upper, na_d)


#---- generate foecasts for hourly series
## convert data into msts object
hourlym4_msts <- lapply(hourly_m4, function(temp){
  temp$x <- convert_msts(temp$x, "hourly")
  return(temp)
})
features_M4H <- seer::cal_features(hourlym4_msts, seasonal=TRUE, m=24,lagmax=25L, 
                             database="M4", highfreq = TRUE)
hourly_rcp <- seer::build_rf(hourly_training, features_M4H, rf_type="rcp", ntree=1000, seed=1)
predictions_hourlym4_rcp <- hourly_rcp$predictions
m4hourly_forecast <- rf_forecast(predictions_hourlym4_rcp,
                                 hourlym4_msts, "M4", "cal_MASE", h=48,
                                 accuracy = FALSE)
hmean <- m4hourly_forecast$mean
hlower <- m4hourly_forecast$lower
hupper <- m4hourly_forecast$upper

#---- processing final csv files
mean_m4 <- rbind(ymean, qmean, mmean, wmean, dmean, hmean)
lower_m4 <- rbind(ylower, qlower, mlower, wlower, dlower, hlower)
upper_m4 <- rbind(yupper, qupper, mupper, wupper, dupper, hupper)

mean_m4[mean_m4 < 0] <- 0
lower_m4[lower_m4 < 0] <- 0
upper_m4[upper_m4 < 0] <- 0

#---- reading the column names and rownames from the template
# before running download the template folder at 
template <- read.csv(file = "template/template_Naive.csv")
col_name <- colnames(template)
id <- template$id
colnames(mean_m4) <- col_name[2:49]
colnames(lower_m4) <- col_name[2:49]
colnames(upper_m4) <- col_name[2:49]

mean_m4 <- data.frame(id , mean_m4)
write.csv(mean_m4, file="mean_m4.csv",
          row.names = FALSE)

lower_m4 <- data.frame(id , lower_m4)
write.csv(lower_m4, file="lower_m4.csv",
          row.names = FALSE)

upper_m4 <- data.frame(id , upper_m4)
write.csv(upper_m4, file="upper_m4.csv",
          row.names = FALSE)