require(dplyr)
require(forecast)
require(tidyr)
require(svMisc)
# Run this code to load the dataset. A prompt will appear. 
#Select the data file to load in R environment.
#Dataset of only one type of frequency can be used at a given time.
Data_set_m4 <- read.csv(file.choose(),stringsAsFactors = F) Data_set_m4 <- remove_empty(Data_set_m4, "cols")

#Converting row wise time series to column wise in order to make it a tidy data set
Data_set_m4.long <- as.data.frame(t(Data_set_m4[,-1]))     
remove(Data_set_m4)

#SOME NOTES BEFORE BEGGINING THE FORECASTING CODE
# Use the code from line no 26 to 97 for yearly, quarterly, hourly and monthly frequency.
# Use the code from line no 100 to 154 for daily and weekly frequency data (upto series no 295 for weekly)
# Use the code after that for weekly data from series no 296 onwards
# The loop breaks down sometimes as some methods fail for some time series
#(specifically for quarterly variables no 9454 and 21168.)
#Note the value of i where the loop breaks down.
#Just run the code manually and then replace the intial value of i in the loop with next value (e.g. 9455) and rerun the loop


#Some user Inputs required for later use.

#Frequency of the data set
data_frequency <- 5 # Use 4 for quarterly | 1 for yearly | 12 for monthly| 24 for hourly | 365.25/7 for weekly | 5 for daily financial and 7 for others

#Fraction of dataset to split into training and test sets
frac_data_train <- .9 # .8 for quarterly, yearly & monthly | .9 for hourly, weekly and daily
#No. of final required forecasts 
no_req_forecasts <- 14 # As per M4 rules according to data frequency
# A parameter for seasonal frequency of dataset
seasonal_freq <- c(5) # 4 for quarterly | 1 for yearly | 24 & 168 (c(24,168)) for hourly |365.25/7 for weekly | 5 for daily financial and 7 for others

# Empty list to store combined forecasts
combined_forecasts <- list()
#Loop to run the entire forecasting procedure across all columns on column at a time
for(i in 1:(ncol(Data_set_m4.long)))
{

progress(i,max.value = ncol(Data_set_m4.long)) # Progress Bar for loop
    
#Convert column i to a seasonal time series object
V1ts <- msts(Data_set_m4.long[,i][!is.na(Data_set_m4.long[,i])], seasonal.periods = seasonal_freq,ts.frequency = data_frequency)

#Split the time series in column i into training and test columns
trainingdim <- 1:(floor((frac_data_train*length(V1ts))))
V1train <- ts(V1ts[trainingdim],frequency = data_frequency)
V1test <- V1ts[-trainingdim]
no_forecasts <- length(V1test)

# The list named accuracy_stats stores the out-of-sample accuracy statistics for forecasts generated 
# using training set and compared with the test set
accuracy_stats <- list()
accuracy_stats$ets <- accuracy(forecast(stlm(V1train,s.window = "periodic",method = "ets"),no_forecasts),V1test)
accuracy_stats$arima <- accuracy(forecast(stlm(V1train,s.window = "periodic",method = "arima"),no_forecasts),V1test)
accuracy_stats$neural <- accuracy(forecast(nnetar(V1train),no_forecasts),V1test)
accuracy_stats$seasnaive <- accuracy(snaive(V1train,no_forecasts),V1test)
accuracy_stats$theta <- accuracy(thetaf(V1train,no_forecasts),V1test)
accuracy_stats$naive <- accuracy(naive(V1train,no_forecasts),V1test)
accuracy_stats$randomwalk <- accuracy(rwf(V1train,drift = T,no_forecasts),V1test)

# Retrieve MAPE of all seven methods used to forecast
mape_all_methods <- sapply(accuracy_stats, function(x){x[10]})

# Select top performing forecasting methods according to MAPE (> median)
# This leads to a minimum of one and a maximum of 3 selected methods
mape_selected <- if(length(names(which(mape_all_methods < median(mape_all_methods)))) != 0)
    { names(which(mape_all_methods < median(mape_all_methods)))} else {names(mape_all_methods) }

# Assign weights to each method according to their MAPE in the previous step.
# The higher the MAPE, the lower the weight. See document for the formula used to calculate weights.
acc_stats_select <- sapply(accuracy_stats[mape_selected], function(x){x[10]}) # extract MAPE of selected methods

acc_stats_weight <- prop.table(sapply(acc_stats_select,function(x){sum(acc_stats_select)-x})^3) # Calculate weights
acc_stats_weight <- ifelse(acc_stats_weight %in% NaN,1,acc_stats_weight) # If only one method is selected, the previous code returns NaN. This code converts the NaN to 1.

# Retrain the models using full data set.
final_forecast <- list()
final_forecast$ets <- forecast(stlm(V1ts,s.window = "periodic",method = "ets"),no_req_forecasts)$mean
final_forecast$arima <- forecast(stlm(V1ts,s.window = "periodic",method = "arima"),no_req_forecasts)$mean
final_forecast$neural <- forecast(nnetar(V1ts),no_req_forecasts)$mean
final_forecast$seasnaive <- snaive(V1ts,no_req_forecasts)$mean
final_forecast$theta <- thetaf(V1ts,no_req_forecasts)$mean
final_forecast$naive <- naive(V1ts,no_req_forecasts)$mean
final_forecast$randomwalk <- rwf(V1ts,drift = T,no_req_forecasts)$mean

# Extract the forecasts of selected methods
forecasts_selected <- sapply(final_forecast[mape_selected],function(x){x[1:no_req_forecasts]})
# Calculate a weighted average of forecasts from the selected methods. This is the step
# where we obtain the final forecasts. The loop Repeats this procedure for all i columns.
combined_forecasts[[i]] <- rowSums(sweep(forecasts_selected,2,acc_stats_weight,FUN = "*"))
}
#Combine all forecasts row wise (M4 format) and export them to csv. Run the code again for different frequencies.
names(combined_forecasts) <- lapply(1:length(combined_forecasts),function(x){ paste("W",x,sep="")})
write.csv(do.call(rbind,combined_forecasts),file="weekly.csv")


# CODE FOR DAILY AND WEEKLY DATA

#Frequency of the data set
data_frequency <- 5 # Use 4 for quarterly | 1 for yearly | 12 for monthly| 24 for hourly | 365.25/7 for weekly | 5 for daily financial and 7 for others

#Fraction of dataset to split into training and test sets
frac_data_train <- .9 # .8 for quarterly, yearly & monthly | .9 for hourly, weekly and daily
#No. of final required forecasts 
no_req_forecasts <- 14 # As per M4 rules according to data frequency
# A parameter for seasonal frequency of dataset
seasonal_freq <- c(5) # 4 for quarterly | 1 for yearly | 24 & 168 (c(24,168)) for hourly |365.25/7 for weekly | 5 for daily financial and 7 for others

# Empty list to store combined forecasts
combined_forecasts <- list()
#Loop to run the entire forecasting procedure across all columns on column at a time
for(i in 1:(ncol(Data_set_m4.long)))
{
    progress(i,max.value = ncol(Data_set_m4.long))
    V1ts <- msts(Data_set_m4.long[,i][!is.na(Data_set_m4.long[,i])], seasonal.periods = seasonal_freq,ts.frequency = data_frequency)
    
    trainingdim <- 1:(floor((frac_data_train*length(V1ts))))
    V1train <- ts(V1ts[trainingdim],frequency = data_frequency)
    V1test <- V1ts[-trainingdim]
    no_forecasts <- length(V1test)
    accuracy_stats <- list()
    accuracy_stats$ets <- accuracy(stlf(V1train,h = no_forecasts,s.window = data_frequency,method = "ets"),V1test)
    accuracy_stats$arima <- accuracy(stlf(V1train,h = no_forecasts,s.window = data_frequency,method = "arima"),V1test)
    accuracy_stats$neural <- accuracy(forecast(nnetar(V1train),no_forecasts),V1test)
    accuracy_stats$seasnaive <- accuracy(snaive(V1train,no_forecasts),V1test)
    accuracy_stats$naive <- accuracy(naive(V1train,no_forecasts),V1test)
    accuracy_stats$randomwalk <- accuracy(rwf(V1train,drift = T,no_forecasts),V1test)
    
    mape_all_methods <- sapply(accuracy_stats, function(x){x[10]})
    
    mape_selected <- if(length(names(which(mape_all_methods < median(mape_all_methods)))) != 0)
    { names(which(mape_all_methods < median(mape_all_methods)))} else {names(mape_all_methods) }
    
    acc_stats_select <- sapply(accuracy_stats[mape_selected], function(x){x[10]})
    acc_stats_weight <- prop.table(sapply(acc_stats_select,function(x){sum(acc_stats_select)-x})^3)
    acc_stats_weight <- ifelse(acc_stats_weight %in% NaN,1,acc_stats_weight)
    final_forecast <- list()
    final_forecast$ets <- stlf(V1ts,h = no_req_forecasts,s.window = data_frequency,method = "ets")$mean
    final_forecast$arima <- stlf(V1ts,h = no_req_forecasts,s.window = data_frequency,method = "arima")$mean
    final_forecast$neural <- forecast(nnetar(V1ts),no_req_forecasts)$mean
    final_forecast$seasnaive <- snaive(V1ts,no_req_forecasts)$mean
    final_forecast$naive <- naive(V1ts,no_req_forecasts)$mean
    final_forecast$randomwalk <- rwf(V1ts,drift = T,no_req_forecasts)$mean
    
    forecasts_selected <- sapply(final_forecast[mape_selected],function(x){x[1:no_req_forecasts]})
    
    combined_forecasts[[i]] <- rowSums(sweep(forecasts_selected,2,acc_stats_weight,FUN = "*"))
}
#Combine all forecasts row wise (M4 format) and export them to csv. Run the code again for different frequencies.
names(combined_forecasts) <- lapply(1:length(combined_forecasts),function(x){ paste("W",x,sep="")})
write.csv(do.call(rbind,combined_forecasts),file="weekly.csv")


## CODE FOR WEEKLY DATA Variable no 296 onwards because the previous code only works till variable 295

#Frequency of the data set
data_frequency <- 5 # Use 4 for quarterly | 1 for yearly | 12 for monthly| 24 for hourly | 365.25/7 for weekly | 5 for daily financial and 7 for others

#Fraction of dataset to split into training and test sets
frac_data_train <- .9 # .8 for quarterly, yearly & monthly | .9 for hourly, weekly and daily
#No. of final required forecasts 
no_req_forecasts <- 14 # As per M4 rules according to data frequency
# A parameter for seasonal frequency of dataset
seasonal_freq <- c(5) # 4 for quarterly | 1 for yearly | 24 & 168 (c(24,168)) for hourly |365.25/7 for weekly | 5 for daily financial and 7 for others

# Empty list to store combined forecasts
combined_forecasts <- list()
#Loop to run the entire forecasting procedure across all columns on column at a time
for(i in 1:(ncol(Data_set_m4.long)))
{
    progress(i,max.value = ncol(Data_set_m4.long))
    V1ts <- msts(Data_set_m4.long[,i][!is.na(Data_set_m4.long[,i])], seasonal.periods = seasonal_freq,ts.frequency = data_frequency)
    
    trainingdim <- 1:(floor((frac_data_train*length(V1ts))))
    V1train <- ts(V1ts[trainingdim],frequency = data_frequency)
    V1test <- V1ts[-trainingdim]
    no_forecasts <- length(V1test)
    accuracy_stats <- list()
    accuracy_stats$neural <- accuracy(forecast(nnetar(V1train),no_forecasts),V1test)
    accuracy_stats$seasnaive <- accuracy(snaive(V1train,no_forecasts),V1test)
    accuracy_stats$naive <- accuracy(naive(V1train,no_forecasts),V1test)
    accuracy_stats$randomwalk <- accuracy(rwf(V1train,drift = T,no_forecasts),V1test)
    
    mape_all_methods <- sapply(accuracy_stats, function(x){x[10]})
    
    mape_selected <- if(length(names(which(mape_all_methods < median(mape_all_methods)))) != 0)
    { names(which(mape_all_methods < median(mape_all_methods)))} else {names(mape_all_methods) }
    
    acc_stats_select <- sapply(accuracy_stats[mape_selected], function(x){x[10]})
    acc_stats_weight <- prop.table(sapply(acc_stats_select,function(x){sum(acc_stats_select)-x})^3)
    acc_stats_weight <- ifelse(acc_stats_weight %in% NaN,1,acc_stats_weight)
    final_forecast <- list()
    final_forecast$neural <- forecast(nnetar(V1ts),no_req_forecasts)$mean
    final_forecast$seasnaive <- snaive(V1ts,no_req_forecasts)$mean
    final_forecast$naive <- naive(V1ts,no_req_forecasts)$mean
    final_forecast$randomwalk <- rwf(V1ts,drift = T,no_req_forecasts)$mean
    
    forecasts_selected <- sapply(final_forecast[mape_selected],function(x){x[1:no_req_forecasts]})
    
    combined_forecasts[[i]] <- rowSums(sweep(forecasts_selected,2,acc_stats_weight,FUN = "*"))
}
#Combine all forecasts row wise (M4 format) and export them to csv. Run the code again for different frequencies.
names(combined_forecasts) <- lapply(1:length(combined_forecasts),function(x){ paste("W",x,sep="")})
write.csv(do.call(rbind,combined_forecasts),file="weekly.csv")
