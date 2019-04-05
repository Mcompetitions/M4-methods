

##No of cores for parallel
ncore = 18           # no. of cores
set.seed(12345)      # random seed

perd  = "WEEKLY"      # capitalized letters
h.fc  = 13            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 52            # series frequency (1/4/12 for yearly/quarterly/monthly)

## Extract raw data

MyData <- read.csv(file="Weekly-train.csv", header=TRUE, sep=",")

# Create List from raw data

mydata.list <- setNames(split(MyData[,-1],seq(nrow(MyData))),MyData[,1])
mydata.list <-  mclapply(mydata.list, function(x) x[!is.na(x)],mc.cores = ncore)

save(mydata.list,file="mydata.list.rda")

#Train
mydata.train <- mclapply(mydata.list, function(x) head(x,length(x)-h.fc),mc.cores = ncore)

#Test
mydata.test <- mclapply(mydata.list, function(x) tail(x,h.fc),mc.cores = ncore)



# Create Time Series

mydata.train.ts <- mclapply(mydata.train,function(x) ts(as.vector(t(x)),frequency = freq),mc.cores = ncore)


## Full time series

mydata.full.ts <- mclapply(mydata.list,function(x) ts(as.vector(t(x)),frequency = freq),mc.cores = ncore)

## Save all the files
save(mydata.list, file ="mydata.list.rda")
save(mydata.train, file ="mydata.train.rda")
save(mydata.test, file = "mydata.test.rda")
save(mydata.train.ts,file = "mydata.train.ts.rda")

save(mydata.full.ts, file ="mydata.full.ts.rda")

