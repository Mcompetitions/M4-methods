

library("forecast")
library("MAPA")
library("TStools")
library("smooth")

##No of cores for parallel
ncore = 18           # no. of cores
set.seed(12345)      # random seed

perd  = "MONTHLY"  # capitalized letters
h.fc  = 18            # forecast horizon (6/8/18 for yearly/quarterly/monthly)
freq  = 12            # series frequency (1/4/12 for yearly/quarterly/monthly)

## Extract raw data

MyData <- read.csv(file="Monthly-train.csv", header=TRUE, sep=",")

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


## Forecastpro

forpro.1 <- read.csv(file="!Project01 Numeric Output final.csv", header=TRUE, sep=",",strip.white=TRUE)
forpro.1 <- as.data.frame(forpro.1)

forpro.1["series"] <- NA
forpro.1$series <- (trimws(forpro.1$Category))

forpro.2 <-   forpro.1[mixedorder(forpro.1$series),]

forepro.list <- setNames(split(forpro.2[,c(-1:-5,-24)],seq(nrow(forpro.2))),forpro.2[,2])

forpro.vec <- mclapply(forepro.list, function(x) t(as.vector(x)),mc.cores = 18)

forpro.vec <- mclapply(forpro.vec, function(x) {rownames(x) <- c() 
return(x)},mc.cores = 18)


forpro.vec.final <- lapply(forpro.vec, function(x) {colnames(x) <- c(("fpro"));x})

save(forpro.vec.final,file = "forpro.vec.final.rda")
