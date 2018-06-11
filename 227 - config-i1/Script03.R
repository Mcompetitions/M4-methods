load("M4Full.Rdata")

library(Mcomp)

library(smooth)
library(foreach)
# If you work on Linux or Mac, use this
library(doMC)
registerDoMC(detectCores())
# Otherwise use doParallel package
# library(doParallel)
# registerDoParallel(detectCores())

# Make a subset from M4
M4Subset <- subset(M4Full,"MONTHLY");

# The length of the dataset.
nSeries <- length(M4Subset)
h <- M4Subset[[1]]$h

# These values are needed for the prediction intervals
nModels <- 4;
bins <- 100-1

# This is needed for appropriate combination of prediction intervals
ourQuantiles <- array(NA,c(nModels,bins,h),dimnames=list(paste0("Model",c(1:nModels)),
                                                         c(1:bins)/(bins+1),paste0("h",c(1:h))))
minMaxQuantiles <- matrix(NA,2,h)
# Prepare the matrix for the sequences from min to max for each h
ourSequence <- array(NA,c(bins,h))

listOfForecasts <- foreach(i=1:nSeries) %dopar% {
    x <- M4Subset[[i]]$x
    
    #### This is model fitting ####
    esModel <- es(x,h=h,intervals="p",level=0);
    cesModel <- auto.ces(x,h=h,intervals="p",level=0);
    ssarimaModel <- auto.ssarima(x,h=h,intervals="p",level=0);
    gesModel <- auto.ges(x,h=h,intervals="p",level=0);
    
    # Calculate AIC weights
    icWeights <- c(AICc(esModel),AICc(cesModel),AICc(ssarimaModel),AICc(gesModel));
    icBest <- min(icWeights);
    icWeights <- exp(-0.5*(icWeights-icBest)) / sum(exp(-0.5*(icWeights-icBest)));
    
    #### This part is for combining the prediction intervals ####
    
    # Write down the median values for all the models
    ourQuantiles[1,"0.5",] <- esModel$lower
    ourQuantiles[2,"0.5",] <- cesModel$lower
    ourQuantiles[3,"0.5",] <- ssarimaModel$lower
    ourQuantiles[4,"0.5",] <- gesModel$lower
    
    # Do loop writing down all the quantiles
    for(j in 1:((bins-1)/2)){
        esModel <- es(x,model=esModel,h=h,intervals="p",level=j*2/(bins+1))
        ourQuantiles[1,(bins+1)/2-j,] <- esModel$lower
        ourQuantiles[1,(bins+1)/2+j,] <- esModel$upper
        
        cesModel <- ces(x,model=cesModel,h=h,intervals="p",level=j*2/(bins+1))
        ourQuantiles[2,(bins+1)/2-j,] <- cesModel$lower
        ourQuantiles[2,(bins+1)/2+j,] <- cesModel$upper
        
        ssarimaModel <- ssarima(x,model=ssarimaModel,h=h,intervals="p",level=j*2/(bins+1))
        ourQuantiles[3,(bins+1)/2-j,] <- ssarimaModel$lower
        ourQuantiles[3,(bins+1)/2+j,] <- ssarimaModel$upper
        
        gesModel <- ges(x,model=gesModel,h=h,intervals="p",level=j*2/(bins+1))
        ourQuantiles[4,(bins+1)/2-j,] <- gesModel$lower
        ourQuantiles[4,(bins+1)/2+j,] <- gesModel$upper
    }
    
    # Write down minimum and maximum values between the models for each horizon
    minMaxQuantiles[1,] <- apply(ourQuantiles,3,min)
    minMaxQuantiles[2,] <- apply(ourQuantiles,3,max)
    # Prepare an array with the new combined probabilities
    newProbabilities <- array(NA,c(bins,h),dimnames=list(c(1:bins),dimnames(ourQuantiles)[[3]]))
    for(j in 1:h){
        ourSequence[,j] <- seq(minMaxQuantiles[1,j],minMaxQuantiles[2,j],length.out=bins)
        for(k in 1:bins){
            newProbabilities[k,j] <- sum(icWeights %*% (ourQuantiles[,,j] <= ourSequence[k,j])) / (bins+1)
            # newProbabilities[k,j] <- length(which(ourQuantiles[,,j] <= ourSequence[k,j]))/(nModels*(bins+1))
        }
    }
    
    # The correct intervals - quantiles, for which the newP is the first time > than selected value
    intervalsCorrect <- matrix(NA,2,h,dimnames=list(c("Lower","Upper"),dimnames(ourQuantiles)[[3]]))
    for(j in 1:h){
        intervalsCorrect[1,j] <- ourSequence[newProbabilities[,j]>=0.025,j][1]
        intervalsCorrect[2,j] <- ourSequence[newProbabilities[,j]>=0.975,j][1]
    }
    
    forecasts <- (esModel$forecast * icWeights[1] + cesModel$forecast * icWeights[2] +
                      ssarimaModel$forecast * icWeights[3] + gesModel$forecast * icWeights[4]);
    matrixReturned <- rbind(c(forecasts),intervalsCorrect)
    rownames(matrixReturned)[1] <- "Forecast"
    return(matrixReturned)
}

# Transform the list into the matrices for the csv files
matrixForecasts <- matrix(NA,nSeries,h);
colnames(matrixForecasts) <- paste0("f",c(1:h));
rownames(matrixForecasts) <- rep("NA",nSeries);
for(i in 1:nSeries){
    rownames(matrixForecasts)[i] <- M4Subset[[i]]$sn;
}
matrixUpper <- matrixLower <- matrixForecasts
for(i in 1:nSeries){
    matrixForecasts[i,] <- listOfForecasts[[i]][1,];
    matrixLower[i,] <- listOfForecasts[[i]][2,];
    matrixUpper[i,] <- listOfForecasts[[i]][3,];
}

write.csv(matrixForecasts,file="M4MonthlyForecasts.csv")
write.csv(matrixLower,file="M4MonthlyLower.csv")
write.csv(matrixUpper,file="M4MonthlyUpper.csv")
