library(forecast)
library(pbapply)

cleanUpper <- function(x){
  results <- x
  parsedSeries <- strsplit(x, split = ",")[[1]]
  seriesName <- parsedSeries[1]
  parsedSeries <- as.numeric(parsedSeries[-1])
  order <- arimaorder(auto.arima(parsedSeries))[2]
  # Prevent explosive growth
  if(order > 1 && tail(parsedSeries, 1) / head(parsedSeries, 1) > 10){
    multFactor <- seq(1, by = 0.025, length.out = length(parsedSeries))
    parsedSeries <- mean(head(parsedSeries, 2)) * multFactor
    results <- paste(c(seriesName, parsedSeries), collapse = ",")
  }
  return(results)
}


dat <- read.table("~/m4/upper/upper_submission.csv", stringsAsFactors=FALSE)
cleaned <- pblapply(dat$V1, FUN = cleanUpper, cl = 8)
capture.output(
    for(i in cleaned){
      cat(i, '\n')
    }, file = "upper_cleaned.csv")
