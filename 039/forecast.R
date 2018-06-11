################################################################
## M4 COMPETITION: PREDILAB TEAM FORECASTS FOR 100.000 SERIES ##
################################################################
# To run this file the working directory should be the one where the data
#     is stored. This script produces an .csv file that contains the 
#     forecasts for all the time series in the required format
rm(list = ls())
source("auxiliar.R")

# Creating output file
fileOutput = "resultsPredilab.csv"
if (file.exists(fileOutput)){
  x = read.table(fileOutput, header= TRUE, sep = ",")
  initialSeriesNumber = dim(x)[1]+1
} else {
  head <- "id,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21,F22,F23,F24,F25,F26,F27,F28,F29,F30,F31,F32,F33,F34,F35,F36,F37,F38,F39,F40,F41,F42,F43,F44,F45,F46,F47,F48"
  write.table(head, file = fileOutput, col.names = FALSE, quote = FALSE, row.names = FALSE)
  initialSeriesNumber = 1
}

## Select random indices if required
# seriesInd = sample(1 : 100000, 10)   # This for randomly chosen time series
seriesInd = initialSeriesNumber : 100000               # This for ALL time series

for (i in 1 : 6){
  if (i == 1){   # Yearly data
    fileInput = "Yearly-train.csv"
    frequency = 1
    horizon = 6
    labels = seriesInd[which(seriesInd <= 23000)]
    subindex = labels
  } else if (i == 2) {   # Quarterly data
    fileInput = "Quarterly-train.csv"
    frequency = 4
    horizon = 8
    labels = seriesInd[which((seriesInd > 23000) + (seriesInd <= 47000) == 2)]
    subindex = labels - 23000
  } else if (i == 3) {   # Monthly data
    fileInput = "Monthly-train.csv"
    frequency = 12
    horizon = 18
    labels = seriesInd[which((seriesInd > 47000) + (seriesInd <= 95000) == 2)]
    subindex = labels - 47000
  } else if (i == 4) {   # Weekly data
    fileInput = "Weekly-train.csv"
    frequency = 1
    horizon = 13
    labels = seriesInd[which((seriesInd > 95000) + (seriesInd <= 95359) == 2)]
    subindex = labels - 95000
  } else if (i == 5) {   # Daily data
    fileInput = "Daily-train.csv"
    frequency = 7
    horizon = 14
    labels = seriesInd[which((seriesInd > 95359) + (seriesInd <= 99586) == 2)]
    subindex = labels - 95359
  } else if (i == 6) {   # Hourly data
    fileInput = "Hourly-train.csv"
    frequency = 24
    horizon = 48
    labels = seriesInd[which(seriesInd > 99586)]
    subindex = labels - 99586
  }
  if (length(subindex) > 0){
    x = read.table(fileInput, header= TRUE, sep = ",")
    y = x[subindex, ]
    nSeries = dim(y)[1]
    forecasts = matrix(NA, nSeries, 48)
    row.names(forecasts) = paste(matrix(substr(fileInput, 1, 1), nSeries, 1), labels, sep = "")
    ini = 1
    for (k in 1 : nSeries){
      ind = max(which(y[k, ] != 0))
      yk = unlist(y[k, 2 : ind])
      forecasts[k, 1 : horizon] = predilab(yk, length(yk), horizon, frequency)$forecasts
      plot(c(yk, forecasts[k, ]), type = "l", col = "red")
      lines(yk)
      title(labels[k])
      print(paste(k, " of ", nSeries))
      if (k %% 100 == 0 || k == nSeries){
        write.table(forecasts[ini : k, ], sep = ",", file = fileOutput, quote = FALSE, append = TRUE,
                  col.names = FALSE, row.names = TRUE)
        ini = k + 1
        cat("\014")
      }
    }
  }
}

