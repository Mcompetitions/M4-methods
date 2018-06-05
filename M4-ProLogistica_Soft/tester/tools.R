################################################################################
# Various helper functions
################################################################################

# Sets negative values in x to zeros
negative.to.zero = function(x) {
  x[which(x < 0)] = 0
  return(x)
}

# Calculates sMAPE error of naive forecast for last horizon of x
check.naive.tail = function(x) {
  naive.err = crit.smape(
    NULL, tail(x$x, x$h), model.naive(head(x$x, -x$h), x$h))
  return(mean(naive.err))
}



# Split data into batches. Intended for internal use.
batch.data = function(data, num.portions) {
  len.data = length(data)
  if (len.data == 1) {
    return(list(data))
  }
  
  batch.size = len.data / num.portions
  
  # this part tries to ensure a meaningful progess bar in pblapply
  max.batch.size = num.portions * 100
  if (num.portions > 1) {
    while (batch.size > max.batch.size) {
      batch.size = batch.size / num.portions
    }
  }
  
  batch.size = round(batch.size)
  if (batch.size == 0) {
    batch.size = 1
  }
  
  batch.inds = c(0)
  for (k in 1:(len.data - 1)) {
    if (k %% batch.size == 0) {
      batch.inds = c(batch.inds, k)
    }
  }
  batch.inds = c(batch.inds, len.data)
  
  batches = list()
  for (k in 2:length(batch.inds)) {
    batches[[k - 1]] = 
      data[(batch.inds[[k - 1]] + 1):batch.inds[[k]]]
  }
  return(batches)
}