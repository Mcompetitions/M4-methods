################################################################################
# Forecasting by extracting part of a highly correlated series.
################################################################################

# For a series s, search in a given list of series for a segment with the 
# highest correlation coefficient with tail of s. The forecast is a scaled 
# and shifted continuation of that segment.
# Returns a forecast of s$x for s$h steps.

# s ----------- a time series in competition format (e.g. M4[[1]])
# seriess ----- list of series to search for a matching segment in
# window.mats - result of 'prepare.window.matrices', see below. The seriess
#               arguments have to match.
# top --------- optionally, the final forecast can be a mean of 'top' top 
#               candidates.
# threshold --- correlation has to exceed this number for as segment to be
#               considered a candidate. If no segment is found, naive forecast
#               is returned for such a series.
cor.predict = function(s, seriess, window.mats, top = 1, debug = F, 
                       threshold = 0.99) {
  window = nrow(window.mats[[1]])
  x = tail(s$x, window)
  cors = list()
  for (i in 1:length(seriess)) {
    cors[[i]] = cor(window.mats[[i]], x)
  }
  
  bst = c()
  bst.idx = c()
  scrs = c()
  bst.scr = 0
  
  for (i in 1:length(cors)) {
    p = cors[[i]]
    if (top == 1) {
      .order = which.max(p)
    } else {
      .order = head(order(p, na.last = T, decreasing = T), top)
    }
    for (k in .order) {
      if (!is.na(p[[k]]) && p[[k]] > bst.scr && p[[k]] > threshold) {
        if (length(bst) < top) {
          bst = c(bst, i)
          bst.idx = c(bst.idx, k)
          scrs = c(scrs, p[[k]])
          if (length(bst) == top) {
            bst.scr = min(scrs)
          }
        } else {
          idx = which.min(scrs)
          bst[[idx]] = i
          bst.idx[[idx]] = k
          scrs[[idx]] = p[[k]]
          bst.scr = min(scrs)
        }
      }
    }
  }
  
  if (debug) {
    print(bst)
    print(bst.idx)
    print(scrs)
  }
  
  if (length(bst) == 0) {
    return(list(correlated = 0, fcst = c(model.naive(s$x, s$h))))
  }
  
  ress = matrix(nrow = length(bst), ncol = s$h)
  for (i in 1:length(bst)) {
    res = c(seriess[[bst[[i]]]]$x[(bst.idx[[i]] + window):(
      bst.idx[[i]] + window + s$h - 1)])
    level.true = mean(x)
    std.true = std(x)
    
    corresponding = seriess[[bst[[i]]]]$x[bst.idx[[i]]:(
      bst.idx[[i]] + window - 1)]
    level.sim = mean(corresponding)
    std.sim = std(corresponding)
    res = (res - level.sim) / std.sim * std.true + level.true
    ress[i,] = res
  }
  return(list(correlated = 1, fcst = colMeans(ress)))
}


# Prepares a matrix containing all segments of length 'window' for each series
# in seriess. You need to feed the result of this to cor.predict.
# NOTE: This takes a lot of RAM. We didn't use it for Monthly or Quarterly data.

# window -- a window length (we used twice the forecast horizon)
# seriess - a list of series in competition format (e.g. M4[1:1000])
prepare.window.matrices = function(window, seriess) {
  window.mats = list()
  for (i in 1:length(seriess)) {
    m = matrix(nrow = window, ncol = length(seriess[[i]]$x) -
                 window - seriess[[1]]$h + 1)
    for (j in 1:window) {
      m[j,] = seriess[[i]]$x[j:(j + ncol(m) - 1)]
    }
    window.mats[[i]] = m
  }
  return(window.mats)
}