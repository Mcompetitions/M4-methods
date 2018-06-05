################################################################################
# Slightly modified version of thetaf function from forecast 8.2. 
################################################################################

# The original code sometimes tries to divide by 0.
# Also, if a series is too short, the original crashes on a deseasonalization.

thetaf.fixed = function (y, h = ifelse(frequency(y) > 1, 2 * frequency(y), 10), 
                         level = c(80, 95), fan = FALSE, x = y) 
{
  if (fan) 
    level <- seq(51, 99, by = 3)
  else {
    if (min(level) > 0 & max(level) < 1) 
      level <- 100 * level
    else if (min(level) < 0 | max(level) > 99.99) 
      stop("Confidence limit out of range")
  }
  n <- length(x)
  if (n == 1) {
    return(list(mean = x))
  }
  
  x <- as.ts(x)
  m <- frequency(x)
  
  # ensure that the decompose() will not crash on short or constant series
  if ((m > 1) && (n >= 2 * m)) {
    r <- as.numeric(acf(x, lag.max = m, plot = FALSE)$acf)[-1]
    stat <- sqrt((1 + 2 * sum(r[-m]^2))/n)
    seasonal <- (abs(r[m])/stat > qnorm(0.95))
    
    if (is.na(seasonal)) {
      seasonal <- FALSE
    }
  }
  else seasonal <- FALSE
  origx <- x
  if (seasonal) {
    decomp <- decompose(x, type = "multiplicative")
    x <- seasadj(decomp)
  }
  fcast <- ses(x, h = h)
  tmp2 <- lsfit(0:(n - 1), x)$coef[2]/2
  alpha <- fcast$model$par["alpha"]
  
  # prevent division by 0
  if (alpha == 0) {
    alpha = 1e-30
  }
  
  fcast$mean <- fcast$mean + tmp2 * (0:(h - 1) + (1 - (1 - alpha)^n)/alpha)
  if (seasonal) 
    fcast$mean <- fcast$mean * rep(tail(decomp$seasonal, m), 
                                   trunc(1 + h/m))[1:h]
  fcast.se <- sqrt(fcast$model$sigma) * sqrt((0:(h - 1)) * alpha^2 + 1)
  nconf <- length(level)
  fcast$lower <- fcast$upper <- ts(matrix(NA, nrow = h, ncol = nconf))
  tsp(fcast$lower) <- tsp(fcast$upper) <- tsp(fcast$mean)
  for (i in 1:nconf) {
    zt <- -qnorm(0.5 - level[i]/200)
    fcast$lower[, i] <- fcast$mean - zt * fcast.se
    fcast$upper[, i] <- fcast$mean + zt * fcast.se
  }
  fcast$x <- origx
  fcast$level <- level
  fcast$method <- "Theta"
  fcast$model <- list(alpha = alpha, drift = tmp2, sigma = fcast$model$sigma)
  fcast$model$call <- match.call()
  return(fcast)
}
