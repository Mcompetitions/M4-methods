################################################################################
# Slightly modified code of otm function from forecTheta 2.2.
################################################################################

# The original crashes on seasonality test and/or decomposition if a series 
# is too short. It also crashed on some statistical tests due to sample size 
# being too small. Since we don't use those values, we disabled the tests here.

otm.fixed = function (y, h = 5, level = c(80, 90, 95), s = NULL, par_ini = c(y[1]/2, 
  0.5, 2), estimation = TRUE, lower = c(-1e+10, 0.1, 1), 
  upper = c(1e+10, 0.99, 1e+10), opt.method = "Nelder-Mead") 
{
  out = twoTL.fixed(y = y, h = h, level = level, s = s, par_ini = par_ini, 
              estimation = estimation, lower = lower, upper = upper, 
              opt.method = opt.method, dynamic = FALSE)
  out$method = "Optimised Theta Model"
  return(out)
}


twoTL.fixed = function (y, h, level, s, par_ini, estimation, lower, upper, 
          opt.method, dynamic) 
{
  if (!is.ts(y)) {
    stop("ERROR in twoTL function: y must be a object of time series class.")
  }
  if (!is.numeric(h)) {
    stop("ERROR in twoTL function: h must be a positive integer number.")
  }
  if (any(par_ini < lower) || any(par_ini > upper)) {
    stop("ERROR in twoTL function: par_ini out of range.")
  }
  n = length(y)
  if (n == 1) {
    return(list(mean = y))
  }
  
  fq = frequency(y)
  time_y = time(y)
  s_type = "multiplicative"
  if (!is.null(s)) {
    if (s == "additive") {
      s = TRUE
      s_type = "additive"
    }
  }
  
  # ensure we don't crash on deseasonalization for short or constant series
  if (is.null(s) && fq >= 4 && (n >= 2 * fq)) {
    xacf = acf(y, lag.max = fq + 1, plot = FALSE)$acf[-1, 
                                                      1, 1]
    clim = 1.64/sqrt(length(y)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    s = abs(xacf[fq]) > clim[fq]
    
    if (is.na(s)) {
      s = FALSE
    }
  }
  else {
    if (is.null(s)) {
      s = FALSE
    }
  }
  if (s) {
    y_decomp = decompose(y, type = s_type)$seasonal
    if ((s_type == "additive") || (s_type == "multiplicative" && 
                                   any(y_decomp < 0.01))) {
      s_type = "additive"
      y_decomp = decompose(y, type = "additive")$seasonal
      y = y - y_decomp
    }
    else {
      y = y/y_decomp
    }
  }
  # tnnTest.pvalue = terasvirta.test(y)$p.value
  mu = ell = A = B = meanY = numeric(n + h)
  Bn = 6 * (2 * mean((1:n) * y) - (1 + n) * mean(y))/(n^2 - 
                                                        1)
  An = mean(y) - (n + 1) * Bn/2
  new_y = as.numeric(y)
  SSM = function(par, computeForec = FALSE) {
    ell0 = par[1]
    alpha = par[2]
    theta = par[3]
    ell[1] = alpha * new_y[1] + (1 - alpha) * ell0
    meanY[1] = new_y[1]
    if (dynamic) {
      A[1] = new_y[1]
      B[1] = 0
      mu[1] = new_y[1]
    }
    else {
      A[1] = An
      B[1] = Bn
      mu[1] = ell0 + (1 - 1/theta) * (An + Bn)
    }
    limit = n
    if (computeForec) {
      limit = n + h
      new_y = c(new_y, rep(NA, h))
    }
    for (i in 1:(limit - 1)) {
      mu[i + 1] = ell[i] + (1 - 1/theta) * (A[i] * ((1 - 
        alpha)^i) + B[i] * (1 - (1 - alpha)^(i + 1))/alpha)
      if (i >= n) {
        new_y[i + 1] = mu[i + 1]
      }
      ell[i + 1] = alpha * new_y[i + 1] + (1 - alpha) * 
        ell[i]
      meanY[i + 1] = (i * meanY[i] + new_y[i + 1])/(i + 
                                                      1)
      if (dynamic) {
        B[i + 1] = ((i - 1) * B[i] + 6 * (new_y[i + 
                                                  1] - meanY[i])/(i + 1))/(i + 2)
        A[i + 1] = meanY[i + 1] - B[i + 1] * (i + 2)/2
      }
      else {
        A[i + 1] = An
        B[i + 1] = Bn
      }
    }
    return(list(mu = mu, ell = ell, A = A, B = B, meanY = meanY))
  }
  cte = mean(abs(y))
  sse = function(par) {
    if (any(par < lower) || any(par > upper)) {
      return(1e+10)
    }
    mu = SSM(par)$mu
    errors = (y[1:n] - mu[1:n])/cte
    if (dynamic) {
      return(sum(errors[3:n]^2))
    }
    else {
      return(sum(errors^2))
    }
  }
  if (estimation) {
    if (opt.method == "Nelder-Mead") 
      opt = optim(par = par_ini, fn = sse, method = "Nelder-Mead")
    if (opt.method == "L-BFGS-B") 
      opt = optim(par = par_ini, fn = sse, method = "L-BFGS-B", 
                  lower = lower, upper = upper)
    if (opt.method == "SANN") 
      opt = optim(par = par_ini, fn = sse, method = "SANN")
    par = opt$par
  }
  else {
    par = par_ini
  }
  out.SSM = SSM(par, computeForec = TRUE)
  mu = out.SSM$mu
  Y_fcast = ts(mu[(n + 1):(n + h)], start = end(y) + c(0, 
    1), frequency = frequency(y))
  Y_fitted = mu[1:n]
  Y_residuals = y - Y_fitted
  # shapTest.pvalue = shapiro.test(Y_residuals[3:n])$p.value
  if (!is.null(level)) {
    nSample = 200
    level = sort(level)
    alpha = par[2]
    theta = par[3]
    A = out.SSM$A[n]
    B = out.SSM$B[n]
    ell = out.SSM$ell[n]
    meanY = mean(y)
    sd.error = sd(Y_residuals[3:n])
    matForec.sample = matrix(NA, nrow = nSample, ncol = h)
    colnames(matForec.sample) = paste("h=", 1:h, sep = "")
    for (i in n:(n + h - 1)) {
      matForec.sample[, i + 1 - n] = ell + (1 - 1/theta) * 
        (A * ((1 - alpha)^i) + B * (1 - (1 - alpha)^(i + 
          1))/alpha) + rnorm(nSample, 0, sd.error)
      ell = alpha * matForec.sample[, i + 1 - n] + (1 - 
                                                      alpha) * ell
      meanY = (i * meanY + matForec.sample[, i + 1 - n])/(i + 
                                                            1)
      B = ((i - 1) * B + 6 * (matForec.sample[, i + 1 - 
                                                n] - meanY)/(i + 1))/(i + 2)
      A = meanY - B * (i + 2)/2
    }
    nn = length(level)
    qq = (1 - 0.01 * level)/2
    probs = numeric(2 * nn)
    probs[2 * (1:nn) - 1] = qq
    probs[2 * (1:nn)] = 1 - qq
    quantiles = t(apply(X = matForec.sample, MARGIN = 2, 
                        FUN = quantile, probs = probs))
    quantiles = ts(quantiles, start = end(y) + c(0, 1), 
                   frequency = frequency(y))
    colnames(quantiles) = unlist(lapply(X = 1:nn, FUN = function(X) paste(c("Lo", 
      "Hi"), level[X])))
  }
  if (s) {
    if (s_type == "multiplicative") {
      y = y * y_decomp
      Y_fitted = y_decomp * Y_fitted
      s_forec = snaive(y_decomp, h = h)$mean
      Y_fcast = s_forec * Y_fcast
      if (!is.null(level)) {
        for (i in 1:ncol(quantiles)) {
          quantiles[, i] = quantiles[, i] * s_forec
        }
      }
    }
    else {
      y = y + y_decomp
      Y_fitted = y_decomp + Y_fitted
      s_forec = snaive(y_decomp, h = h)$mean
      Y_fcast = s_forec + Y_fcast
      if (!is.null(level)) {
        for (i in 1:ncol(quantiles)) {
          quantiles[, i] = quantiles[, i] + s_forec
        }
      }
    }
    Y_residuals = y - Y_fitted
  }
  out = list()
  out$method = "Two Theta Lines Model"
  out$y = y
  out$s = s
  if (s) {
    out$type = s_type
  }
  out$opt.method = ifelse(estimation, opt.method, "none")
  out$par = matrix(par, ncol = 1, nrow = 3)
  rownames(out$par) = c("ell0", "alpha", "theta")
  colnames(out$par) = "MLE"
  omega = 1 - 1/par[3]
  out$weights = matrix(c(omega, 1 - omega), ncol = 1, nrow = 2)
  rownames(out$weights) = c("omega_1", "omega_2")
  colnames(out$weights) = "Estimative"
  out$fitted = ts(Y_fitted, start = start(y), frequency = frequency(y))
  out$residuals = ts(Y_residuals, start = start(y), frequency = frequency(y))
  out$mean = Y_fcast
  out$level = level
  if (!is.null(level)) {
    nn = length(level)
    out$lower = quantiles[, 2 * (1:nn) - 1]
    out$upper = quantiles[, 2 * (1:nn)]
  }
  else {
    out$lower = out$upper = NULL
  }
  # out$tests = matrix(c(tnnTest.pvalue, shapTest.pvalue), nrow = 2)
  # rownames(out$tests) = c("tnn-test", "shapiro-test")
  # colnames(out$tests) = c("p.value")
  return(structure(out, class = "thetaModel"))
}
