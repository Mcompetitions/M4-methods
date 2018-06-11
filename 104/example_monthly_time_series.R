library(forecast)
library(parallel)
library(TSclust)

#this code works fine in UNIX based OS
MBB <- function(x, window_size) {
  
  bx = array(0, (floor(length(x)/window_size)+2)*window_size)
  for (i in 1:(floor(length(x)/window_size)+2)){
    c <- sample(1:(length(x)-window_size+1),1)
    bx[((i-1)*window_size+1):(i*window_size)] <- x[c:(c+window_size-1)]
  }
  start_from <- sample(0:(window_size-1),1) + 1
  bx[start_from:(start_from+length(x)-1)]
}



bld.mbb.bootstrap<-function (x, num, block_size = if (frequency(x) > 1) 2 * frequency(x) else 8) 
{
  freq <- frequency(x)
  xs <- list()
  xs[[1]] <- x
  if (num > 1) {
    lambda <- BoxCox.lambda(x, lower = 0, upper = 1)
    x.bc <- BoxCox(x, lambda)
    if (freq > 1) {
      x.stl <- stl(ts(x.bc, frequency = freq), "per")$time.series
      seasonal <- x.stl[, 1]
      trend <- x.stl[, 2]
      remainder <- x.stl[, 3]
    }
    else {
      trend <- 1:length(x)
      suppressWarnings(x.loess <- loess(x.bc ~ trend, span = 6/length(x), 
                                        degree = 1))
      seasonal <- rep(0, length(x))
      trend <- x.loess$fitted
      remainder <- x.loess$residuals
    }
    for (i in 2:num) {
      xs[[i]] <- InvBoxCox(trend + seasonal + MBB(remainder, 
                                                  block_size), lambda)
    }
  }
  xs
}



baggedETS2<-function (y,cores=detectCores()-1,nclusters=5,distance="EUCL",silhouette=F,
                      h_pseudo = ifelse(frequency(object$x) > 1, 2 * frequency(object$x), 
                                                         10),
                      bootstrapped_series = bld.mbb.bootstrap(y, 1000), 
         ...) 
{
############## Intervention TIAGO ###############
  
  start.i <- tsp(y)[1]
  start.f <- tsp(y)[2] + 1/frequency(y)
  
  
  real.pre<-y[((length(y)-h_pseudo+1):length(y))]
  
  
  if(length(y)-h_pseudo>h_pseudo){
    #real.pre<-y[((length(y)-h_pseudo+1):length(y))]
    pseudo <- mclapply(bootstrapped_series,mc.cores=cores, function(x) {
      ts(x[(1:(length(y)-h_pseudo))],frequency=frequency(y),start=start.i)
    })
    
    
    forecasts_pseudo <- mclapply(pseudo,mc.cores=cores, function(x) {
      mod <- forecast(ets(x),h=h_pseudo)$mean
    })
    
  #  forecasts_pseudo <- mclapply(pseudo,mc.cores=cores, function(x) {
  #      mod <- hw(x,h=h_pseudo)$mean
  #    })
      
    
    resultado.pre.lista<-list()
    
    resultado.pre.lista<-mclapply(forecasts_pseudo,mc.cores=cores, function(x) {
      100*sum(abs((real.pre-as.numeric(x))/real.pre))/length(real.pre)})
    
    resultado.pre<-unlist(resultado.pre.lista)
    
    selec=which(rank(resultado.pre)<300)
    
    
    matseries<-ts(matrix(unlist(bootstrapped_series), ncol = length(bootstrapped_series), byrow = F),frequency=frequency(y),start=start.i)
    
    selecao<-NULL
    selecClus<-NULL
    eucl<-diss(matseries[,selec],distance)
    
    #silhouette
    if (silhouette==T){
      k<-(2:100)
      teste<-sapply(k,function(x){pam(eucl, k=x) $ silinfo $ avg.width})
      k.best<-which.max(teste)+1
    }else{k.best=nclusters}  
    
    eucl.pamclus <- pam(eucl, k = k.best)$clustering
   
    
    Nh<-NULL
    n<-100
    nh<-NULL
    Sh<-NULL
    selecClus<-NULL
    selecao3<-list()
    
    t<-(1:k.best)
    Nh<-as.numeric(table(eucl.pamclus))
    
    nh<-round(Nh/299*100)
    nh<-ifelse(nh==0,1,nh)
    
    
    for (t in 1:k.best){
      selecao2<-NULL
      teste2<-names(eucl.pamclus[eucl.pamclus==t])
      
      for (i in (1:length(teste2))){
        pre_sele2<-as.numeric(strsplit(teste2," ")[[i]][2])
        selecao2<-c(selecao2,pre_sele2)
      }
      selecao3[[t]]<-selecao2
      
    }
    
    
    for (t in 1:k.best){
      selecClus<-c(selecClus,selecao3[[t]][which(rank(resultado.pre[selecao3[[t]]],ties.method ="first")<(nh[t]+1))])
    }
    bootstrapped_series_ori<-bootstrapped_series
    bootstrapped_series<-bootstrapped_series[selecClus]
  }else{
    bootstrapped_series_ori<-bootstrapped_series
    bootstrapped_series<-bootstrapped_series[sample(1:1000,100)]
    k.best<-NA
  }
###########################################
  
  
  mod_boot <- mclapply(bootstrapped_series,mc.cores=cores, function(x) {
    mod <- ets(x,...)
  })
  out <- list()
  out$y <- as.ts(y)
  out$selec<-selec
  out$resultado.pre<-resultado.pre
  out$clusters<-selecao3
  out$bootstrapped_series <- bootstrapped_series
  out$bootstrapped_series_ori <- bootstrapped_series_ori
  out$models <- mod_boot
  out$etsargs <- list(...)
  fitted_boot <- lapply(out$models, fitted)
  fitted_boot <- as.matrix(as.data.frame(fitted_boot))
  out$fitted <- ts(apply(fitted_boot, 1, mean))
  tsp(out$fitted) <- tsp(out$y)
  out$residuals <- out$y - out$fitted
  out$series <- deparse(substitute(y))
  out$k<-k.best
  out$method <- "baggedETS"
  out$call <- match.call()
  return(structure(out, class = c("baggedETS")))
}





forecast.baggedETS2<-function (object, cores=detectCores()-1,h = ifelse(frequency(object$x) > 1, 2 * frequency(object$x), 
                             10), ...) 
{
  out <- list(model = object, series = object$series, x = object$y, 
              method = object$method)
  tspx <- tsp(out$x)
  
  forecasts_boot <- mclapply(out$model$models,mc.cores=cores ,function(mod) {
    forecast(mod, PI = FALSE, h = h)$mean
  })
  forecasts_boot <- as.matrix(as.data.frame(forecasts_boot))
  colnames(forecasts_boot) <- NULL
  if (!is.null(tspx)) 
    start.f <- tspx[2] + 1/frequency(out$x)
  else start.f <- length(out$x) + 1
  out$forecasts_boot <- forecasts_boot
  out$mean <- ts(apply(forecasts_boot, 1, mean), frequency = frequency(out$x), 
                 start = start.f)
  out$median <- ts(apply(forecasts_boot, 1, median))
  out$lower <- ts(apply(forecasts_boot, 1, min))
  out$upper <- ts(apply(forecasts_boot, 1, max))
  out$level <- 100
  tsp(out$median) <- tsp(out$lower) <- tsp(out$upper) <- tsp(out$mean)
  class(out) <- "forecast"
  out
}




devtools::install_github("carlanetto/M4comp2018")


library(forecast)
library(parallel)
library(TSclust)
library(miscTools)
library(M4comp2018)
data(M4)
names(M4[[1]])
#> [1] "st"     "x"      "n"      "type"   "h"      "period"
#extract yearly series
Monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
Monthly_train<-Monthly_M4
rm(Monthly_M4)
gc()

tamanho<-unlist(lapply(Monthly_train,function(y)length(y$x)))


cores<-parallel::detectCores()


seasonal<-list()
set.seed(100)

teste<-mclapply(1:48000, function(i) {
  
  M<-as.numeric(Monthly_train[[i]]$x)
  M1<-M[!is.na(M)]
  real<-ts(M1,frequency=12)
  a<-ets(real)
  seasonal<-a$components[3]
  seasonal
})


padroes<-unlist(teste)

set.seed(100)
inicio<-Sys.time()

mclapply(1:48000, function(i) {
  M<-as.numeric(Monthly_train[[i]]$x)
  M1<-M[!is.na(M)]
  real<-ts(M1,frequency=12)
  
  if(padroes[i]=="N"){
    real<-ts(M1,frequency=1)
  }
  
  
  a<-baggedETS2(real,silhouette = T,h_pseudo = 18,cores=cores)
  b<-forecast.baggedETS2(a,h=18)$median
  
  
  texto2<-paste0("previsoes/monthly_prev_",i)
  save(b,file=texto2)
  
})
fim<-Sys.time()

fim-inicio



