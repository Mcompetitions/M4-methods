
ncore = 18

load("M4_YEARLY_srihari.rda")
load("for.Htheta.rda")
load("forpro.vec.final.rda")


## Merge all data

# Rename Hybrid Theta
m4.htheta <- mclapply(for.Hytheta, function(x) t(t(as.vector(x))), mc.cores = ncore)

m4.htheta.nm <- lapply(m4.htheta, function(x) {colnames(x) <- c(("htheta"));x})

#Merge EATS with theta

for.y.eats <- Map(function(x, y) cbind(x,y), for.eats, m4.htheta.nm)


## Merge ForecastPro

for.y.eatshf <- Map(function(x, y) cbind(x,y),for.y.eats, forpro.vec.final)

# convert to data.frame
for.y.eatshf <- lapply(for.y.eatshf,as.data.frame)

## Extract names
lab.names <- noquote(paste("c(", paste(shQuote(names(for.y.eatshf$Y1)), collapse = ", "), ")", sep = ""))
lab.names


## Mean median and trim mean
## Median and Mean function
med <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'mapa.bc', 'naive.f', 'htheta', 'fpro')],1, median, na.rm = TRUE)
}

men <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'mapa.bc', 'naive.f', 'htheta', 'fpro')],1, mean,  na.rm = TRUE)
}

men.trim <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'mapa.bc', 'naive.f', 'htheta', 'fpro')],1,mean,trim = .2, na.rm = TRUE)
}




## Compute mean, media and trim mean
#median
for.med.a <- mclapply(for.y.eatshf,med,mc.cores = 18)

for.med.b <- lapply(for.med.a, function(x) t(t(x)))

for.med.c <- lapply(for.med.b, function(x) {colnames(x) <- c(("median.f"));x})

#mean

for.men.a <- mclapply(for.y.eatshf,men,mc.cores = 18)

for.men.b <- lapply(for.men.a, function(x) t(t(x)))

for.men.c <- lapply(for.men.b, function(x) {colnames(x) <- c(("mean.f"));x})


#mean trim

for.ment.a <- mclapply(for.y.eatshf,men.trim,mc.cores = 18)

for.ment.b <- lapply(for.ment.a, function(x) t(t(x)))

for.ment.c <- lapply(for.ment.b, function(x) {colnames(x) <- c(("meant.f"));x})


for.y.comb <- Map(function(x, y,z) cbind(x,y,z),for.med.c,for.men.c,for.ment.c)

## Finalize output

for.yearly.m4 <- Map(function(x, y) cbind(x,y),for.y.eatshf,for.y.comb)

## Save data

save(for.yearly.m4,file="for.yearly.m4.rda")



