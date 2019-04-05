
ncore = 18

load("q_eats_QUARTERLY_m4.rda")
load("M4_mapa_QUARTERLY_srihari.rda")
load("forpro.vec.final.rda")
load("M4_b48_ets_QUARTERLY.rda")
load("for.Htheta.rda")


## Merge all data

# Rename Hybrid Theta
q4.htheta <- mclapply(for.Hytheta, function(x) t(t(as.vector(x))), mc.cores = ncore)

q4.htheta.nm <- lapply(q4.htheta, function(x) {colnames(x) <- c(("htheta"));x})

#Merge EATS with theta

for.q.eats <- Map(function(x, y) cbind(x,y), for.m.eats, q4.htheta.nm)


## Merge ForecastPro

for.q.eatshf <- Map(function(x, y) cbind(x,y),for.q.eats, forpro.vec.final)

# convert to data.frame
for.q.eatshf <- lapply(for.q.eatshf,as.data.frame)

## Merge MAPA
for.q.eatshfm <- Map(function(x, y) cbind(x,y),for.q.eatshf, for.M4.mapa)

## Extract names
lab.names <- noquote(paste("c(", paste(shQuote(names(for.q.eatshfm$Q1)), collapse = ", "), ")", sep = ""))
lab.names


## Mean median and trim mean
## Median and Mean function
med <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'naive.f', 'stlf.bc', 'stlfar.bc', 
             'stlfth.bc', 'htheta', 'fpro', 'mapa.bc', 'thief.ets', 'thief.arm')],1, median, na.rm = TRUE)
}

men <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'naive.f', 'stlf.bc', 'stlfar.bc', 
             'stlfth.bc', 'htheta', 'fpro', 'mapa.bc', 'thief.ets', 'thief.arm')],1, mean,  na.rm = TRUE)
}

men.trim <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'naive.f', 'stlf.bc', 'stlfar.bc',
             'stlfth.bc', 'htheta', 'fpro', 'mapa.bc', 'thief.ets', 'thief.arm')],1,mean,trim = .2, na.rm = TRUE)
}




## Compute mean, media and trim mean
#median
for.med.a <- mclapply(for.q.eatshfm,med,mc.cores = 18)

for.med.b <- lapply(for.med.a, function(x) t(t(x)))

for.med.c <- lapply(for.med.b, function(x) {colnames(x) <- c(("median.f"));x})

#mean

for.men.a <- mclapply(for.q.eatshfm,men,mc.cores = 18)

for.men.b <- lapply(for.men.a, function(x) t(t(x)))

for.men.c <- lapply(for.men.b, function(x) {colnames(x) <- c(("mean.f"));x})


#mean trim

for.ment.a <- mclapply(for.q.eatshfm,men.trim,mc.cores = 18)

for.ment.b <- lapply(for.ment.a, function(x) t(t(x)))

for.ment.c <- lapply(for.ment.b, function(x) {colnames(x) <- c(("meant.f"));x})


for.y.comb <- Map(function(x, y,z) cbind(x,y,z),for.med.c,for.men.c,for.ment.c)

## Finalize output

for.quarterly.m4 <- Map(function(x, y) cbind(x,y),for.q.eatshfm,for.y.comb)

## Save data

save(for.quarterly.m4,file="for.quarterly.m4.rda")
