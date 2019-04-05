

ncore = 18

load("m_eats_MONTHLY_m4_final.rda")
load("M4_mapa_MONTHLY_srihari.rda")
load("for.Htheta.rda")
load("M4_bg_ets_10MONTHLY.rda")
load("M4_bg_ets_20MONTHLY.rda")
load("M4_bg_ets_30MONTHLY.rda")
load("M4_bg_ets_40MONTHLY.rda")
load("M4_bg_ets_48MONTHLY.rda")
load("forpro.vec.final.rda")

M4_b48_ets <- c(M4_bg_ets_10,M4_bg_ets_20,M4_bg_ets_30,M4_bg_ets_40,M4_bg_ets_48)

## Merge all data

# Rename Hybrid Theta
m4.htheta <- mclapply(for.Hytheta, function(x) t(t(as.vector(x))), mc.cores = ncore)

m4.htheta.nm <- lapply(m4.htheta, function(x) {colnames(x) <- c(("htheta"));x})

#Merge EATS with theta

for.m.eats <- Map(function(x, y) cbind(x,y), for.m.eats, m4.htheta.nm)


## Merge ForecastPro

for.m.eatshf <- Map(function(x, y) cbind(x,y),for.m.eats, forpro.vec.final)

# convert to data.frame
for.m.eatshf <- lapply(for.m.eatshf,as.data.frame)

## Merge MAPA
for.m.eatshfm <- Map(function(x, y) cbind(x,y),for.m.eatshf, for.M4.mapa)

#Merge Bag ets

bag.f  <-  lapply(M4_b48_ets, `[[`, 'f.mod')
m4.bagging <- mclapply(bag.f, function(x) t(t(as.vector(x))))

m4.bagging.nm <- lapply(m4.bagging, function(x) {colnames(x) <- c(("ets_bagging"));x})

for.m.eatshfmb <- Map(function(x, y) cbind(x,y),for.m.eatshfm, m4.bagging.nm)


## Extract names
lab.names <- noquote(paste("c(", paste(shQuote(names(for.m.eatshfmb$M1)), collapse = ", "), ")", sep = ""))
lab.names


## Mean median and trim mean
## Median and Mean function
med <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'naive.f', 'stlf.bc', 'stlfar.bc', 'stlfth.bc', 
             'htheta', 'fpro', 'mapa.bc', 'thief.ets', 'thief.arm', 'ets_bagging')],1, median, na.rm = TRUE)
}

men <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'naive.f', 'stlf.bc', 'stlfar.bc', 'stlfth.bc', 'htheta', 
             'fpro', 'mapa.bc', 'thief.ets', 'thief.arm', 'ets_bagging')],1, mean,  na.rm = TRUE)
}

men.trim <- function (x) {
  apply(x[,c('ets.bc', 'arima.bc', 'theta.bc', 'etsd.bc', 'naive.f', 'stlf.bc', 'stlfar.bc', 'stlfth.bc', 'htheta', 
             'fpro', 'mapa.bc', 'thief.ets', 'thief.arm', 'ets_bagging')],1,mean,trim = .2, na.rm = TRUE)
}




## Compute mean, media and trim mean
#median
for.med.a <- mclapply(for.m.eatshfmb,med,mc.cores = 18)

for.med.b <- lapply(for.med.a, function(x) t(t(x)))

for.med.c <- lapply(for.med.b, function(x) {colnames(x) <- c(("median.f"));x})

#mean

for.men.a <- mclapply(for.m.eatshfmb,men,mc.cores = 18)

for.men.b <- lapply(for.men.a, function(x) t(t(x)))

for.men.c <- lapply(for.men.b, function(x) {colnames(x) <- c(("mean.f"));x})


#mean trim

for.ment.a <- mclapply(for.m.eatshfmb,men.trim,mc.cores = 18)

for.ment.b <- lapply(for.ment.a, function(x) t(t(x)))

for.ment.c <- lapply(for.ment.b, function(x) {colnames(x) <- c(("meant.f"));x})


for.y.comb <- Map(function(x, y,z) cbind(x,y,z),for.med.c,for.men.c,for.ment.c)

## Finalize output

for.monthly.m4 <- Map(function(x, y) cbind(x,y),for.m.eatshfmb,for.y.comb)

## Save data

save(for.monthly.m4,file="for.monthly.m4.rda")
