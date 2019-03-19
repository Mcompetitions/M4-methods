
library("thief")

ncore = 18

h.fc=48

load("mydata.full.ts.hourly.rda")

load("M4_HOURLY207_srihari.rda")
load("M4_HOURLY414_srihari.rda")


for.hourly.m4 <- c(for.hm4_207,for.hm4_414)
for.hourly.m4 <- lapply(for.hourly.m4,as.data.frame)

for.naive <- function(x, h.fc, frequency = freq, transform = FALSE){
  
  if(transform & min(x, na.rm = TRUE) >= 0) {
    lambda    = BoxCox.lambda(na.contiguous(x), method = "guerrero", 
                              lower = 0, upper = 1)
    x.bc      = BoxCox(x, lambda)
  } else {
    lambda    = NULL
    x.bc      = x
    transform = FALSE
  }

        n <- thief(as.ts(x.bc,frequency=freq),h = h.fc,usemodel = "snaive")$mean



        return(as.vector(n))
        
        
}

for.n <- pbmclapply(mydata.full.ts,for.naive,frequency = 168,h.fc=48,transform = FALSE,mc.cores=ncore)

for.n.t <- mclapply(for.n, function(x) t(t(as.vector(x))), mc.cores = ncore)

for.n.nm <- lapply(for.n.t, function(x) {colnames(x) <- c(("snaive"));x})

for.hourly.m4.sn <- Map(function(x, y) cbind(x,y), for.hourly.m4, for.n.nm)

## remove x.naive and keep only snaive


keepvar <- function (x) {
  x[,c('ges','arima', 'snaive', 'tbats','dshw','elm','mlp')]
}

for.hourly.m4.sn.c <- mclapply(for.hourly.m4.sn,keepvar,mc.cores = ncore)

lab.names <- noquote(paste("c(", paste(shQuote(names(for.hourly.m4.sn.c$H1)), collapse = ", "), ")", sep = ""))
lab.names


med <- function (x) {
  apply(x[,c('ges', 'arima', 'snaive', 'tbats', 'dshw', 'elm', 'mlp')],1, median, na.rm = TRUE)
}




for.med.a <- mclapply(for.hourly.m4.sn.c,med,mc.cores = 18)

for.med.b <- lapply(for.med.a, function(x) t(t(x)))

for.med.c <- lapply(for.med.b, function(x) {colnames(x) <- c(("median.f"));x})


for.hourly.m4 <- Map(function(x, y) cbind(x,y),for.hourly.m4.sn.c,for.med.c)


save(for.hourly.m4,file="for.hourly.m4.rda")
