

library("thief")

ncore = 18


load("for.weekly.m4.rda")


for.weekly.m4.df <- lapply(for.weekly.m4,as.data.frame)


lab.names <- noquote(paste("c(", paste(shQuote(names(for.weekly.m4.df$W1)), collapse = ", "), ")", sep = ""))
lab.names


med <- function (x) {
  apply(x[,c('ets.bc', 'xets.bc', 'theta.bc', 'stl.bc', 'naive.f', 'thief.bc', 'thief.es', 'g.tbat')],1, median, na.rm = TRUE)
}




for.med.a <- mclapply(for.weekly.m4.df,med,mc.cores = 18)

for.med.b <- lapply(for.med.a, function(x) t(t(x)))

for.med.c <- lapply(for.med.b, function(x) {colnames(x) <- c(("median.f"));x})


for.weekly.m4.df <- Map(function(x, y) cbind(x,y),for.weekly.m4.df,for.med.c)


save(for.weekly.m4.df,file="for.weekly.m4.df.rda")





