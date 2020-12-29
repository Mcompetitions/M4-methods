
library(forecTheta)
rm(list=ls())
load("M4.RData")

Size = 100000
MaxH = 48

forec = matrix(NA, nrow=Size , ncol=MaxH)
lower = matrix(NA, nrow=Size , ncol=MaxH)
upper = matrix(NA, nrow=Size , ncol=MaxH)


#cnames <- c()
#for(i in 1:MaxH){
# cnames  <- c(cnames ,sprintf("F%d",i))
#}
#col.names(forec) <- cnames 
#col.names(lower) <- cnames 
#col.names(upper) <- cnames 

rnames <- c()
for(i in 1:Size){
 rnames  <- c(rnames ,M4[[i]]$st)
}
row.names(forec) <- rnames 
row.names(lower) <- rnames 
row.names(upper) <- rnames 

for(i in 1:Size){
 if(i %% 1000 == 0){print(i);}
 x=M4[[i]]$x
 if(M4[[i]]$n >= 5000)
{
x =  ts(M4[[i]]$x[(M4[[i]]$n-4999):M4[[i]]$n])
}
 h=M4[[i]]$h
res <- tryCatch({
     out = dotm(x,h,level=c(97.5))
 forec[i,1:h] = out$mean
 lower[i,1:h] = out$lower
 upper[i,1:h] = out$upper

}, warning = function(warning_condition) {
 return (0)
}, error = function(error_condition) {
return (0)
}, finally={

})

}

write.csv(forec, file = "dotm.csv", row.names = TRUE, col.names = TRUE)


write.csv(lower, file = "dotmL.csv", row.names = TRUE, col.names = TRUE)


write.csv(upper, file = "dotmU.csv", row.names = TRUE, col.names = TRUE)


