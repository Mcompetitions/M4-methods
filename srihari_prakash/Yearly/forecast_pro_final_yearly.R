library("dplyr")
library("data.table")
library("gtools")

load("mydata.list.rda")

ncore = detectCores()-1
freq=1

mydata.forpro <- mclapply(mydata.list,function(x) ts(as.vector(t(x)),frequency = freq,end=c(2018)),mc.cores = ncore)
mydata.forpro1 <- mclapply(mydata.forpro,function(x) data.frame(Y=as.matrix(x), date=(as.Date(x,format="%m %Y %d"))),mc.cores = ncore)

dt<-rbindlist(mydata.forpro1, use.names=TRUE, fill=TRUE,idcol="series")

tDatCast<- dcast(dt, series~date,value.var = 'Y')
tDatCast <- as.data.frame(tDatCast[mixedsort(series),])

total               <- rep("Total",nrow(tDatCast))
"Starting Year"     <- rep(year(min(names(tDatCast))),nrow(tDatCast))
"Starting Period"   <- rep(month(min(names(tDatCast))),nrow(tDatCast))
"Periods Per Year"  <- rep(12,nrow(tDatCast))
"Periods Per Cycle" <- rep(12,nrow(tDatCast))

tDatCast1 <- as.data.frame(cbind(total,Category = tDatCast[,1], Description = tDatCast[,1],`Starting Year`,`Starting Period`,`Periods Per Year`,
                                 `Periods Per Cycle`,tDatCast[,-1]))

tDatCast1[is.na(tDatCast1)] <- " "


write.csv(tDatCast1,file="data_all_final.csv",row.names=FALSE)


#Version 2

freq=1
end=c(2018)

rest.hist <- function(x,freq,end) {
  
  ts(as.vector(t(x)),frequency = freq,end=end)
}


mydata.forpro <- mclapply(mydata.list,rest.hist,freq=freq,end=end,mc.cores = ncore)
mydata.forpro1 <- mclapply(mydata.forpro,function(x) data.frame(Y=as.matrix(x), date=(as.Date(x,format="%m %Y %d"))),mc.cores = ncore)

dt<-rbindlist(mydata.forpro1, use.names=TRUE, fill=TRUE,idcol="series")


total               <- rep("Total",nrow(dt))
hist_year           <- year(dt$date)  
hist_period         <- month(dt$date)
PPY                 <- rep(freq,nrow(dt))
PPC                 <- rep(freq,nrow(dt))

fpro.m <- as.data.frame(cbind(total,Category = dt$series, Description = dt$series,hist_year,hist_period ,PPY,PPC,hist_value = dt$Y))


write.csv(fpro.m,file="m4_yearly_fpro_t_final.csv",row.names=FALSE)


