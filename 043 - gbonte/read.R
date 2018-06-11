rm(list=ls())
source("library.R")
yearly<-read.csv("./M4DataSet/Yearly-train.csv")
print(dim(yearly))

quarterly<-read.csv("./M4DataSet/Quarterly-train.csv")
print(dim(quarterly))

monthly<-read.csv("./M4DataSet/Monthly-train.csv")
print(dim(monthly))


weekly<-read.csv("./M4DataSet/Weekly-train.csv")
print(dim(weekly))

daily<-read.csv("./M4DataSet/Daily-train.csv")
print(dim(daily))

hourly<-read.csv("./M4DataSet/Hourly-train.csv")
print(dim(hourly))


info<-read.csv("./M4DataSet/M4-info.csv")
print(dim(info))


TSERIES<-list()
cnt=1

maxS=60

for (i in 1:NROW(yearly)){
  tsi=unlist(yearly[i,-1])
  wna=which(!is.na(tsi))

  TS=tsi[wna]
  N<-length(TS)
  seas=0
  if (round(N/10)>3)
    seas=detectSeason(TS,maxs=min(maxS,round(N/10)))
  TSERIES[[cnt]]<-list(H=info[cnt,4],name=paste("Y",i,sep=""),
                       ts=TS,seas=seas)
  if (seas>0)
    cat("Series=",TSERIES[[cnt]]$name, " N=", N," Seas=", TSERIES[[cnt]]$seas,"\n")
  cnt=cnt+1
  if (i%%500==0)
    cat(".")
}

print(length(TSERIES))
save(file="TSERIES.Rdata",list=c("TSERIES"))


for (i in 1:NROW(quarterly)){
  tsi=unlist(quarterly[i,-1])
  wna=which(!is.na(tsi))
  TS=tsi[wna]
  N<-length(TS)
  seas=0
  if (round(N/10)>3)
    seas=detectSeason(TS,maxs=min(maxS,round(N/10)) )
  TSERIES[[cnt]]<-list(H=info[cnt,4],name=paste("Q",i,sep=""),
                       ts=TS,seas=seas)
  if (seas>0)
    cat("Series=",TSERIES[[cnt]]$name, " N=", N," Seas=", TSERIES[[cnt]]$seas,"\n")
  cnt=cnt+1
  if (i%%500==0)
    cat(".")
}
print(length(TSERIES))
save(file="TSERIES.Rdata",list=c("TSERIES"))

for (i in 1:NROW(monthly)){
  tsi=unlist(monthly[i,-1])
  wna=which(!is.na(tsi))
  TS=tsi[wna]
  N<-length(TS)
  seas=0
  if (round(N/10)>3)
    seas=detectSeason(TS,min(maxS,round(N/10)))
  TSERIES[[cnt]]<-list(H=info[cnt,4],name=paste("M",i,sep=""),
                       ts=TS,seas=seas)
  if (seas>0)
    cat("Series=",TSERIES[[cnt]]$name, " N=", N, " Seas=", TSERIES[[cnt]]$seas,"\n")
  cnt=cnt+1
  if (i%%500==0)
    cat(".")
}

print(length(TSERIES))
save(file="TSERIES.Rdata",list=c("TSERIES"))


for (i in 1:NROW(weekly)){
  tsi=unlist(weekly[i,-1])
  wna=which(!is.na(tsi))
  TS=tsi[wna]
  N<-length(TS)
  seas=0
  if (round(N/10)>3)
    seas=detectSeason(TS,maxs=min(maxS,round(N/10)))

  TSERIES[[cnt]]<-list(H=info[cnt,4],name=paste("W",i,sep=""),
                       ts=TS,seas=seas)
  if (seas>0)
    cat("Series=",TSERIES[[cnt]]$name," N=", N, " Seas=", TSERIES[[cnt]]$seas,"\n")
  cnt=cnt+1
  if (i%%500==0)
    cat(".")
}

print(length(TSERIES))
save(file="TSERIES.Rdata",list=c("TSERIES"))


for (i in 1:NROW(daily)){
  tsi=unlist(daily[i,-1])
  wna=which(!is.na(tsi))
  TS=tsi[wna]
  N<-length(TS)
  seas=0
  if (round(N/10)>3)
    seas=detectSeason(TS,min(maxS,round(N/10)))
  TSERIES[[cnt]]<-list(H=info[cnt,4],name=paste("D",i,sep=""),
                       ts=TS,seas=seas)
  if (seas>0)
    cat("Series=",TSERIES[[cnt]]$name, " Seas=", TSERIES[[cnt]]$seas,"\n")
  cnt=cnt+1
  cat(".")
}

print(length(TSERIES))
save(file="TSERIES.Rdata",list=c("TSERIES"))


for (i in 1:NROW(hourly)){
  tsi=unlist(hourly[i,-1])
  wna=which(!is.na(tsi))
  TS=tsi[wna]
  N<-length(TS)
  seas=0
  if (round(N/10)>3)
    seas=detectSeason(TS,min(maxS,round(N/10)))
  TSERIES[[cnt]]<-list(H=info[cnt,4],name=paste("H",i,sep=""),
                       ts=TS,seas=seas)
  if (seas>0)
    cat("Series=",TSERIES[[cnt]]$name, " N=", N," Seas=", TSERIES[[cnt]]$seas,"\n")
  cnt=cnt+1
  if (i%%500==0)
    cat(".")
}

print(length(TSERIES))


save(file="TSERIES.Rdata",list=c("TSERIES"))
