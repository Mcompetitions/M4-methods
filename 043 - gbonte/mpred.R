## Script returning for each series a number of continuations (in the field ContM) on the basis of the best models stored into "calibration.all.Rdata"

rm(list=ls())
library(devtools)
install_github("gbonte/gbcode")
source("library.R")
library(gbcode) ##

library(foreach)
library(doParallel)
library(foreach)
ncores=15
cl <- makeForkCluster(ncores)
registerDoParallel(cl)

ex<-function(x){
  return(!is.null(x$sd))
  
}


load("calibration.all.Rdata")


load("TSERIES.Rdata")
savefile<-TRUE
NTS=100000

Methods=c("liniter","lindirect","rfiter",
          "rfdirect","stats","lazyiter","lazydirect","mimo.comb","timefit")



SS<-seq(0,NTS,by=ncores)
for (s in 1:(length(SS)-1)){
  print(SS[s])
  FF<-foreach(r=(SS[s]+1):SS[s+1]) %dopar% {
    Cont<-NULL
    
    TSorig<-TSERIES[[r]]$ts
    
    print(r)
    log=allFF[r,]
    
    inds=log[3:(length(log))]
    ## method, order, detrend, diff,C,dummy
    for (cnt.mod in c(0,35)){
      for (mod in 1:5){
        ##print(inds[(cnt.mod+1):(cnt.mod+6)])
        method=Methods[inds[cnt.mod+1]]
        print(method)
        n=inds[cnt.mod+2]
        detrend=inds[cnt.mod+3] ## detrend: 1 no detrending, 2 linear, 3 geometric
        diff=inds[cnt.mod+4]
        C=inds[cnt.mod+5]
        dummy=inds[cnt.mod+6]
        NMSE=inds[cnt.mod+7]
        cnt.mod=cnt.mod+7
        
        eps=1e-10
        minr<-20
        L<-length(TSorig)
        maxn=min(4,max(1,floor(L/10)))
        maxC=4
        
        
        if (L >300 & L <500)
          minr<-10
        if (L >=500 & L <1000)
          minr<-5
        if (L>=1000)
          minr<-3
        for (rr in 1:min(c(minr,round(length(TSorig)/3),(1+round(20*(-1+1/(min(NMSE,1)+eps))))))){
          seas<-0
          
          TS=TSorig[rr:length(TSorig)]
          
          N<-length(TS)
          maxn=min(5,max(1,floor(N/10)))
          N.tr<-N
          TS<-array(TS,c(N,1))
          H<-TSERIES[[r]]$H ## horizon prediction
          TS.ts<-numeric(H)
          
          cat("Series=",r,":",TSERIES[[r]]$name, "N=", N, "H=", H, "\n" )
          
          #############
          #####
          typeser=substr(TSERIES[[r]]$name,1,1)
          if (typeser=="Y")
            dummylength=2
          if (typeser=="M")
            dummylength=12
          if (typeser=="Q")
            dummylength=3
          if (typeser=="D")
            dummylength=7
          if (typeser=="W")
            dummylength=4
          if (typeser=="H")
            dummylength=24
          
          
        
          TS.tr=TS[,1]
          if (diff>1){
            DTS.tr=differ(TS.tr)
            TS.tr=DTS.tr$D
          } # 
          scaled<-TRUE
          if (sd(TS.tr)<0.01)
            scaled<-FALSE
          
          if (scaled){
            sTS.tr<-scale(TS.tr)
            TS.tr<-array(sTS.tr,c(length(sTS.tr),1))
          }# if scaled
          
          if (detrend ==2){
            
            trnd=pred("lin",seq(TS.tr),TS.tr,seq(c(TS.tr)),classi=FALSE) 
            trnd.ts=pred("lin",seq(TS.tr),TS.tr,seq(c(TS.tr,TS.ts)),classi=FALSE)[(length(TS.tr)+1):(length(TS.tr)+H)]
            TS.tr=TS.tr-trnd
          }# if detrend
          
          if (detrend ==3){
            
            trnd=pred("lin",seq(TS.tr),TS.tr,seq(c(TS.tr)),classi=FALSE) 
            trnd[abs(trnd)<0.01]<-0.01
            
            
            trnd.ts=pred("lin",seq(TS.tr),TS.tr,seq(c(TS.tr,TS.ts)),classi=FALSE)[(length(TS.tr)+1):(length(TS.tr)+H)]
            TS.tr=TS.tr/trnd
          }#
          
          if (diff==1 && seas>0){
            outpseas=removeSeas(TS.tr,seas)
            Ts.tr=outpseas$TSdeseas
            TSeas0=outpseas$TSeas0
            
          }#
          
          TS.tr=array(TS.tr,c(length(TS.tr),1))
          
          
          if (diff>1){
            if (method=="stats" ){
              DY.cont=StatPredictors1(c(TS.tr),H,(n-1)*maxC+C+(dummy-1)*maxn*maxC)
            } else{
              
              if (dummy==1)
                DUM=-2
              if (dummy==2)
                DUM=-1
              if (dummy==3)
                DUM=0
              if (dummy >3)
                DUM=dummylength
              DY.cont=multiplestepAhead(TS.tr,n=n, H=H,method=method,C=C,
                                        dummy=DUM,Kmin=4)
              
            }#
            if (detrend==2)
              DY.cont=DY.cont+trnd.ts
            if (detrend==3)
              DY.cont=DY.cont*trnd.ts
            if (scaled)
              DY.cont=DY.cont* attr(sTS.tr, 'scaled:scale') + attr(sTS.tr, 'scaled:center')    
            
            Y.co=differB(DY.cont,DTS.tr$last)
          } else {  ## if diff>1
            if (method=="stats"){
              Y.co=StatPredictors1(c(TS.tr),H, (n-1)*maxC+C+(dummy-1)*maxn*maxC)
            } else{
              
              if (dummy==1)
                DUM=-2
              if (dummy==2)
                DUM=-1
              if (dummy==3)
                DUM=0
              if (dummy >3)
                DUM=dummylength
              Y.co=multiplestepAhead(TS.tr,n=n, H=H,method=method,C=C,
                                     dummy=DUM,Kmin=4)
              
            }#
            
            if (detrend==2){
              Y.co=Y.co+trnd.ts
            } # if detrend
            if (detrend==3){
              Y.co=Y.co*trnd.ts
            } 
            if (scaled)
              Y.co=Y.co* attr(sTS.tr, 'scaled:scale') + attr(sTS.tr, 'scaled:center')
            if (seas>0){
              allS<-c(TS.tr,Y.co)
              allS<-allS+rep(TSeas0,length.out=length(allS))
              Y.co=allS[(length(TS.tr)+1):length(allS)]
            }#
          } # else  
          
          
          Cont<-rbind(Cont,Y.co)
          
        } ## for rr    
        
        
      } # for mod
    }
   
    list(ts=r,Cont=Cont)
    
  } ## foreach
  for (f in 1:length(FF)){
    ts=FF[[f]]$ts
    TSERIES[[ts]]$ContM<-FF[[f]]$Cont
    TSERIES[[ts]]$cont<-apply(FF[[f]]$Cont,2,mean)
    TSERIES[[ts]]$sd<-apply(FF[[f]]$Cont,2,sd)
    
  }
  
  print(length(which(unlist(lapply(TSERIES,ex)))))
  
  if (savefile)
    save(file="TS.pred.Rdata",list=c("TSERIES"))
} # for s





