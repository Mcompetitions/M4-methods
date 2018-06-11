## Script returning for each series the configuration of the best 5 models in terms of NMSE and SMAPE
## For each of the stored model the parameters are 
## 1 method: learning strategy (see variable Methods)
## 2 order: embedding order
## 3 detrend: detrending strategy (1 no detrending, 2 linear detrend)
#  4 diff: differentiation strategy
## 5 C: hyperparameter of the learning strategy
## 6 dummy: dummy strategy
## 7 cost: in terms of NMSE or sMAPE normalized with respect to the one of naive strategy

## Note that a multicore computer is required (foreach) given the high computational cost of this selection procedure

rm(list=ls())
library(devtools)
install_github("gbonte/gbcode")
source("library.R")
library(gbcode) 
library(foreach)
library(doParallel)
library(foreach)
cl <- makeForkCluster(5)
registerDoParallel(cl)

sMAPE<-function(Yts,Yhat){
  H<-length(Yts)
  e=abs(Yts-Yhat)
  den=abs(Yts)+abs(Yhat)
  return(sum(2*e/den)/H)
}

cMASE<-function(Yts,Yhat,Yt,m=12){
  H<-length(Yts)
  e=abs(Yts-Yhat)
  n=length(Yt)
  etr=NULL
  for (t in seq((m+1),n,by=1))
    etr=c(etr,abs(Yt[t]-Yt[t-m]))
  return((sum(e)/(0.0001+sum(etr)/(n-m)))/H)
  
}



load("TSERIES.Rdata")
savefile=TRUE
allFF<-NULL 
## data structure containing for each time series the parameters of the 5 best models in terms of NMSE and 
## the best 5 models in terms of SMAPE

s0<-100

start_time <- Sys.time()
NTS=100000

## list of methods taken into consideration by the selection process
Methods=c("liniter","lindirect","rfiter",
          "rfdirect","stats","lazyiter","lazydirect",
          "mimo.comb","timefit")


SS<-seq(s0,NTS,by=5)
#SS<-c(10000,15000,20000,25000,30000,40000)
for (s in 1:(length(SS)-1)){
  
  print(SS[s])
  
  FF<-foreach(r=(SS[s]+1):SS[s+1]) %dopar% {
  ##  for (r in (SS[s]+1):SS[s+1]) {
    TS<-TSERIES[[r]]$ts
    N<-length(TS)
    TS<-array(TS,c(N,1))
    Methods2=Methods
    if (N>200 & r > 95000)
      Methods2[c(3,4)]="NA"
    H<-TSERIES[[r]]$H ## horizon prediction
    
      maxdummy=4
      maxn=min(4,max(1,floor(N/10)))
      maxC=4
    
    ## method, order, detrend, diff,C,dummy
    NMSE=array(Inf,c(length(Methods),maxn,2,2,maxC,maxdummy))
    SMA=array(Inf,c(length(Methods),maxn,2,2,maxC,maxdummy))
    MAS=array(Inf,c(length(Methods),maxn,2,2,maxC,maxdummy))
    cat("Series=",r,":",TSERIES[[r]]$name, "N=", N, "H=", H, "\n" )
    
    typeser=substr(TSERIES[[r]]$name,1,1)
    if (typeser=="Y"){
      dummylength=2
      fr=1
    }
    if (typeser=="M"){
      dummylength=12
      fr=12
    }
    if (typeser=="Q"){
      dummylength=4
      fr=4
    }
    if (typeser=="D"){
      dummylength=7
      fr=1
    }
    if (typeser=="W"){
      dummylength=4
      fr=1
    }
    if (typeser=="H"){
      dummylength=24
      fr=24
    }
    
    
    
    cnt.tr<-0
    seqtr=seq(max(round(N/2),N-H-5),N-H,by=1)
    if (N<20)
      seqtr=N-H
    for (N.tr in seqtr){
      cnt.tr<-cnt.tr+1
      for (dummy in seq(1,maxdummy)){
        for (diff in c(1,2)){ ## diff:1 no differentiation
          for (detrend in c(1,2)){  ## detrend: 1 no detrending, 2 linear, 3 geometric
            TS.tr=TS[1:N.tr,1]
            TS.ts<-TS[(N.tr+1):(N.tr+H),1]                                       
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
            
            
            
            TS.tr=array(TS.tr,c(length(TS.tr),1))
            
            Y.cont<-array(NA,c(length(Methods),maxn,H))
            
            for (n in seq(1,maxn,by=1)){
              for (C in seq(1,max(2,maxC))){
                cnt<-0 ## counter of method
                for (method in Methods2){
                  cnt<-cnt+1
                  if (method!="NA"){
                    
                    if (diff>1){
                      if (method=="stats"){
                        DY.cont=StatPredictors1(c(TS.tr),H,(n-1)*maxC+C+(dummy-1)*maxn*maxC)
                        #                print((n-1)*maxC+C+(dummy-1)*maxn*maxC)
                      } else{
                        
                        if (dummy==1)
                          DUM=-2
                        if (dummy==2)
                          DUM=-1
                        if (dummy==3)
                          DUM=0
                        if (dummy >3)
                          DUM=dummylength
                        DY.cont=multiplestepAhead(TS.tr,n=n, H=H,method=method,C=C,dummy=DUM,Kmin=4)
                        
                      }#
                      if (detrend==2)
                        DY.cont=DY.cont+trnd.ts
                      if (detrend==3)
                        DY.cont=DY.cont*trnd.ts
                      if (scaled)
                        DY.cont=DY.cont* attr(sTS.tr, 'scaled:scale') + attr(sTS.tr, 'scaled:center')    
                      
                      Y.co=differB(DY.cont,DTS.tr$last)
                    } else {  ## if diff>1
                      if (method=="stats" ){
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
                        Y.co=multiplestepAhead(TS.tr,n=n, H=H,method=method,C=C,dummy=DUM,Kmin=4)
                        
                      }#
                      
                      if (detrend==2){
                        Y.co=Y.co+trnd.ts
                      } # if detrend
                      if (detrend==3){
                        Y.co=Y.co*trnd.ts
                      } 
                      if (scaled)
                        Y.co=Y.co* attr(sTS.tr, 'scaled:scale') + attr(sTS.tr, 'scaled:center')
                      
                    } # else  
                    
                    Y.cont[cnt,n,]=Y.co
                    e=TS.ts-Y.cont[cnt,n,]
                    Yn=rep(TS[N.tr],H)
                    enaive=TS.ts-Yn
                    NMS=mean(e^2)
                    NMSn=(1e-4+mean(enaive^2))
                    SMAPE=sMAPE(TS.ts,Y.co)
                    SMAPEn=sMAPE(TS.ts,Yn)+1e-4
                    MASE=cMASE(TS.ts,Y.co,TS.tr,fr)
                    MASEn=cMASE(TS.ts,Yn,TS.tr,fr)+1e-4
                    
                    if (is.infinite(NMSE[cnt,n,detrend,diff,C,dummy])){
                      
                      NMSE[cnt,n,detrend,diff,C,dummy]=NMS/NMSn # SMAPE/SMAPEn
                      SMA[cnt,n,detrend,diff,C,dummy]=SMAPE/SMAPEn
                      MAS[cnt,n,detrend,diff,C,dummy]=MASE/MASEn
                    }  else {
                      
                      NMSE[cnt,n,detrend,diff,C,dummy]=NMSE[cnt,n,detrend,diff,C,dummy]+NMS/NMSn
                      SMA[cnt,n,detrend,diff,C,dummy]=SMA[cnt,n,detrend,diff,C,dummy]+SMAPE/SMAPEn
                      MAS[cnt,n,detrend,diff,C,dummy]=MAS[cnt,n,detrend,diff,C,dummy]+MASE/MASEn
                    } # else
                    
                    
                    
                    if (any(is.nan(NMSE)))
                      stop("NAN NMSE")
                  } ## method == NA
                } # for method
                cat("*")
              }# for C
              
            } # for n
            
          }# for detrend
        }# for diff
        cat(".")
      } # for dummy
      print(N.tr)      
    }## for N.tr
    NMSE=NMSE/cnt.tr
    SMA=SMA/cnt.tr
    MAS=MAS/cnt.tr
    
    COST=(SMA+MAS)/2
    
    
    mNMSE=min(NMSE)
    mSM=min(SMA)
    mMAS=min(MAS)
    mCOST=min(COST)
    
    inds = which(NMSE == min(NMSE), arr.ind=TRUE)[1,]  
    ## method, order, detrend, diff,C,dummy
    NMSE[inds[1],inds[2],inds[3],inds[4],inds[5],inds[6]]=Inf
    
    mNMSE2=min(NMSE)
    inds2 = which(NMSE == min(NMSE), arr.ind=TRUE)[1,]
    NMSE[inds2[1],inds2[2],inds2[3],inds2[4],inds2[5],inds2[6]]=Inf
    
    
    mNMSE3=min(NMSE)
    inds3 = which(NMSE == min(NMSE), arr.ind=TRUE)[1,]
    NMSE[inds3[1],inds3[2],inds3[3],inds3[4],inds3[5],inds3[6]]=Inf
    
    mNMSE4=min(NMSE)
    inds4 = which(NMSE == min(NMSE), arr.ind=TRUE)[1,]
    NMSE[inds4[1],inds4[2],inds4[3],inds4[4],inds4[5],inds4[6]]=Inf
    
    mNMSE5=min(NMSE)
    inds5 = which(NMSE == min(NMSE), arr.ind=TRUE)[1,]
    
    
    
    inds.C = which(COST == min(COST), arr.ind=TRUE)[1,]  
    ## method, order, detrend, diff,C,dummy
    COST[inds.C[1],inds.C[2],inds.C[3],inds.C[4],inds.C[5],inds.C[6]]=Inf
    
    mCOST2=min(COST)
    inds2.C = which(COST == min(COST), arr.ind=TRUE)[1,]
    COST[inds2.C[1],inds2.C[2],inds2.C[3],inds2.C[4],inds2.C[5],inds2.C[6]]=Inf
    
    mCOST3=min(COST)
    inds3.C = which(COST == min(COST), arr.ind=TRUE)[1,]
    COST[inds3.C[1],inds3.C[2],inds3.C[3],inds3.C[4],inds3.C[5],inds3.C[6]]=Inf
    
    mCOST4=min(COST)
    inds4.C = which(COST == min(COST), arr.ind=TRUE)[1,]
    COST[inds4.C[1],inds4.C[2],inds4.C[3],inds4.C[4],inds4.C[5],inds4.C[6]]=Inf
    
    mCOST5=min(COST)
    inds5.C = which(COST == min(COST), arr.ind=TRUE)[1,]
    
    
    
    cat(Methods[inds[1]],":",mNMSE,mSM,mMAS,"\n")
    log=c(r,N,inds,mNMSE,inds2,mNMSE2,inds3,mNMSE3,inds4,mNMSE4,inds5,mNMSE5,
          inds.C,mCOST,inds2.C,mCOST2,inds3.C,mCOST3, inds4.C,mCOST4,inds5.C,mCOST5)
    
    log
  }# foreach
  allFF<-rbind(allFF,matrix(unlist(FF), ncol = 72, byrow = TRUE)) # 2+6*5+5 + 6*5+5
  print(dim(allFF))
  print(summary(allFF[,9]))
  nf=SS[s]
  if (savefile)
    save(file="calibration.all.Rdata",list=c("allFF","s","nf","Methods"))
} # for s



stopCluster(cl)

