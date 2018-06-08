

colnames=c("id","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10",
           "F11","F12","F13","F14","F15","F16","F17","F18","F19","F20",
           "F21","F22","F23","F24","F25","F26","F27","F28","F29","F30",
           "F31","F32","F33","F34","F35","F36","F37","F38","F39","F40","F41","F42","F43",
           "F44","F45","F46","F47","F48")
load("calibration.all.Rdata")
load("TS.pred.Rdata")

if (FALSE){
    Pred=array(NA,c(length(TS),49))
    
    for (i in 1:length(TS)){
        Pred[i,1]=TS[[i]]$name
        print(allFF[i,9])
        browser()
        if (all(TS[[i]]$ts>0))
            TS[[i]]$ContM<-pmax(TS[[i]]$ContM,0)
        H=TS[[i]]$H
        if (NCOL(TS[[i]]$ContM)!=H)
            stop("Error")
        Pred[i,2:(H+1)]=apply(TS[[i]]$ContM,2,median)
        if (i%%5000==0)
            print(i)
    }

    write.table(Pred,"Pred.csv",col.names=colnames,row.names=FALSE,sep=",",quote=FALSE)
    print("saved Pred")

}


Lower=array(NA,c(length(TS),49))

for (i in 1:length(TS)){
    NMSEi=allFF[i,9]
    L<-length(TS[[i]]$ts)
    H<-TS[[i]]$H
    nH=2*H
    varn=var(TS[[i]]$ts[max(L-nH,1):L]) ## variance of the navie predictor
    diffusion=apply(TS[[i]]$ContM,2,var)
    diffusion=diffusion/mean(diffusion)
    sdi=sqrt(varn*NMSEi*diffusion)
    
    
    Lower[i,1]=TS[[i]]$name
    if (all(TS[[i]]$ts>0))
        TS[[i]]$ContM<-pmax(TS[[i]]$ContM,0)
    H=TS[[i]]$H
    Lower[i,2:(H+1)]=pmax(apply(TS[[i]]$ContM,2,quantile,0.5)-1.96*sdi,0)
    if (i%%5000==0)
        print(i)
    }

write.table(Lower,"Lower.last.csv",col.names=colnames,row.names=FALSE,sep=",",quote=FALSE)
print("saved Lower")

Upper=array(NA,c(length(TS),49))

for (i in 1:length(TS)){
    NMSEi=allFF[i,9]
    L<-length(TS[[i]]$ts)
    H<-TS[[i]]$H
    nH=2*H
    
    varn=var(TS[[i]]$ts[max(L-nH,1):L]) ## variance of the navie predictor
    diffusion=apply(TS[[i]]$ContM,2,var)
    diffusion=diffusion/mean(diffusion)
    sdi=sqrt(varn*NMSEi*diffusion)
    
    
    Upper[i,1]=TS[[i]]$name
    if (all(TS[[i]]$ts>0))
        TS[[i]]$ContM<-pmax(TS[[i]]$ContM,0)
    H=TS[[i]]$H
    Upper[i,2:(H+1)]=apply(TS[[i]]$ContM,2,quantile,0.5)+1.96*sdi
    if (i%%5000==0)
        print(i)
    }

write.table(Upper,"Upper.last.csv",col.names=colnames,row.names=FALSE,sep=",",quote=FALSE)
print("saved Upper")


