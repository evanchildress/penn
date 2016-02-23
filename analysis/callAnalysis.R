library(zoo)
library(data.table)
library(dplyr)
library(reshape2)
library(jagsUI)


source("analysis/dataPrep.R")
source("analysis/model.R")


params<-c("pMrSigma","pDepSigma","muPMr","muPDep","pMrEps","pDepEps","pMrBeta","pDepBeta",
          "yearSigma","siteSigma","mu","epsSigma","eps","yearRan","siteRan")

ni=20000
nb=18000
nt=5
nc=3
na=2000

inits<-function(){list(N=data[,sum(count),by=surveyIndex]$V1)}
start<-Sys.time()
out<-jags(jagsData,inits=inits,params,"analysis/model.txt",
          n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt,parallel=T)
saveRDS(out,"results/out.rds")
finish<-Sys.time()
print(finish-start)