library(zoo)
library(data.table)
library(dplyr)
library(reshape2)
library(jagsUI)
stageSelect="none"

source("analysis/dataPrep.R")
source("analysis/model.R")


params<-c("pMrSigma","pDepSigma","muPMr","muPDep","pMrEps","pDepEps",
          "pMrBetaWidth","pDepBetaWidth","pMrBetaFlow","pDepBetaFlow",
          "yearSigma","siteSigma","mu","epsSigma","eps","yearRan","siteRan")
codaOnly<-c("pMrEps","pDepEps","yearRan","siteRan","eps")
ni=14000
nb=12000
nt=5
nc=3
na=2000

inits<-function(){list(N=data[,sum(count),by=surveyIndex]$V1)}
start<-Sys.time()
out<-jags(jagsData,inits=inits,params,"analysis/model.txt",
          n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt,parallel=T,
          codaOnly=codaOnly)
saveRDS(out,"results/out.rds")
finish<-Sys.time()
print(finish-start)