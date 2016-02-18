library(data.table)
library(dplyr)
library(reshape2)
library(R2jags)

source("analysis/dataPrep.R")
source("analysis/model.R")


params<-c("pSigma","muP","yearSigma","siteSigma","mu","epsSigma","eps","yearRan","siteRan")

ni=10000
nb=8000
nt=5
nc=3

inits<-function(){list(N=data[,sum(count),by=surveyIndex]$V1)}

jags(jagsData,inits=inits,params,"analysis/model.txt",nc,ni,nb,nt)
