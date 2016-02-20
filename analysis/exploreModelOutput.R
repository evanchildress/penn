library(jagstools)
library(ape)
jagsresults(out,params=c("muPDep","muPMr","pMrSigma","pDepSigma","mu","siteSigma","yearSigma","epsSigma"))

sites<-data[,.(lat=mean(lat),
               long=mean(long),
               siteWidth=mean(siteWidth,na.rm=T),
               nYears=length(unique(year))),
            by=.(siteId,siteIndex)]

repeatSites<-which(sites$nYears>1)

sims<-out$BUGSoutput$sims.list
siteRan<-apply(sims$siteRan,2,mean)
yearRan<-apply(sims$yearRan,2,mean)
eps<-apply(sims$eps,c(2,3),mean)

logLambda<-array(NA,dim=dim(eps))
for(y in 1:length(yearRan)){
  logLambda[,y]<-mean(sims$mu)+siteRan+yearRan[y]+eps[,y]
}

plot(yearRan~I((1:40)+1974),type='l')

colors<-c(rgb(0,0,1,0.6),gray(0.2,0.5),rgb(1,0,0,0.6))
whichColors<-1+ as.numeric(round(siteRan,3)>-0.001) + as.numeric(siteRan>0)
plot(lat~long,data=sites[repeatSites],col=colors[whichColors],
     cex=log(abs(siteRan[repeatSites])+1)*10,pch=19)

for(y in 1:length(yearRan)){
  whichColors<-1+ as.numeric(round(eps[,y],3)>-0.001) + as.numeric(eps[,y]>0)
  plot(lat~long,data=sites[repeatSites],col=colors[whichColors],
       cex=log(abs(eps[repeatSites,y])+1)*5,pch=19,main=y+1974)
}

distMat<-as.matrix(dist(cbind(sites$long,sites$lat)))
invDist<-1/distMat
diag(invDist)<-0
siteMoran<-Moran.I(siteRan[repeatSites],invDist[repeatSites,repeatSites])


epsMoran<-NA
epsMoranP<-NA
for(y in 1:length(yearRan)){
  moran<-Moran.I(eps[,y],invDist)
  epsMoran[y]<-moran$observed
  epsMoranP[y]<-moran$p.value
}

plot(epsMoran~I(1:length(yearRan)+1974),type='l')
points(epsMoran[which(epsMoranP<0.05)]~
         I(1:length(yearRan)+1974)[which(epsMoranP<0.05)])

# for(y in 1:length(yearRan)){
#   plot(lat~long,data=sites,pch=19,col=gray(0.8,0.5),cex=exp(logLambda[,y])/50,
#        main=y+1974)
# }
