cat("model{
#abundance model
  #likelihood
  for(i in 1:nSurveys){
      N[i] ~ dpois(lambda[i])
      log(lambda[i]) <- mu + siteRan[site[i]] + yearRan[year[i]] + eps[site[i],year[i]] + log(siteLength[i])
  }
  
  #priors
  mu ~ dnorm(0,0.01)
  
  siteTau<-1/pow(siteSigma,-2)
  siteSigma ~ dunif(0,10)
  
  yearTau<-1/pow(yearSigma,-2)
  yearSigma ~ dunif(0,10)
  
  epsTau<-1/pow(epsSigma,-2)
  epsSigma ~ dunif(0,10)
  
  for(t in 1:nYears){
    yearRan[t]~dnorm(0,yearTau)
  }
  for(s in 1:nSites){
    siteRan[s]~dnorm(0,siteTau)
  }
  for(t in 1:nYears){
    for(s in 1:nSites){
      eps[s,t]~dnorm(0,epsTau)
    }
  }
  
#observation model
  #likelihood
  for(i in 1:nFirstPassRows){
    y[firstPassRows[i]]~dbin(p[firstPassRows[i]],N[survey[firstPassRows[i]]])
  }
  for(i in 1:nSecondPassRows){
    y[secondPassRows[i]]~dbin(p[secondPassRows[i]],N[survey[secondPassRows[i]]]-y[secondPassRows[i]-1])
  }
  for(i in 1:nThirdPassRows){
    y[thirdPassRows[i]]~dbin(p[thirdPassRows[i]],N[survey[thirdPassRows[i]]]-
                                                    y[thirdPassRows[i]-1]-
                                                    y[thirdPassRows[i]-2])
  }
  for(i in 1:nFourthPassRows){
    y[fourthPassRows[i]]~dbin(p[fourthPassRows[i]],N[survey[fourthPassRows[i]]]-
                                                      y[fourthPassRows[i]-1]-
                                                      y[fourthPassRows[i]-2]-
                                                      y[fourthPassRows[i]-3])
  }
  
  for(i in 1:nRecapRows){
    y[recapRows[i]]~dbin(p[recapRows[i]],y[recapRows[i]-2]) #1st pass captures are two rows above recaps
  }
  
  for(i in 1:nRows){
    logitP[i]<-muP + pEps[site[i]]
    p[i]<-1/(1+exp(-logitP[i]))
  }
  
  #priors
  muP~dnorm(0,0.01)
  pTau<-1/pow(pSigma,-2)
  pSigma ~ dunif(0,10)
  for(s in 1:nSites){
    pEps[s]~dnorm(0,pTau)
  }
}",file="analysis/model.txt")
