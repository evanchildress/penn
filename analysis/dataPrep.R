require(zoo)
require(getWBData)
load("~/lee/dataStore/cleanData/niles.RDATA")

siteData<-siteData[,":="(lat=mean(siteLatitude),
               long=mean(siteLongitude),
               siteWidth=siteAvgWidth),by=site] %>%
  .[,list(site,date,lat,long,siteLength,siteWidth)] %>%
  setkey(site,date) %>%
  .[is.na(siteLength),siteLength:=100]

fish<-fish %>% setkey(site,date) %>%
  siteData[.] %>%
  .[,siteId:=as.numeric(as.factor(site))]

binSizes<-Vectorize(function(x){
  bins<-seq(0,2000,25)
  max(bins[which(x>=bins)])})

fish<-fish[!is.na(length),sizeBin:=binSizes(length)] %>%
      .[!is.na(length),.(count=.N),by=.(site,date,lat,long,species,sizeBin,pass,siteId,siteLength,siteWidth)] %>%
      .[,siteSurveyId:=as.numeric(paste(siteId,year(date),sep="."))] %>%
      .[,":="(recapCount=NA,estimateType="Niles")] %>%
      .[,.(siteId,siteSurveyId,site,lat,long,siteLength,siteWidth,species,sizeBin,count,recapCount,pass,estimateType,date)] %>%
      setnames(c("siteId","siteSurveyId","siteName","lat","long","siteLength","siteWidth",
                "species","sizeBin","count","recapCount","pass","estimateType","date"))

data<-fread("data/qrystbtpopest.csv")
data[,date:=as.Date(SurveyDate,format="%m/%d/%Y")]
data[,Comname:=camelCase(Comname,sep=" "),by=Comname]

setnames(data,
         c("WaterSectionID","WaterSiteSurvey_ID","WaterName","SurveySiteLatDD","SurveySiteLonDD","SiteLength_m",
           "SiteWidth_m","Comname","GroupSize","EffortCatch","DeadOrMarkedCapture","EffortNumber",
           "EstimateType"),
         c("siteId","siteSurveyId","siteName","lat","long","siteLength","siteWidth",
           "species","sizeBin","count","recapCount","pass","estimateType"))
data<-data[species=="brookTrout",
           list(siteId,siteSurveyId,siteName,lat,long,siteLength,siteWidth,
                species,sizeBin,count,recapCount,
                 pass,estimateType,date)] %>%
      .[,siteSurveyId:=as.numeric(siteSurveyId)]

data<-data[!duplicated(data)] %>%
      setkey(siteSurveyId) %>%
      .[!duplicated(.[,list(siteId,siteName,lat,long,siteLength,siteWidth,species,sizeBin,count,recapCount,pass,estimateType,date)])]
data<-rbind(data,fish)

data[,year:=year(date)]
data[,month:=month(date)]
data[,siteName:=camelCase(siteName,sep=" "),by=siteName]

data[,meanSiteWidth:=mean(siteWidth,na.rm=T),by=siteId][
  is.na(siteWidth),siteWidth:=meanSiteWidth][
    ,meanSiteWidth:=NULL]
data<-data[!is.na(siteWidth)] %>%
      setkey(siteSurveyId)
suppressWarnings(data[,recapCount:=as.numeric(recapCount)])

precip<-readRDS("C:/Users/Evan/Desktop/Conte/lee/figures/allPrecip.rds")
getPrecip<-function(lat,long){
  precip$precip[which.min(abs(precip$lat-lat)+abs(precip$long-long))]
}

data[,leePrecip:=getPrecip(lat,long),by=.(lat,long)]
rm(precip)
rm(fish)

#subset to a single stage
if(stageSelect=="yoy"){
data<-data[sizeBin<=50]
} else if(stageSelect=="adult"){
  data<-data[sizeBin>50]
} else (warning("no stage selected, so all fish were included in the model"))
data<-data[,.(count=sum(count,na.rm=T),
              recapCount=sum(recapCount,na.rm=T)),
           by=.(species,siteId,siteSurveyId,siteName,year,siteLength,siteWidth,lat,long,date,month,year,pass,estimateType,leePrecip)] #%>%
  # .[,pass:=as.character(pass)]

data<-data %>% 
  filter(estimateType=="Petersen M & R"&pass==2) %>%
  mutate(pass=3,count=recapCount) %>%
  bind_rows(data) %>%
  data.table() %>%
  .[,recapCount:=estimateType=="Petersen M & R"&pass==3] %>%
  setkey(siteSurveyId,pass)

data[,surveyIndex:=which(siteSurveyId[1]==unique(data$siteSurveyId)),by=siteSurveyId]
data[,siteIndex:=which(siteId[1]==unique(data$siteId)),by=siteId]
data[,yearIndex:=year-min(year)+1]

nPasses<-data.table(estimateType=c("Zippen 3 Pass Removel","Jolly 2 Pass Removel","Petersen M & R","Zippen 4 Pass Removel","Niles"),
           nPasses=c(3,2,3,4,3))
data[,nPasses:=nPasses$nPasses[match(estimateType,nPasses$estimateType)]] %>%
  setkey(pass)

data<-data[,.SD[data.table(pass=1:nPasses[1],key="pass")],
     by=.(surveyIndex,species,siteId,siteName,siteSurveyId,year,siteLength,siteWidth,lat,
          long,date,month,estimateType,leePrecip,siteIndex,yearIndex)]
for(skip in 1:nrow(skippedPasses)){
  data<-data[siteName!=skippedPasses$site[skip]|
               date!=skippedPasses$date[skip]|
               pass!=skippedPasses$pass[skip]|
               estimateType!="Niles"]
}

recapsNoMarks<-data[is.na(count)&estimateType=="Petersen M & R",surveyIndex]
data[surveyIndex %in% recapsNoMarks,count:=c(count[3],count[2:3]),by=surveyIndex]

data[is.na(count),count:=0]
setkey(data,surveyIndex,pass)
 
nRolled=15
dailyDischarge<-readRDS("results/dailyMediansPa.rds") %>%
                .[,.(medianQ=mean(medianQ,na.rm=T)),by=date] %>%
                setkey(date) %>%
                .[,typicalFlow:=c(rep(NA,(nRolled-1)/2),rollmean(medianQ,nRolled),rep(NA,(nRolled-1)/2))] %>%
                .[c((366-(nRolled-2)):366,1:(nRolled-1)),
                  typicalFlow:=c(typicalFlow[1:((nRolled-1)/2)],
                                rollmean(medianQ,nRolled),
                                typicalFlow[((nRolled-1)/2*3+1):((nRolled-1)*2)])] %>%
                .[,typicalFlow:=scale(typicalFlow)[,1]]

jagsData<-list(#data/survey info
               y=data$count,
               year=data$year-min(data$year)+1,
               dayOfYear=as.numeric(format(data$date,"%j")),
               site=data$siteIndex,
               siteLength=data[,mean(siteLength)/100,by=surveyIndex]$V1,
               siteWidth=scale(data$siteWidth)[,1],
               survey=data$surveyIndex,
               typicalFlow=dailyDischarge$typicalFlow,
               leePrecip=scale(data[,mean(leePrecip),by=siteIndex]$V1)[,1],
               year2012=data[year==2012,unique(yearIndex)],
               #control structures
               nSites=max(data$siteIndex),
               nYears=max(data$year-min(data$year)+1),
               nSurveys=max(data$surveyIndex),
               nRows=nrow(data),
               firstPassRows=which(data$pass==1&data$estimateType!="Petersen M & R"),
               secondPassRows=which(data$pass==2&data$estimateType!="Petersen M & R"),
               thirdPassRows=which(data$pass==3&data$estimateType!="Petersen M & R"),
               fourthPassRows=which(data$pass==4&data$estimateType!="Petersen M & R"),
               unmarkedRows=which(data$pass==2&data$estimateType=="Petersen M & R"),
               recapRows=which(data$pass==3&data$estimateType=="Petersen M & R"),
               mrSites=data[estimateType=="Petersen M & R",unique(siteIndex)],
               depSites=data[estimateType!="Petersen M & R",unique(siteIndex)])

jagsData$depRows<-c(jagsData$firstPassRows,jagsData$secondPassRows,jagsData$thirdPassRows,
                   jagsData$fourthPassRows)
jagsData$mrRows<-c(jagsData$recapRows,jagsData$unmarkedRows)
#higher level controls
jagsData$nFirstPassRows<-length(jagsData$firstPassRows)
jagsData$nSecondPassRows<-length(jagsData$secondPassRows)
jagsData$nThirdPassRows<-length(jagsData$thirdPassRows)
jagsData$nFourthPassRows<-length(jagsData$fourthPassRows)
jagsData$nRecapRows<-length(jagsData$recapRows)
jagsData$nUnmarkedRows<-length(jagsData$unmarkedRows)
jagsData$nMrSites<-length(jagsData$mrSites)
jagsData$nDepSites<-length(jagsData$depSites)
jagsData$nMrRows<-length(jagsData$mrRows)
jagsData$nDepRows<-length(jagsData$depRows)

