data<-fread("data/qrystbtpopest.csv")
data[,date:=as.Date(SurveyDate,format="%m/%d/%Y")]
data[,year:=year(date)]
data[,month:=month(date)]

setnames(data,
         c("WaterSectionID","WaterSiteSurvey_ID","SurveySiteLatDD","SurveySiteLonDD","SiteLength_m",
           "SiteWidth_m","Comname","GroupSize","EffortCatch","DeadOrMarkedCapture","EffortNumber",
           "EstimateType"),
         c("siteId","siteSurveyId","lat","long","siteLength","siteWidth",
           "species","sizeBin","count","recapCount","pass","estimateType"))
data<-data[species=="Brook Trout",
           list(siteId,siteSurveyId,lat,long,siteLength,siteWidth,
                species,sizeBin,count,recapCount,
                 pass,estimateType,date,month,year)]

data[,meanSiteWidth:=mean(siteWidth,na.rm=T),by=siteId][
  is.na(siteWidth),siteWidth:=meanSiteWidth][
    ,meanSiteWidth:=NULL]
data<-data[!is.na(siteWidth)]

suppressWarnings(data[,recapCount:=as.numeric(recapCount)])

#need to deal with cohort (or not)
data[,stage:=as.numeric(sizeBin>=0)]
data<-data[sizeBin<100]
data<-data[,.(count=sum(count,na.rm=T),
              recapCount=sum(recapCount,na.rm=T)),
           by=.(species,siteId,siteSurveyId,year,stage,siteLength,siteWidth,lat,long,date,month,year,pass,estimateType)] %>%
  .[,pass:=as.character(pass)]

data<-data %>% 
  filter(estimateType=="Petersen M & R"&pass==2) %>%
  mutate(pass="recap",count=recapCount) %>%
  bind_rows(data) %>%
  data.table() %>%
  .[,recapCount:=estimateType=="Petersen M & R"&pass==3] %>%
  setkey(siteSurveyId,pass)

data[,surveyIndex:=which(siteSurveyId[1]==unique(data$siteSurveyId)),by=siteSurveyId]
data[,siteIndex:=which(siteId[1]==unique(data$siteId)),by=siteId]
data[,yearIndex:=year-min(year)+1]


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
               #stage=data$stage,
               year=data$year-min(data$year)+1,
               dayOfYear=as.numeric(format(data$date,"%j")),
               site=data$siteIndex,
               siteLength=data[,mean(siteLength)/100,by=surveyIndex]$V1,
               siteWidth=data$siteWidth,
               survey=data$surveyIndex,
               typicalFlow=dailyDischarge$typicalFlow,
               #control structures
               nSites=max(data$siteIndex),
               nYears=max(data$year-min(data$year)+1),
               nSurveys=max(data$surveyIndex),
               nRows=nrow(data),
               firstPassRows=which(data$pass=="1"&data$estimateType!="Petersen M & R"),
               secondPassRows=which(data$pass=="2"&data$estimateType!="Petersen M & R"),
               thirdPassRows=which(data$pass=="3"&data$estimateType!="Petersen M & R"),
               fourthPassRows=which(data$pass=="4"&data$estimateType!="Petersen M & R"),
               unmarkedRows=which(data$pass=="2"&data$estimateType=="Petersen M & R"),
               recapRows=which(data$pass=="recap"),
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

