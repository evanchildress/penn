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
data<-data[species=="Brook Trout",list(siteId,siteSurveyId,lat,long,siteLength,siteWidth,species,sizeBin,count,recapCount,
                 pass,estimateType,date,month,year)]

suppressWarnings(data[,recapCount:=as.numeric(recapCount)])

#need to deal with cohort (or not)
data[,stage:=as.numeric(sizeBin>=0)]
data<-data[sizeBin>100]
data<-data[,.(count=sum(count,na.rm=T),
              recapCount=sum(recapCount,na.rm=T)),
           by=.(species,siteId,siteSurveyId,year,stage,siteLength,siteWidth,lat,long,date,month,year,pass,estimateType)] %>%
  .[,pass:=as.character(pass)]

data<-data %>% 
  .[estimateType=="Petersen M & R"&pass==2] %>%
  .[,":="(pass="recap",count=recapCount)] %>%
  bind_rows(data) %>%
  data.table() %>%
  .[,recapCount:=estimateType=="Petersen M & R"&pass==3] %>%
  setkey(siteSurveyId,pass)

data[,surveyIndex:=which(siteSurveyId[1]==unique(data$siteSurveyId)),by=siteSurveyId]
data[,siteIndex:=which(siteId[1]==unique(data$siteId)),by=siteId]
data[,yearIndex:=year-min(year)+1]

jagsData<-list(#data/survey info
               y=data$count,
               #stage=data$stage,
               year=data$year-min(data$year)+1,
               site=data$siteIndex,
               siteLength=data[,mean(siteLength)/100,by=surveyIndex]$V1,
               survey=data$surveyIndex,
               #control structures
               nSites=max(data$siteIndex),
               nYears=max(data$year-min(data$year)+1),
               nSurveys=max(data$surveyIndex),
               nRows=nrow(data),
               firstPassRows=which(data$pass=="1"),
               secondPassRows=which(data$pass=="2"),
               thirdPassRows=which(data$pass=="3"),
               fourthPassRows=which(data$pass=="4"),
               recapRows=which(data$pass=="recap"))

#higher level controls
jagsData$nFirstPassRows<-length(jagsData$firstPassRows)
jagsData$nSecondPassRows<-length(jagsData$secondPassRows)
jagsData$nThirdPassRows<-length(jagsData$thirdPassRows)
jagsData$nFourthPassRows<-length(jagsData$fourthPassRows)
jagsData$nRecapRows<-length(jagsData$nRecapRows)


