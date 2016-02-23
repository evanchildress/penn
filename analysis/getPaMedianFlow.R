library(waterData)

paSites<-read.table("data/usgsPaSites.txt",skip=24,
                    stringsAsFactors=F,sep="\t",
                    colClasses="character")[,2]
daily<-NULL
pb<-txtProgressBar(min=0,max=length(paSites))
for(s in paSites){
  try(q<-importDVs(s,code="00060",stat="00003",sdate="1900-12-01",
            edate="2015-12-31") %>% data.table(),silent=T)
  if(is.null(q$staid)) next
  q[,date:=as.numeric(format(dates,"%j"))]
  nYears<-q[,length(unique(year(dates)))]
  q[val<0,val:=NA]
  q<-q[,.(medianQ=median(val,na.rm=T)),by=date]
  q[,medianQ:=scale(medianQ)[,1]]
  q[,":="(site=s,nYears=nYears)]
  daily<-rbind(daily,q)
  setProgressBar(pb,which(s==paSites))
}

saveRDS(daily,"results/dailyMediansPa.rds")

dailyMean<-daily[,.(meanMedianQ=mean(medianQ,na.rm=T),
                    medianMedianQ=median(medianQ,na.rm=T)),by=date]