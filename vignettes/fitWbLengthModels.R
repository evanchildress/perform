
library(perform)
# library(dplyr)
# library(data.table)
# library(reshape2)
# library(jagsUI)
# library(getWBData)

rivers<-c("wb jimmy","wb mitchell","wb obear","west brook")
#rivers<-c("wb obear")
#rivers<-"wb jimmy"
for(r in rivers){
  #if(r %in% c("wb mitchell","wb obear")){next}
  core<-createCoreData("electrofishing") %>%
    addTagProperties() %>%
    filter(species=="bkt" & !is.na(observedLength) & river==r) %>%
    createCmrData() %>%
    addKnownZ() %>%
    filter(knownZ==1) %>%
    addSampleProperties() %>%
    data.frame() %>%
    data.table() %>%
    .[,nObs:=sum(!is.na(observedLength)),by=tag] %>%
    .[nObs>1] %>%
    .[,nObs:=NULL] %>%
    .[,tagIndex:=match(tag,unique(tag))]

  jagsData<-createJagsData(data.frame(core) %>% addEnvironmental()) %>%
    .[c("firstObsRows",
        "nFirstObsRows",
        "evalRows",
        "nEvalRows",
        "nAllRows",
        "lengthDATA")]

  core<-core[,.(tag,tagIndex,detectionDate,observedLength)]

  temp<-tbl(conDplyr,"data_hourly_temperature") %>%
    filter(river==r) %>%
    collect() %>%
    data.table() %>%
    mutate(date=as.Date(datetime)) %>%
    .[date>=min(gr$startDate)&date<=max(gr$endDate)] %>%
    setkey(datetime)

  core[,':='(time=which.min(
    abs(
      temp$datetime-as.POSIXct(paste0(detectionDate," 12:00:00"))
    )
  )),
  by=.(tag,detectionDate)]

  temp[,month:=round((month(date)+0.1)/2)]
  hoursPerMonth<-temp[date>=as.Date("2003-01-01")&date<=as.Date("2014-12-31"),
                      .(hours=.N/length(unique(year(date)))),
                      by=month] %>%
    setkey(month)

  propMonth<-core[,.(tag,time)] %>%
    .[,startTime:=as.numeric(shift(time)),by=tag] %>%
    .[,obs:=1:nrow(.)] %>%
    .[is.na(startTime),startTime:=time-1]

  propMonth<-propMonth[!is.na(startTime),temp[startTime:time,.N,by=month] %>%
                         setkey(month) %>%
                         .[hoursPerMonth] %>%
                         .[is.na(N),N:=0] %>%
                         .[,propMonth:=N/hours] %>%
                         .[,.(propMonth,month)],
                       by=.(obs)] %>%
    melt(id.vars=c("month","obs")) %>%
    acast(obs~month)

  jagsData$propMonth<-propMonth
  jagsData$tempDATA<-temp$temperature
  jagsData$nTimes<-nrow(temp)
  jagsData$time<-core$time

  createModelRandom()
  inits<-function(){list(ctMax=rnorm(1,20,0.5),
                         tOpt=rnorm(1,9,0.5),
                         beta=c(0.015,-6e-05))
  }

  out<-fitModel(jagsData=jagsData,inits=inits,parallel=T,nb=10000,ni=12000,nt=2)
  saveRDS(out,file=paste0("out",toupper(substr(r,1,1)),substr(r,2,nchar(r)),".rds"))
  assign(paste0("out",which(r==rivers)),out)
}

for(r in rivers){
  out<-get(paste0("out",which(r==rivers)))
  plot(NA,xlim=c(0,22),ylim=c(-2,1),main=r,xlab="temp",ylab="performance")
  for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
    points(predictPerformance(0:22,tOpt=out$sims.list$tOpt[i],
                              sigma=out$sims.list$sigma[i],
                              ctMax=out$sims.list$ctMax[i])~I(0:22),
           type='l',col='gray')
  }


  plot(mmPerDay~startLength,data=get(paste0("gr",which(r==rivers))),
       col=gray(0.5,0.5),pch=19,main=r,xlab="startLength",ylab="growth (mm/day)")
  x<-seq(60,300)
  for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
    points(predictVonBert(x,
                          out$sims.list$beta[i,1],
                          out$sims.list$beta[i,2],
                          derivative=T)*24~x,
           type='l',col='blue')
  }



  predicted<-apply(out$sims.list$gr,2,mean)
  plot(predicted~get(paste0("gr",which(r==rivers)))$growth,main=r,
       ylab="predicted growth",xlab="observed growth")
  abline(0,1)
}

