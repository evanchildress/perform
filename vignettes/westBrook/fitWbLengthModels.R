library(perform)
reconnect()
rivers<-c("wb jimmy","wb mitchell","wb obear")
#r<-"wb jimmy"
for(r in rivers){
  core<-wbLengths%>%
    data.frame() %>%
    fillSizeLocation(size=F) %>%
    filter(river==r) %>%
    data.table() %>%
    .[,diffSample:=c(NA,diff(sampleIndex)),by=tag]
  movers<-unique(core[diffSample>1,tag])
  for(t in movers){
    core[tag==t&
           sampleIndex %in%
           sampleIndex[min(which(diffSample>1)):length(sampleIndex)],
         observedLength:=NA]
  }
  core[,diffSample:=NULL]
  core<-core[,nMeasures:=sum(!is.na(observedLength)),by=tag] %>%
        .[nMeasures>1] %>%
        .[,nMeasures:=NULL] %>%
        .[,lastObs:=detectionDate==max(detectionDate),by=tag] %>%
        .[!lastObs|!is.na(observedLength)] %>%
        .[,lastObs:=NULL]
  core[,tagIndex:=match(tag,unique(tag))]
  #core<-core[!tag %in% c("00088d1ac1","00088d2f6f")]
    # .[,tagIndex:=match(tag,unique(tag))] %>%
    # setkey(tagIndex,detectionDate) %>%
    # .[,firstObs:=detectionDate==min(detectionDate),by=tag]

  jagsData<-createJagsData(data.frame(core) %>% addEnvironmental()) %>%
    .[c("firstObsRows",
        "nFirstObsRows",
        "evalRows",
        "nEvalRows",
        "nAllRows",
        "lengthDATA",
        "flowDATA",
        "ind",
        "season")]

  core<-core[,.(tag,tagIndex,detectionDate,observedLength)]

  t<-temp %>%
    .[river==r] %>%
    .[,date:=as.Date(datetime)]%>%
    .[datetime>=min(core$detectionDate)&datetime<=max(core$detectionDate)] %>%
    setkey(datetime)

  core[,':='(time=which.min(
    abs(
      t$datetime-as.POSIXct(paste0(detectionDate," 12:00:00"))
    )
  )),
  by=.(tag,detectionDate)]

  t[,month:=ceiling((month(date))/3)]
  hoursPerMonth<-t[date>=as.Date("2003-01-01")&date<=as.Date("2014-12-31"),
                      .(hours=.N/length(unique(year(date)))),
                      by=month] %>%
    setkey(month)

  propMonth<-core[,.(tag,time)] %>%
    .[,startTime:=as.numeric(shift(time)),by=tag] %>%
    .[,obs:=1:nrow(.)] %>%
    .[is.na(startTime),startTime:=time-1]

  propMonth<-propMonth[!is.na(startTime),t[startTime:time,.N,by=month] %>%
                         setkey(month) %>%
                         .[hoursPerMonth] %>%
                         .[is.na(N),N:=0] %>%
                         .[,propMonth:=N/hours] %>%
                         .[,.(propMonth,month)],
                       by=.(obs)] %>%
    melt(id.vars=c("month","obs")) %>%
    acast(obs~month)
#
#   jagsData$time<-core$time<-list(lengthDATA=core$observedLength,
#                  firstObsRows=which(core$firstObs==1),
#                  nFirstObsRows=length(which(core$firstObs==1)),
#                  evalRows=which(core$firstObs==0),
#                  nEvalRows=length(which(core$firstObs==0)),
#                  propMonth=propMonth,
#                  tempDATA=t$temperature,
#                  nTimes=nrow(t),
#                  time=core$time,
#                  nMonths=max(hoursPerMonth$month)
#   )
  jagsData$propMonth<-propMonth
  jagsData$nMonths<-max(hoursPerMonth$month)
  jagsData$tempDATA<-t$temperature
  jagsData$nTimes<-nrow(t)
  jagsData$time<-core$time
  jagsData$nInd<-max(core$tagIndex)
  jagsData$isSpring<-as.numeric(jagsData$season==2)


  core[,lengthInit:=approx(observedLength,n=length(observedLength))$y,by=tag]
  core[!is.na(observedLength),lengthInit:=NA]

  inits<-function(){list(lengthData=core$lengthInit,
                         maxAdd=18,
                         tOpt=12.5,
                         beta1=0.015,
                         beta2=-6e-05,
                         eps=0.003)
  }

  parsToMonitor<-c("beta1","beta2","beta3","beta4","tOpt",'ctMax',"sigma",
                   'eps',"sigmaInd","lengthExp")
  out<-fitModel(jagsData=jagsData,inits=inits,parallel=T,params=parsToMonitor,
                nb=5000,ni=7000,nt=1,modelFile="modelLengthField.R",codaOnly="lengthExp")
  saveRDS(out,file=paste0("out",toupper(substr(r,1,1)),substr(r,2,nchar(r)),".rds"))
  assign(paste0("out",which(r==rivers)),out)
  assign(paste0("core",which(r==rivers)),core)
  # core[jagsData$evalRows,predictedLength:=apply(out$sims.list$lengthExp,2,mean)]
  # core[,residual:=observedLength-predictedLength]
 }




 for(r in rivers){
  out<-get(paste0("out",which(r==rivers)))
  plot(NA,xlim=c(0,22),ylim=c(-1,1),main=r,xlab="temp",ylab="performance")
  for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
    points(predictPerformance(0:22,tOpt=out$sims.list$tOpt[i],
                              sigma=out$sims.list$sigma[i],
                              ctMax=out$sims.list$ctMax[i])~I(0:22),
           type='l',col='gray')
  }

core<-get(paste0("core",which(r==rivers)))
core$dummy<-1
core[,firstObs:=detectionDate==min(detectionDate),by=tag] %>%
      .[firstObs==F,predictedLength:=apply(out$sims.list$lengthExp,2,mean)]

assign(paste0("gr",which(r==rivers)),
       core[,.(obsGrowth=diff(observedLength),
               predGrowth=predictedLength[2:length(observedLength)]-
                 observedLength[1:(length(observedLength)-1)],
               date=detectionDate[2:length(detectionDate)]),
            by=tag] %>%
         .[,residual:=obsGrowth-predGrowth])
assign(paste0("core",which(r==rivers)),core)

plot(predictedLength~observedLength,data=get(paste0("core",which(r==rivers))))
a<-lm(predictedLength~observedLength,get(paste0("core",which(r==rivers))))
text(75,150,bquote(R^2==.(round(summary(a)$r.squared,2))))

plot(obsGrowth~predGrowth,data=get(paste0("gr",which(r==rivers))),pch=19,col=gray(0.45,0.5))
a<-lm(obsGrowth~predGrowth,get(paste0("gr",which(r==rivers))))
abline(a)
abline(0,1,lty=2)
text(5,15,bquote(R^2==.(round(summary(a)$r.squared,2))),col='black')


#   plot(mmPerDay~startLength,data=get(paste0("gr",which(r==rivers))),
#        col=gray(0.5,0.5),pch=19,main=r,xlab="startLength",ylab="growth (mm/day)")
#   x<-seq(60,300)
#   for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
#     points(predictVonBert(x,
#                           out$sims.list$beta[i,1],
#                           out$sims.list$beta[i,2],
#                           derivative=T)*24~x,
#            type='l',col='blue')
#   }
#
#
#
#   predicted<-apply(out$sims.list$length,2,mean)
#   plot(predicted~get(paste0("gr",which(r==rivers)))$growth,main=r,
#        ylab="predicted growth",xlab="observed growth")
#   abline(0,1)
# }
}
