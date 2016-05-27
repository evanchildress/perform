library(perform)
library(plotHacks)

predictGrowth<-function(startDay,endDay,tempData,tOpt,ctMax,sigma,b1,b2,b3,startSize,discharge){

  years<-unique(year(tempData$datetime))
  endSize<-rep(as.numeric(NA),length(years))
  for(y in years){
    size<-startSize
    startTime<-which.min(abs(as.numeric(tempData$datetime-as.POSIXct(paste0(y,"-",startDay)))))
    endTime<-which.min(abs(as.numeric(tempData$datetime-as.POSIXct(paste0(y,"-",endDay)))))
    p<-predictPerformance(tempData[startTime:endTime,temperature],
                            tOpt,ctMax,sigma)

    days<-c(seq(1,length(p),24),length(p))
#     qYear<-discharge[date>=as.Date(paste0(y,"-",startDay))&date<=as.Date(paste0(y,"-",endDay)),
#                  (discharge-0.3084605)/0.2345925]
    for(d in 1:(length(days)-1)){
      size[d+1]<-size[d]+sum(p[days[d]:(days[d+1]-1)])*(size[d]*b2+b1)
    }
    endSize[which(y==years)]<-max(size)
  }
  return(endSize)
}

predictElliottGrowth<-function(startDay,endDay,tempData,startSize){

  years<-unique(year(tempData$datetime))
  endSize<-rep(as.numeric(NA),length(years))
  for(y in years){
    size<-startSize
    startTime<-which.min(abs(as.numeric(tempData$datetime-as.POSIXct(paste0(y,"-",startDay)))))
    endTime<-which.min(abs(as.numeric(tempData$datetime-as.POSIXct(paste0(y,"-",endDay)))))

    days<-c(seq(1,length(startTime:endTime),24),length(startTime:endTime))

    for(d in 1:(length(days)-1)){
      size[d+1]<-elliottGrowth(size[d],
                               tempData[(startTime+days[d]-1):
                                          (startTime+days[d+1]-2),
                                        temperature])
    }
    endSize[which(y==years)]<-max(size)
  }
  return(endSize)
}


mcMahonGrowth<-function(weight,temps){

    wExp<-sum((0.0131*temps-
                    0.0005*temps^2-0.0362)/24)+weight

  return(wExp)
}

predictMcMahonGrowth<-function(startDay,endDay,tempData,startSize){

  years<-unique(year(tempData$datetime))
  endSize<-rep(as.numeric(NA),length(years))
  for(y in years){
    startTime<-which.min(abs(as.numeric(tempData$datetime-as.POSIXct(paste0(y,"-",startDay)))))
    endTime<-which.min(abs(as.numeric(tempData$datetime-as.POSIXct(paste0(y,"-",endDay)))))

    endSize[which(y==years)]<-mcMahonGrowth(startSize,
                                     tempData[startTime:endTime,temperature])
  }
  return(endSize)
}

predictBktLabGrowth<-function(startDay,endDay,tempData,tOpt,ctMax,
                              sigma,b1,b2,startSize){

  years<-unique(year(tempData$datetime))
  endSize<-rep(as.numeric(NA),length(years))
  for(y in years){
    size<-startSize
    startTime<-which.min(abs(as.numeric(tempData$datetime-as.POSIXct(paste0(y,"-",startDay)))))
    endTime<-which.min(abs(as.numeric(tempData$datetime-as.POSIXct(paste0(y,"-",endDay)))))
    p<-predictPerformance(tempData[startTime:endTime,temperature],
                          tOpt,ctMax,sigma)

    days<-c(seq(1,length(p),24),length(p))
    #     qYear<-discharge[date>=as.Date(paste0(y,"-",startDay))&date<=as.Date(paste0(y,"-",endDay)),
    #                  (discharge-0.3084605)/0.2345925]
    for(d in 1:(length(days)-1)){
      size[d+1]<-size[d]+sum(p[days[d]:(days[d+1]-1)])*(size[d]*b2+b1)
    }
    endSize[which(y==years)]<-max(size)
  }
  return(endSize)
}

t<-temp %>%
  .[river=="west brook"] %>%
  .[year(datetime)>=2002&year(datetime)<=2015] %>%
  setkey(datetime)


for(sp in c("bkt","bnt")){
  bla<-createCoreData(columnsToAdd="observedWeight") %>% data.table()
  assign(paste0(sp,"lm"),lm(log(observedLength)~log(observedWeight),data=bla))
  rm(bla)


  res<-data.frame(year=2002:2015)
  res2<-res


    out<-readRDS(paste0("vignettes/westBrook/results/out",sp,"West brook",".rds"))

    tOpt<-mean(out$sims.list$tOpt)
    ctMax<-mean(out$sims.list$ctMax)
    sigma<-mean(out$sims.list$sigma)

    beta1<-mean(out$sims.list$beta1)
    beta2<-mean(out$sims.list$beta2)
    beta3<-mean(out$sims.list$beta3)

    if(sp=="bkt"){
      out<-readRDS("vignettes/westBrook/results/outBktLabPerformance.rds")

      tOptLab<-median(out$sims.list$tOpt)
      ctMaxLab<-median(out$sims.list$ctMax)
      sigmaLab<-median(out$sims.list$sigma)
    }

  for(a in c(0,1,2,3)){
    bla<-t
    bla$copy<-NA
    bla[,temperature:=temperature+a]

    res[,paste0("t",a)]<-predictGrowth("03-10","10-01",bla,tOpt,ctMax,sigma,beta1,beta2,beta3,20,q)
    if(sp=="bnt"){
      res2[,paste0("t",a)]<-predictElliottGrowth("03-10","10-01",bla,0.087)
    }
    if(sp=="bkt"){
      #res2[,paste0("t",a)]<-predictMcMahonGrowth("03-10","10-01",bla,0.087)
      res2[,paste0("t",a)]<-predictGrowth("03-10","10-01",bla,tOptLab,
                                          ctMaxLab,sigmaLab,0.0176,-5.569e-5,
                                          beta3,20)
    }
  }
  res<-melt(res,id.vars="year")
  names(res)<-c("year","temp","fallSize")
  res$model<-"mine"

  res2<-melt(res2,id.vars="year")
  names(res2)<-c("year","temp","fallSize")
  res2$model<-"other"
  res2$fallSize<-exp(predict(get(paste0(sp,"lm")),
                              data.frame(observedWeight=res2$fallSize)))
  res<-rbind(res,res2)

  assign(paste0(sp,"Res"),data.table(res))
}

bktRes[,presentSize:=fallSize[which(temp=="t0")],by=.(year,model)]
bktRes[,proportionOfPresent:=fallSize/presentSize]

bntRes[,presentSize:=fallSize[which(temp=="t0")],by=.(year,model)]
bntRes[,proportionOfPresent:=fallSize/presentSize]


tiff.par("vignettes/westBrook/results/figures/predictedFallSize.tif",mfrow=c(2,2))
for(sp in c("bkt","bnt")){
  boxplot(proportionOfPresent~temp,data=get(paste0(sp,"Res"))[model=="mine"],
          ylab="Predicted Fall Size (mm)",ylim=c(0,1),
          names=NA)
  axis(1,at=1:4,labels=c(expression(+0~degree*C),expression(+1~degree*C),
                   expression(+2~degree*C),expression(+3~degree*C)))

  boxplot(proportionOfPresent~temp,data=get(paste0(sp,"Res"))[model=="other"],
          ylab="Predicted Fall Size (mm)",ylim=c(0,1),
          names=NA)
  axis(1,at=1:4,labels=c(expression(+0~degree*C),expression(+1~degree*C),
                         expression(+2~degree*C),expression(+3~degree*C)))

}
dev.off()
