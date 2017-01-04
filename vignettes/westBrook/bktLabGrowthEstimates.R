library(perform)
library(plotHacks)
reconnect()

rivers<-c("wb jimmy","wb mitchell","wb obear","west brook")
#r<-"wb jimmy"
r<-"west brook"
sp<-"bkt"

predictGrowth<-function(startTime,endTime,tempData,tOpt,ctMax,sigma,
                        b1,b2,b3,b4,b5,startSize,flow,bktBiomass,bntBiomass){
  endSize<-rep(as.numeric(NA),length(startTime))
  for(t in 2:length(startTime)){
    p<-sum(predictPerformance(tempData[startTime[t]:endTime[t],temperature],
                          tOpt,ctMax,sigma))

    endSize[t]<-p*(startSize[t-1]*b2+b1+b3*flow[t-1]+
                     b4*bktBiomass[t-1]+b5*bntBiomass[t-1])+startSize[t-1]
  }
  return(endSize)
}


    core<-readRDS(paste0("vignettes/westBrook/results/core",sp,r,".rds"))
    core[tag=="257c67ca48"&observedLength==118,observedLength:=218]
    core[tag=="00088cfb9e"&observedLength==223,observedLength:=NA]
    core[tag=="00088d215d"&observedLength==67,observedLength:=NA]
    core[tag=="0009f6f4d5"&observedLength==115,observedLength:=NA]
#     createCoreData("electrofishing",columnsToAdd="observedWeight") %>%
#       data.table() %>%
#       .[,n:=.N,by=tag] %>%
#       .[n>1] %>%
#       .[,n:=NULL] %>%
#       data.frame() %>%
#       addTagProperties() %>%
#       createCmrData() %>%
#       addKnownZ() %>%
#       filter(knownZ==1) %>%
#       fillSizeLocation(size=F) %>%
#       addSampleProperties() %>%
#       filter(river==r&species==sp) %>%
#       addEnvironmental(funName="mean") %>%
#       rename(medianFlow=meanFlow)%>%
#       select(-meanTemperature,-lagDetectionDate) %>%
#       data.table() %>%
#       .[,diffSample:=c(NA,diff(sampleIndex)),by=tag]
#
#     if(sp=="bnt"){
#       core[tag=="257c59a7cc"&observedLength==258,observedLength:=NA]
#       core[tag=="1c2c582218"&observedLength==240,observedLength:=NA]
#       core[tag=="00088d22ae"&observedLength==70,observedLength:=NA]
#       core[tag=="257c59b698"&observedLength==117,observedLength:=NA]
#       core[tag=="1bf0fec1d7"&observedLength==66,observedLength:=NA]
#     }
#     if(sp=="bkt"){
#       core[tag=="257c67ca48"&observedLength==224,observedLength:=NA]
#     }
#
#     movers<-unique(core[diffSample>1,tag])
#     for(t in movers){
#       core[tag==t&
#              sampleIndex %in%
#              sampleIndex[min(which(diffSample>1)):length(sampleIndex)],
#            observedLength:=NA]
#     }
#     core[,diffSample:=NULL]
#     core<-core[,nMeasures:=sum(!is.na(observedLength)),by=tag] %>%
#       .[nMeasures>1] %>%
#       .[,nMeasures:=NULL] %>%
#       .[,lastObs:=detectionDate==max(detectionDate),by=tag] %>%
#       .[!lastObs|!is.na(observedLength)] %>%
#       .[,lastObs:=NULL] %>%
#       .[,firstDate:=min(detectionDate[!is.na(observedLength)]),tag] %>%
#       .[detectionDate>=firstDate] %>%
#       .[,firstDate:=NULL]

    t<-temp %>%
      .[river==r] %>%
      .[,date:=as.Date(datetime)]%>%
      .[datetime>=min(core$detectionDate)&datetime<=max(core$detectionDate)] %>%
      setkey(datetime)

#     core[,':='(time=which.min(
#       abs(
#         t$datetime-as.POSIXct(paste0(detectionDate," 12:00:00"))
#       )
#     )),
#     by=.(tag,detectionDate)]

    out<-readRDS(paste0("vignettes/westBrook/results/out",sp,r,".rds"))
    beta1<-apply(out$sims.list$beta1,2,mean)
    beta2<-mean(out$sims.list$beta2)
    beta3<-mean(out$sims.list$beta3)
    beta4<-mean(out$sims.list$beta4)
    beta5<-mean(out$sims.list$beta5)
    rm(out)

    out<-readRDS(paste0("vignettes/westBrook/results/out",sp,"LabPerformance.rds"))
    #out<-readRDS(paste0("vignettes/westBrook/results/out",sp,r,".rds"))
    growthSumStats<-NULL
    for(i in 1){
    s<-sample(1:length(out$sims.list$tOpt),1)
    tOpt<-out$sims.list$tOpt[s]
    ctMax<-out$sims.list$ctMax[s]
    sigma<-out$sims.list$sigma[s]

    core[,timeLagged:=shift(time),by=tag]
    setkey(core,tag,detectionDate)

    core[,":="(bktBiomassScaled=scale(bktBiomass)[,1],
               bntBiomassScaled=scale(bntBiomass)[,1],
               medianFlowScaled=scale(medianFlow)[,1])]


#     setkey(core,year,season)
#
#
#     bktBiomass<-readRDS("vignettes/westBrook/bktBiomass.rds")
#     bntBiomass<-readRDS("vignettes/westBrook/bntBiomass.rds")
#
#     core<-bktBiomass[core] %>%
#       bntBiomass[.] %>%
#       setkey(tag,detectionDate)
#
#     core[,":="(medianFlow=scale(medianFlow)[,1],
#                bktBiomass=scale(bktBiomass)[,1],
#                bntBiomass=scale(bntBiomass)[,1])]

    core[,lExp:=predictGrowth(timeLagged,time,tempData=t,tOpt=tOpt,ctMax=ctMax,
                              sigma=sigma,b1=beta1[unique(tagIndex)],b2=beta2,b3=beta3,b4=beta4,
                              #b1=beta1,b2=beta2,b3=beta3,b4=beta4,
                              b5=beta5,startSize=observedLength,medianFlowScaled,
                              bktBiomassScaled,bntBiomassScaled),by=tag]


    gr<-core[,.( gObs=diff(observedLength),
                 gExp=lExp[2:length(lExp)]-observedLength[1:(length(lExp)-1)],
                 startLength=observedLength[1:(length(lExp)-1)],
                 startDate=detectionDate[1:(length(lExp)-1)],
                 endDate=detectionDate[2:length(lExp)]),
              by=tag]
    lengthSumStats<-core[!is.na(observedLength)&!is.na(lExp),
               .(relativeBias=sum(lExp-observedLength)/.N/mean(observedLength),
                 rmse=sqrt(sum((lExp-observedLength)^2)/.N))]
    growthSumStats<-rbind(growthSumStats,
                          gr[!is.na(gObs)&!is.na(gExp),
                         .(relativeBias=sum(gExp-gObs)/.N/mean(gObs),
                           rmse=sqrt(sum((gExp-gObs)^2)/.N))])
    cat(paste0(i,": ",s,"\n"))
}
# tiff.par("vignettes/westBrook/results/figures/labBktGrowth.tif")
    plot(I(gExp/10)~I(gObs/10),data=gr,pch=16,col=gray(0.5,0.5),
         ylab="Lab Model Predicted Growth (cm)",
         xlab="Observed Growth (cm)")
    abline(0,1)
    panelLabel(bquote(bold(a)))
# dev.off()


