library(perform)
reconnect()

rivers<-c("wb jimmy","wb mitchell","wb obear","west brook")
#r<-"wb jimmy"
for(r in c("west brook")){
  for(sp in c("bkt","bnt")){
  # for(sp in c("ats")){
    if(sp=="bnt"&r=="wb obear") next

endDate<-as.POSIXct("2016-10-01")

  core<-createCoreData("electrofishing",columnsToAdd="observedWeight") %>%
    data.table() %>%
    .[,n:=.N,by=tag] %>%
    .[n>1] %>%
    .[,n:=NULL] %>%
    data.frame() %>%
    addTagProperties() %>%
    createCmrData(dateEnd = endDate) %>%
    addKnownZ() %>%
    filter(knownZ==1) %>%
    fillSizeLocation(size=F) %>%
    addSampleProperties() %>%
    filter(river==r&species==sp) %>%
    addEnvironmental(funName="mean") %>%
    rename(medianFlow=meanFlow)%>%
    select(-meanTemperature,-lagDetectionDate) %>%
    data.table() %>%
    .[,diffSample:=c(NA,diff(sampleIndex)),by=tag]


  if(sp=="bnt"){
    core[tag=="257c59a7cc"&observedLength==258,observedLength:=NA]
    core[tag=="1c2c582218"&observedLength==240,observedLength:=NA]
    core[tag=="00088d22ae"&observedLength==70,observedLength:=NA]
    core[tag=="257c59b698"&observedLength==117,observedLength:=NA]
    core[tag=="1bf0fec1d7"&observedLength==66,observedLength:=NA]
  }
  if(sp=="bkt"){
    core[tag=="257c67ca48"&observedLength==224,observedLength:=NA]
  }

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
        .[,lastObs:=NULL] %>%
        .[,firstDate:=min(detectionDate[!is.na(observedLength)]),tag] %>%
        .[detectionDate>=firstDate] %>%
        .[,firstDate:=NULL]

  core[,tagIndex:=match(tag,unique(tag))] %>%
    setkey(river,year,season)

#
  bktBiomass<-readRDS("vignettes/westBrook/bktBiomass.rds")
  bntBiomass<-readRDS("vignettes/westBrook/bntBiomass.rds")

  core<-bktBiomass[core] %>%
        bntBiomass[.] %>%
    setkey(tag,detectionDate)
#   if(r=="wb obear"){
#     core[,bntBiomass:=0]
#   }
  if(r=="wb mitchell"){
    core[is.na(bntBiomass),bntBiomass:=0]
  }
#   if(sp=="ats"){
#     core[,":="(meanBktBiomass=mean(bktBiomass,na.rm=T),
#                meanBntBiomass=mean(bntBiomass,na.rm=T)),by=season] %>%
#       .[is.na(bktBiomass),bktBiomass:=meanBktBiomass] %>%
#       .[is.na(bntBiomass),bntBiomass:=meanBntBiomass] %>%
#       .[,":="(meanBktBiomass=NULL,
#               meanBntBiomass=NULL)]
#   }
  #core<-core[!tag %in% c("00088d1ac1","00088d2f6f")]
    # .[,tagIndex:=match(tag,unique(tag))] %>%
    # setkey(tagIndex,detectionDate) %>%
    # .[,firstObs:=detectionDate==min(detectionDate),by=tag]



  jagsData<-createJagsData(data.frame(core) %>%
                             addEnvironmental()) %>%
    .[c("firstObsRows",
        "nFirstObsRows",
        "evalRows",
        "nEvalRows",
        "nAllRows",
        "lengthDATA",
        "ind",
        "season")]

  core[,age:=as.numeric((as.Date(detectionDate)-as.Date(paste0(cohort,"-03-15"))))/365]

  core<-core[,.(tag,tagIndex,detectionDate,observedLength,
                bktBiomass,bntBiomass,medianFlow,age)]

  t<-temp %>%
    .[river==r] %>%
    .[,date:=as.Date(datetime)]%>%
    .[datetime>=min(core$detectionDate)&datetime<=max(core$detectionDate)] %>%
    setkey(datetime)

  #setting an arbitrary temperature for a period of NAs,
  #but growth over this period is excluded from the likelihood in the model
  if(sp=="ats"&r=="west brook"){
    t[is.na(temperature),temperature:=23]
  }

  core[,':='(time=which.min(
    abs(
      t$datetime-as.POSIXct(paste0(detectionDate," 12:00:00"))
    )
  )),
  by=.(tag,detectionDate)]


  jagsData$nInd<-max(core$tagIndex)
  jagsData$age<-core$age
  jagsData$evalRows<-1:nrow(core)
  jagsData$nEvalRows<-nrow(core)

  if(r=="wb obear"){
    jagsData$bntBiomassDATA<-rep(0,nrow(core))
  }




  core[,lengthInit:=approx(observedLength,n=length(observedLength))$y,by=tag]
  core[!is.na(observedLength),lengthInit:=NA]

  inits<-function(){list(kMean=0.75,
                         lInf=350,
                         l0=10)
  }

  parsToMonitor<-c("kMean","sigmaK","lInf","l0","lengthExp","k")
  out<-fitModel(jagsData=jagsData,inits=inits,parallel=T,params=parsToMonitor,
                nb=8000,ni=18000,nt=10,modelFile="modelVonBert.R",codaOnly=c("lengthExp","ranInd","ranSlope","beta1"))
  saveRDS(out,file=paste0("vignettes/westBrook/results/outVonBert",sp,toupper(substr(r,1,1)),substr(r,2,nchar(r)),".rds"))
  #saveRDS(core,file=paste0("vignettes/westBrook/results/core",sp,toupper(substr(r,1,1)),substr(r,2,nchar(r)),".rds"))
  print(out)
  assign(paste0("out",sp,which(r==rivers)),out)
  assign(paste0("core",sp,which(r==rivers)),core)
  # core[jagsData$evalRows,predictedLength:=apply(out$sims.list$lengthExp,2,mean)]
  # core[,residual:=observedLength-predictedLength]
 }
}


#
#  for(r in c("wb jimmy","wb mitchell","wb obear","west brook")){
#    for(sp in c("bkt")){
#      if(sp=="bnt"&r=="wb obear") next
#
#   out<-get(paste0("out",sp,which(r==rivers)))
#   plot(NA,xlim=c(0,22),ylim=c(-1,1),main=paste(r,sp),xlab="temp",ylab="performance")
#   for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
#     points(predictPerformance(0:22,tOpt=out$sims.list$tOpt[i],
#                               sigma=out$sims.list$sigma[i],
#                               ctMax=out$sims.list$ctMax[i])~I(0:22),
#            type='l',col='gray')
#   }
#
# core<-get(paste0("core",sp,which(r==rivers)))
# core$dummy<-1
# core[,firstObs:=detectionDate==min(detectionDate),by=tag] %>%
#       .[firstObs==F,predictedLength:=apply(out$sims.list$lengthExp,2,mean)]
#
# assign(paste0("gr",sp,which(r==rivers)),
#        core[,.(obsGrowth=diff(observedLength),
#                predGrowth=predictedLength[2:length(observedLength)]-
#                  observedLength[1:(length(observedLength)-1)],
#                date=detectionDate[2:length(detectionDate)]),
#             by=tag] %>%
#          .[,residual:=obsGrowth-predGrowth])
# assign(paste0("core",sp,which(r==rivers)),core)
#
# plot(predictedLength~observedLength,data=get(paste0("core",sp,which(r==rivers))))
# a<-lm(predictedLength~observedLength,get(paste0("core",sp,which(r==rivers))))
# text(75,150,bquote(R^2==.(round(summary(a)$r.squared,2))))
#
# plot(obsGrowth~predGrowth,data=get(paste0("gr",sp,which(r==rivers))),pch=19,col=gray(0.45,0.5))
# a<-lm(obsGrowth~predGrowth,get(paste0("gr",sp,which(r==rivers))))
# abline(a)
# abline(0,1,lty=2)
# text(5,15,bquote(R^2==.(round(summary(a)$r.squared,2))),col='black')
#
#
# #   plot(mmPerDay~startLength,data=get(paste0("gr",which(r==rivers))),
# #        col=gray(0.5,0.5),pch=19,main=r,xlab="startLength",ylab="growth (mm/day)")
# #   x<-seq(60,300)
# #   for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
# #     points(predictVonBert(x,
# #                           out$sims.list$beta[i,1],
# #                           out$sims.list$beta[i,2],
# #                           derivative=T)*24~x,
# #            type='l',col='blue')
# #   }
# #
# #
# #
# #   predicted<-apply(out$sims.list$length,2,mean)
# #   plot(predicted~get(paste0("gr",which(r==rivers)))$growth,main=r,
# #        ylab="predicted growth",xlab="observed growth")
# #   abline(0,1)
#  }
# }
#
# ###code to estimate bias and mse
# bktSummary<-corebkt4[!is.na(predictedLength)&!is.na(observedLength),
#                      .(rmse=sqrt(sum(((observedLength-predictedLength))^2)/.N),
#                        relativeBias=sum((observedLength-predictedLength)/observedLength)/.N)]
# bntSummary<-corebnt4[!is.na(predictedLength)&!is.na(observedLength),
#                      .(rmse=sqrt(sum(((observedLength-predictedLength))^2)/.N),
#                        relativeBias=sum((observedLength-predictedLength)/observedLength)/.N)]
# bktGrowthSummary<-grbkt4[!is.na(predGrowth)&!is.na(obsGrowth),
#                          .(rmse=sqrt(sum(((obsGrowth-predGrowth))^2)/.N),
#                            relativeBias=sum((obsGrowth-predGrowth))/.N/mean(obsGrowth))]
# bntGrowthSummary<-grbnt4[!is.na(predGrowth)&!is.na(obsGrowth),
#                          .(rmse=sqrt(sum(((obsGrowth-predGrowth))^2)/.N),
#                            relativeBias=sum((obsGrowth-predGrowth))/.N/mean(obsGrowth))]
#
#
# plot(NA,xlim=c(0,25),ylim=c(-1.5,1))
# for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
#   points(predictPerformance(seq(0,23.3,length.out=100),tOpt=outbkt1$sims.list$tOpt[i],
#                             sigma=outbkt1$sims.list$sigma[i],
#                             ctMax=outbkt1$sims.list$ctMax[i])~I(seq(0,23.3,length.out=100)),
#          type='l',col=palette()[1])
#
#   points(predictPerformance(seq(0,25.4,length.out=100),tOpt=outbkt2$sims.list$tOpt[i],
#                             sigma=outbkt2$sims.list$sigma[i],
#                             ctMax=outbkt2$sims.list$ctMax[i])~I(seq(0,24.4,length.out=100)),
#          type='l',col=palette()[2])
#
#   points(predictPerformance(seq(0,21.2,length.out=100),tOpt=outbkt3$sims.list$tOpt[i],
#                             sigma=outbkt3$sims.list$sigma[i],
#                             ctMax=outbkt3$sims.list$ctMax[i])~I(seq(0,21.2,length.out=100)),
#          type='l',col=palette()[3])
# }
#
