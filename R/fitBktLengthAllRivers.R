library(perform)
reconnect()
sp<-"bkt"

    priors<-list(bkt=list(tOptMean=0,#14.2,
                          tOptPrecision=0.0001,
                          ctMaxMean=25,#23.4,
                          ctMaxPrecision=0.0001,
                          ctUltimate=26),#25.3),
                 bnt=list(tOptMean=0,#15.95,
                          tOptPrecision=0.0001,
                          ctMaxMean=25,#22.5,
                          ctMaxPrecision=0.0001,
                          ctUltimate=40)#28)
    )

    core<-createCoreData("electrofishing",columnsToAdd="observedWeight") %>%
      # data.table() %>%
      # .[,n:=.N,by=tag] %>%
      # .[n>1] %>%
      # .[,n:=NULL] %>%
      # data.frame() %>%
      addTagProperties() %>%
      filter(species==sp) %>%
      createCmrData() %>%
      addKnownZ() %>%
      #filter(knownZ==1) %>%
      fillSizeLocation(size=F) %>%
      addSampleProperties() %>%
      addEnvironmental(funName="mean") %>%
      rename(medianFlow=meanFlow)%>%
      select(-meanTemperature,-lagDetectionDate) %>%
      data.table() #%>%
      # .[,diffSample:=c(NA,diff(sampleIndex)),by=tag]


    getFirstLength<-function(riv,age,sampName){
      core[river==riv&ageInSamples %in% age&sampleName %in% sampName,mean(observedLength,na.rm=T),
           by=.(ageInSamples,sampleName)]$V1
    }

    core[,firstLength:=min(detectionDate[!is.na(observedLength)]),by=tag] %>%
      .[detectionDate<firstLength&enc==1,observedLength:=getFirstLength(river,ageInSamples,sampleName),by=tag] %>%
      .[,firstLength:=NULL]

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

    # movers<-unique(core[diffSample>1,tag])
    # for(t in movers){
    #   core[tag==t&
    #          sampleIndex %in%
    #          sampleIndex[min(which(diffSample>1)):length(sampleIndex)],
    #        observedLength:=NA]
    # }
    # core[,diffSample:=NULL]
    # core<-core[,nMeasures:=sum(!is.na(observedLength)),by=tag] %>%
    #   .[nMeasures>1] %>%
    #   .[,nMeasures:=NULL] %>%
    #   .[,lastObs:=detectionDate==max(detectionDate),by=tag] %>%
    #   .[!lastObs|!is.na(observedLength)] %>%
    #   .[,lastObs:=NULL] %>%
    #   .[,firstDate:=min(detectionDate[!is.na(observedLength)]),tag] %>%
    #   .[detectionDate>=firstDate] %>%
    #   .[,firstDate:=NULL]

    core[,tagIndex:=match(tag,unique(tag))] %>%
      setkey(river,year,season)

    #
    bktBiomass<-readRDS("vignettes/westBrook/bktBiomass.rds")
    bntBiomass<-readRDS("vignettes/westBrook/bntBiomass.rds")

    core<-bktBiomass[core] %>%
      bntBiomass[.] %>%
      setkey(tag,detectionDate)

    core[is.na(bntBiomass)&river=="wb mitchell",bntBiomass:=0]
    core[river=="wb obear",bntBiomass:=0]

    jagsData<-createJagsData(data.frame(core) %>%
                               addEnvironmental()) #%>%
      # .[c("firstObsRows",
      #     "nFirstObsRows",
      #     "evalRows",
      #     "nEvalRows",
      #     "nAllRows",
      #     "lengthDATA",
      #     "ind",
      #     "season")]

    # core<-core[,.(tag,tagIndex,detectionDate,observedLength,
    #               bktBiomass,bntBiomass,medianFlow)]



    t<-temp %>%
      .[,date:=as.Date(datetime)]%>%
      .[datetime>=min(core$detectionDate)&datetime<=max(core$detectionDate)] %>%
      .[,.(temperature=mean(temperature)),by=.(river,datetime)] %>%
      setkey(river,datetime) %>%
      melt(id.vars=c("river","datetime")) %>%
      dcast(datetime~river)

    core[,':='(time=which.min(abs(t$datetime-detectionDate))),
          by=.(tag,detectionDate)]

    tempDATA<-t[,2:5]

    jagsData$tempDATA<-tempDATA
    jagsData$nTimes<-nrow(tempDATA)
    jagsData$time<-core$time
    jagsData$nInd<-max(core$tagIndex)
    jagsData$bktBiomassDATA<-scale(core$bktBiomass)[,1]
    jagsData$bntBiomassDATA<-scale(core$bntBiomass)[,1]
    jagsData$biomassDATA<-scale(core[[paste0(sp,"Biomass")]])[,1]
    jagsData$flowDATA<-scale(core$medianFlow)[,1]
    jagsData$tOptMean<-priors[[sp]][["tOptMean"]]
    jagsData$tOptPrecision<-priors[[sp]][["tOptPrecision"]]
    jagsData$ctMaxMean<-priors[[sp]][["ctMaxMean"]]
    jagsData$ctMaxPrecision<-priors[[sp]][["ctMaxPrecision"]]
    jagsData$ctUltimate<-priors[[sp]][["ctUltimate"]]

    jagsData$meanFlow<-mean(core$medianFlow,na.rm=T)
    jagsData$sdFlow<-sd(core$medianFlow,na.rm=T)
    jagsData$meanBktBiomass<-mean(core$bktBiomass,na.rm=T)
    jagsData$sdBktBiomass<-sd(core$bktBiomass,na.rm=T)
    jagsData$meanBntBiomass<-mean(core$bntBiomass,na.rm=T)
    jagsData$sdBntBiomass<-sd(core$bntBiomass,na.rm=T)


    # if(r=="wb mitchell"){
    #   badStarts<-as.POSIXct("2007-08-02 19:00:00","2013-05-01 00:00:00")
    #   badEnds<-as.POSIXct("2007-08-04 00:00:00","2013-10-01 00:00:00")
    #   for(b in 1:length(badStarts)){
    #     st<-which(t$datetime==badStarts[b])
    #     en<-which(t$datetime==badEnds[b])
    #     jagsData$evalRows<-jagsData$evalRows[
    #       (jagsData$time[jagsData$evalRows]&jagsData$time[jagsData$evalRows-1]<st)|
    #         (jagsData$time[jagsData$evalRows]&jagsData$time[jagsData$evalRows-1]>en)]
    #   }
    #   jagsData$nEvalRows<-length(jagsData$evalRows)
    # }
    core[,nObs:=length(na.omit(observedLength)),by=tag]
    core[nObs>1,lengthInit:=approx(observedLength,n=length(observedLength))$y,by=tag]
    core[,nObs:=NULL]
    core[!is.na(observedLength),lengthInit:=NA]

    inits<-function(){list(lengthData=core$lengthInit,
                           ctMax=rep(20,4),
                           tOpt=rep(10,4),
                           beta1=rep(0.015,4),
                           beta2=rep(-6e-05,4),
                           eps=rep(0.003,4))
    }

    parsToMonitor<-c("beta1","beta2","beta3","beta4","beta5",
                     "tOpt",'ctMax',"sigma","ranMonth","sigmaMonth",
                     'eps',"sigmaInd","lengthExp","b")
    out<-fitModel(jagsData=jagsData,inits=inits,parallel=T,params=parsToMonitor,
                  nb=5000,ni=7000,nt=1,modelFile="modelLengthFieldAllRivers.R",codaOnly="lengthExp")




    saveRDS(out,file=paste0("vignettes/westBrook/results/out",sp,".rds"))

    core[,firstObs:=detectionDate==min(detectionDate),by=tag]
    core[firstObs=="FALSE",predictedLength:=apply(out$sims.list$lengthExp,2,mean)]

    saveRDS(core,file=paste0("vignettes/westBrook/results/core",sp,".rds"))
    print(out)
    # core[jagsData$evalRows,predictedLength:=apply(out$sims.list$lengthExp,2,mean)]
    # core[,residual:=observedLength-predictedLength]



#
# for(r in c("wb jimmy","wb mitchell","wb obear","west brook")){
#   for(sp in c("bkt")){
#     if(sp=="bnt"&r=="wb obear") next
#
#     out<-get(paste0("out",sp,which(r==rivers)))
#     plot(NA,xlim=c(0,22),ylim=c(-1,1),main=paste(r,sp),xlab="temp",ylab="performance")
#     for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
#       points(predictPerformance(0:22,tOpt=out$sims.list$tOpt[i],
#                                 sigma=out$sims.list$sigma[i],
#                                 ctMax=out$sims.list$ctMax[i])~I(0:22),
#              type='l',col='gray')
#     }
#
#     core<-get(paste0("core",sp,which(r==rivers)))
#     core$dummy<-1
#     core[,firstObs:=detectionDate==min(detectionDate),by=tag] %>%
#       .[firstObs==F,predictedLength:=apply(out$sims.list$lengthExp,2,mean)]
#
#     assign(paste0("gr",sp,which(r==rivers)),
#            core[,.(obsGrowth=diff(observedLength),
#                    predGrowth=predictedLength[2:length(observedLength)]-
#                      observedLength[1:(length(observedLength)-1)],
#                    date=detectionDate[2:length(detectionDate)]),
#                 by=tag] %>%
#              .[,residual:=obsGrowth-predGrowth])
#     assign(paste0("core",sp,which(r==rivers)),core)
#
#     plot(predictedLength~observedLength,data=get(paste0("core",sp,which(r==rivers))))
#     a<-lm(predictedLength~observedLength,get(paste0("core",sp,which(r==rivers))))
#     text(75,150,bquote(R^2==.(round(summary(a)$r.squared,2))))
#
#     plot(obsGrowth~predGrowth,data=get(paste0("gr",sp,which(r==rivers))),pch=19,col=gray(0.45,0.5))
#     a<-lm(obsGrowth~predGrowth,get(paste0("gr",sp,which(r==rivers))))
#     abline(a)
#     abline(0,1,lty=2)
#     text(5,15,bquote(R^2==.(round(summary(a)$r.squared,2))),col='black')
#
#
#     #   plot(mmPerDay~startLength,data=get(paste0("gr",which(r==rivers))),
#     #        col=gray(0.5,0.5),pch=19,main=r,xlab="startLength",ylab="growth (mm/day)")
#     #   x<-seq(60,300)
#     #   for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
#     #     points(predictVonBert(x,
#     #                           out$sims.list$beta[i,1],
#     #                           out$sims.list$beta[i,2],
#     #                           derivative=T)*24~x,
#     #            type='l',col='blue')
#     #   }
#     #
#     #
#     #
#     #   predicted<-apply(out$sims.list$length,2,mean)
#     #   plot(predicted~get(paste0("gr",which(r==rivers)))$growth,main=r,
#     #        ylab="predicted growth",xlab="observed growth")
#     #   abline(0,1)
#   }
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
