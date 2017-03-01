library(perform)
reconnect()

rivers<-c("wb jimmy","wb mitchell","wb obear","west brook")
r<-"west brook"

  # for(sp in c("bkt","bnt")){

sp<-"bnt"
  # for(sp in c("ats")){
    if(sp=="bnt"&r=="wb obear") next

  priors<-list(bkt=list(tOptMean=0,#14.2,
                            tOptPrecision=0.0001,
                            ctMaxMean=25,#23.4,
                            ctMaxPrecision=0.0001,
                            ctUltimate=40),
                   bnt=list(tOptMean=0,#15.95,
                            tOptPrecision=0.0001,
                            ctMaxMean=25,#22.5,
                            ctMaxPrecision=0.0001,
                            ctUltimate=40)
  )

  core<-createCoreData("electrofishing",columnsToAdd="observedWeight") %>%
    data.table() %>%
    .[,n:=.N,by=tag] %>%
    .[n>1] %>%
    .[,n:=NULL] %>%
    data.frame() %>%
    addTagProperties() %>%
    createCmrData(dateEnd=as.POSIXct("2016-10-01")) %>%
    addKnownZ() %>%
    #filter(knownZ==1) %>%
    fillSizeLocation(size=F) %>%
    addSampleProperties() %>%
    filter(river==r&species==sp) %>%
    addEnvironmental(funName="mean") %>%
    rename(medianFlow=meanFlow)%>%
    select(-meanTemperature,-lagDetectionDate) %>%
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
        .[,lastObs:=NULL] %>%
        .[,firstDate:=min(detectionDate[!is.na(observedLength)]),tag] %>%
        .[detectionDate>=firstDate] %>%
        .[,firstDate:=NULL]

  core[,tagIndex:=match(tag,unique(tag))] %>%
    setkey(river,year,season)

  bktBiomass<-readRDS("vignettes/westBrook/bktBiomass.rds")
  bntBiomass<-readRDS("vignettes/westBrook/bntBiomass.rds")

  core<-bktBiomass[core] %>%
        bntBiomass[.] %>%
    setkey(tag,detectionDate)

  if(r=="wb mitchell"){
    core[is.na(bntBiomass),bntBiomass:=0]
  }

  core[,lengthInit:=approx(observedLength,n=length(observedLength))$y,by=tag]
  core[!is.na(observedLength),lengthInit:=NA]
  core[,trueLength:=observedLength]
  core[,firstObs:=detectionDate==min(detectionDate),by=tag]
  growthObsRows<-core[!is.na(observedLength)&firstObs==FALSE,,which=T]


for(crossVal in 1:3){
  workingCore<-core
  workingCore[sample(growthObsRows,round(length(growthObsRows)*0.1)),observedLength:=NA]

  jagsData<-createJagsData(data.frame(workingCore) %>%
                             addEnvironmental()) %>%
    .[c("firstObsRows",
        "nFirstObsRows",
        "evalRows",
        "nEvalRows",
        "nAllRows",
        "lengthDATA",
        "ind",
        "season")]


  workingCore<-workingCore[,.(tag,tagIndex,detectionDate,observedLength,lengthInit,trueLength,
                bktBiomass,bntBiomass,medianFlow)]



  t<-temp %>%
    .[river==r] %>%
    .[,date:=as.Date(datetime)]%>%
    .[datetime>=min(workingCore$detectionDate)&datetime<=max(workingCore$detectionDate)] %>%
    setkey(datetime)

  workingCore[,':='(time=which.min(
    abs(
      t$datetime-as.POSIXct(paste0(detectionDate," 12:00:00"))
    )
  )),
  by=.(detectionDate)]


  jagsData$tempDATA<-t$temperature
  jagsData$nTimes<-nrow(t)
  jagsData$time<-workingCore$time
  jagsData$nInd<-max(workingCore$tagIndex)
  jagsData$isSpring<-as.numeric(jagsData$season==2)
  jagsData$bktBiomassDATA<-scale(workingCore$bktBiomass)[,1]
  jagsData$bntBiomassDATA<-scale(workingCore$bntBiomass)[,1]
  jagsData$biomassDATA<-scale(workingCore[[paste0(sp,"Biomass")]])[,1]
  jagsData$flowDATA<-scale(workingCore$medianFlow)[,1]
  jagsData$tOptMean<-priors[[sp]][["tOptMean"]]
  jagsData$tOptPrecision<-priors[[sp]][["tOptPrecision"]]
  jagsData$ctMaxMean<-priors[[sp]][["ctMaxMean"]]
  jagsData$ctMaxPrecision<-priors[[sp]][["ctMaxPrecision"]]
  jagsData$ctUltimate<-priors[[sp]][["ctUltimate"]]

  jagsData$meanFlow<-mean(workingCore$medianFlow,na.rm=T)
  jagsData$sdFlow<-sd(workingCore$medianFlow,na.rm=T)
  jagsData$meanBktBiomass<-mean(workingCore$bktBiomass,na.rm=T)
  jagsData$sdBktBiomass<-sd(workingCore$bktBiomass,na.rm=T)
  jagsData$meanBntBiomass<-mean(workingCore$bntBiomass,na.rm=T)
  jagsData$sdBntBiomass<-sd(workingCore$bntBiomass,na.rm=T)

  inits<-function(){list(lengthData=workingCore$lengthInit,
                         maxAdd=5,
                         tOpt=16,
                         beta1=0.015,
                         beta2=-6e-05,
                         eps=0.003)
  }

  parsToMonitor<-c("beta1","beta2","beta3","beta4","beta5",
                   "tOpt",'ctMax',"sigma","ranMonth","sigmaMonth",
                   'eps',"sigmaInd","lengthExp","b")
  out<-fitModel(jagsData=jagsData,inits=inits,parallel=T,params=parsToMonitor,
                nb=5000,ni=7000,nt=1,modelFile="modelLengthField.R",codaOnly="lengthExp")
  saveRDS(out,file=paste0("vignettes/westBrook/results/outCrossVal",crossVal,sp,toupper(substr(r,1,1)),substr(r,2,nchar(r)),".rds"))
  saveRDS(workingCore,file=paste0("vignettes/westBrook/results/coreCrossVal",crossVal,sp,toupper(substr(r,1,1)),substr(r,2,nchar(r)),".rds"))
  print(out)
  rm(out)
}

