library(perform)
library(plotHacks)
reconnect()

rivers<-c("wb jimmy","wb mitchell","wb obear","west brook")
#r<-"wb jimmy"
r<-"west brook"
sp<-"bkt"

core<-createCoreData("electrofishing",columnsToAdd="observedWeight") %>%
  data.table() %>%
  .[,n:=.N,by=tag] %>%
  .[n>1] %>%
  .[,n:=NULL] %>%
  data.frame() %>%
  addTagProperties() %>%
  createCmrData() %>%
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

lengthWeight<-lm(log(observedLength)~log(observedWeight),data=core)

mcMahonGrowth<-function(weight,time,tempData){
  wExp<-rep(as.numeric(NA),length(weight))
  for(i in 2:length(weight)){
    wExp[i]<-sum((0.0131*tempData[time[i-1]:time[i],temperature]-
                  0.0005*tempData[time[i-1]:time[i],temperature]^2-0.0362)/24)+
              weight[i-1]

  }
  return(wExp)
}
core<-core[!is.na(observedWeight)]
core<-core[,n:=.N,tag][n>1][,n:=NULL]
setkey(core,tag,detectionDate)
core[,wExp:=mcMahonGrowth(observedWeight,time,tempData=t),by=tag]
core[,lExp:=exp(predict(lengthWeight,data.frame(observedWeight=wExp)))]
setkey(core,river,year,season)


bktBiomass<-readRDS("vignettes/westBrook/bktBiomass.rds")
bntBiomass<-readRDS("vignettes/westBrook/bntBiomass.rds")

core<-bktBiomass[core] %>%
  bntBiomass[.] %>%
  setkey(tag,detectionDate)
core[,residual:=observedWeight-wExp]

gr<-core[,.(gObsWeight=diff(observedWeight),
            gExpWeight=wExp[2:length(wExp)]-observedWeight[1:(length(wExp)-1)],
            gObs=diff(observedLength),
            gExp=lExp[2:length(lExp)]-observedLength[1:(length(lExp)-1)],
            startLength=observedLength[1:(length(wExp)-1)],
            startDate=detectionDate[1:(length(wExp)-1)],
            endDate=detectionDate[2:length(wExp)],
            endAge=ageInSamples[2:length(wExp)]),
         by=tag]
gr<-gr[endAge<=3]
lengthSumStats<-core[!is.na(observedLength)&!is.na(lExp)&ageInSamples<=3,
                     .(relativeBias=sum(lExp-observedLength)/.N/mean(observedLength),
                       rmse=sqrt(sum((lExp-observedLength)^2)/.N))]
weightSumStats<-core[!is.na(observedWeight)&!is.na(wExp)&ageInSamples<=3,
                     .(relativeBiasWeight=sum(wExp-observedWeight)/.N/mean(observedWeight),
                       rmseWeight=sqrt(sum((wExp-observedWeight)^2)/.N))]
growthSumStats<-gr[!is.na(gObs)&!is.na(gExp)&!is.na(gObsWeight)&!is.na(gExpWeight),
                   .(relativeBias=sum(gExp-gObs)/.N/mean(gObs),
                     rmse=sqrt(sum((gExp-gObs)^2)/.N),
                     relativeBiasWeight=sum(gExpWeight-gObsWeight)/.N/mean(gObsWeight),
                     rmseWeight=sqrt(sum((gExpWeight-gObsWeight)^2)/.N))]

tiff.par("vignettes/westBrook/results/figures/mcMahonGrowth.tif")
plot(gExp~gObs,data=gr,pch=16,col=gray(0.5,0.5),
     ylab="McMahon Model Predicted Growth (mm)",
     xlab="Observed Growth (mm)")
abline(0,1)
dev.off()
