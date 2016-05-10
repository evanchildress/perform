dat<-createCoreData("electrofishing",columnsToAdd = c("river","observedWeight","observedLength","sampleName")) %>%
  addTagProperties() %>%
  filter(species=="bkt" & !is.na(observedLength)) %>%
  addSampleProperties() %>%
  data.frame() %>%
  data.table() %>%
  .[,stage:=as.numeric(cohort!=round(year+season/3.9)-1)+1]

meanWeight<-dat[,.(meanWeight=mean(observedWeight,na.rm=T)),.(river,year,season,stage)] %>%
  setkey(river,year,season,stage)

abundance<-readRDS("C:/Users/Evan/Desktop/Conte/trout_yoy/cjsInputs/bktAdultAlive.rds") %>%
  .[,":="(river=as.numeric(river),
          season=as.numeric(season),
          year=as.numeric(year)+1999,
          stage=as.numeric(stage))] %>%
  .[,river:=c("west brook","wb jimmy","wb mitchell","wb obear")[river]] %>%
  setkey(river,year,season,stage)

biomass<-abundance[meanWeight] %>%
  .[,biomass:=meanWeight*mean] %>%
  .[!is.na(stage),.(river,stage,year,season,biomass)] %>%
  melt(id.vars=c("river","stage","year","season")) %>%
    dcast.data.table(river+year+season~stage) %>%
  setnames(c("1","2"),c("yoy","adults")) %>%
  .[,totalBiomass:=sum(yoy,adults,na.rm=T),by=.(river,year,season)]

a<-lm(totalBiomass~as.factor(year)*as.factor(season)*river,data=biomass)
unsampled<-data.table(river="west brook",totalBiomass=NA,season=c(4,4,4,1),
                      year=c(2005,2007,2012,2015))
unsampled$totalBiomass<-predict(a,data.frame(unsampled[,.(river,year,season)]))

unsampled<-
  rbind(unsampled,
        biomass[((season==3&year==2002)|(season==1&year==2003))&river!="west brook",
                .(totalBiomass=mean(totalBiomass)),by=river] %>%
          .[,":="(season=4,year=2002)]
  )

biomass<-bind_rows(biomass,unsampled) %>% data.frame() %>% data.table() %>%
  .[,.(river,year,season,totalBiomass)] %>%
  setnames("totalBiomass","bktBiomass") %>%
  setkey(river,year,season)
saveRDS(biomass,"vignettes/westBrook/bktBiomass.rds")

###########################################################################
#bnt
dat<-createCoreData("electrofishing",columnsToAdd = c("river","observedWeight","observedLength","sampleName")) %>%
  addTagProperties() %>%
  filter(species=="bnt" & !is.na(observedLength)) %>%
  addSampleProperties() %>%
  data.frame() %>%
  data.table() %>%
  .[,stage:=as.numeric(cohort!=round(year+season/3.9)-1)+1]



meanWeight<-dat[,.(meanWeight=mean(observedWeight,na.rm=T)),.(river,year,season,stage)] %>%
  setkey(river,year,season,stage)

abundance<-readRDS("C:/Users/Evan/Desktop/Conte/trout_yoy/cjsInputs/bktAdultAlive.rds") %>%
  .[,":="(river=as.numeric(river),
          season=as.numeric(season),
          year=as.numeric(year)+1999,
          stage=as.numeric(stage))] %>%
  .[,river:=c("west brook","wb jimmy","wb mitchell","wb obear")[river]] %>%
  setkey(river,year,season,stage)

biomass<-abundance[meanWeight] %>%
  .[,biomass:=meanWeight*mean] %>%
  .[!is.na(stage),.(river,stage,year,season,biomass)] %>%
  melt(id.vars=c("river","stage","year","season")) %>%
  dcast.data.table(river+year+season~stage) %>%
  setnames(c("1","2"),c("yoy","adults")) %>%
  .[,totalBiomass:=sum(yoy,adults,na.rm=T),by=.(river,year,season)]

unsampled<-data.table(river=c("wb jimmy","west brook",'west brook','west brook','west brook'),
                      season=c(4,4,4,4,1),
                      year=c(2002,2005,2007,2012,2015),
                      totalBiomass=c(2283.985,5000,3000,4000,2200))



biomass<-bind_rows(biomass,unsampled) %>% data.frame() %>% data.table() %>%
  .[,.(river,year,season,totalBiomass)] %>%
  setnames("totalBiomass","bntBiomass") %>%
  setkey(river,year,season)
saveRDS(biomass,"vignettes/westBrook/bntBiomass.rds")
