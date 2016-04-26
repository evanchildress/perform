dat<-createCoreData("electrofishing",columnsToAdd = c("river","observedWeight","observedLength","sampleName")) %>%
  addTagProperties() %>%
  filter(species=="bkt" & !is.na(observedLength)) %>%
  addSampleProperties() %>%
  data.frame() %>%
  data.table() %>%
  .[,stage:=as.numeric(cohort!=round(year+season/3.9)-1)+1]



meanWeight<-dat[,.(meanWeight=mean(observedWeight,na.rm=T)),.(river,year,season,stage)] %>%
  setkey(river,year,season,stage)

abundance<-readRDS("C:/Users/echildress/downloads/bktAdultAlive.rds") %>%
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
  .[,totalBiomass:=yoy+adults]


saveRDS("vignettes/biomass.rds")
