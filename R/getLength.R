core<-createCoreData("electrofishing") %>%
  addTagProperties() %>%
  filter(species=="bkt" & !is.na(observedLength) & river=="wb jimmy") %>%
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
