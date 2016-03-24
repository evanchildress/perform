#' calculates growth over repeated observations
#'
#'@export

getGrowth<-function(sp='bkt',rivers='wb jimmy'){
  dat<-createCoreData("electrofishing") %>%
    addTagProperties %>%
    filter(species==sp & !is.na(observedLength) & river %in% rivers) %>%
    addSampleProperties() %>%
    data.frame() %>%
    data.table()

  dat<-dat[tag %in% (dat[,.N,by=tag] %>% .[N>1,tag]) ] %>%
    .[,detectionDate:=as.Date(detectionDate)] %>%
    setkey(tag,detectionDate) %>%
    .[,age:=as.numeric(detectionDate-as.Date(paste0(cohort,"-03-01")))]

  growth<-dat[,.(growth=diff(observedLength),
              startDate=detectionDate[1:(.N-1)],
              endDate=detectionDate[2:.N],
              startLength=observedLength[1:(.N-1)],
              endLength=observedLength[2:.N],
              startAge=age[1:(.N-1)],
              endAge=age[2:.N]),
           ,by=tag]

  growth[,time:=as.numeric(endDate-startDate)]
  growth[,':='(mmPerDay=growth/time,
               medianAge=(startAge+endAge)/2,
               medianLength=(startLength+endLength)/2)]
  return(growth)
}
