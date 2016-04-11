#' simulates length based on performance curves then fits a model
#'
#' @export

#simulation setttings
pSim<-function(tOpt,ctMax,sigma,eps,nYoy=60,seasonal=T,
               river,modelFile="modelLength.R"){
  pSurv<-0.76
  if(!seasonal){pSurv<-pSurv^4}
  pDetect<-0.6
# tOpt<-15
# ctMax<-20
# sigma<-4
#
beta1<-0.015
beta2<- -6e-05


#
# nYoy<-10

r<-river

#  temp<-tbl(conDplyr,"data_hourly_temperature") %>%
t<-temp %>%
  .[river==r] %>%
  .[,date:=as.Date(datetime)]%>%
  .[date>as.Date("2001-09-30")] %>%
  setkey(datetime)

t[,performance:=predictPerformance(temperature,tOpt,ctMax,sigma)]

if(seasonal){
  sampleDates<-data.table(date=seq(as.Date("2001-10-01"),
                                   as.Date("2016-01-01"),
                                   by="quarter")) %>%
    .[,season:=match(month(date),c(1,4,7,10))] %>%
    .[,sample:=1:nrow(.)] %>%
    setkey(sample)
} else {
  sampleDates<-data.table(date=seq(as.Date("2001-10-01"),
                                   as.Date("2016-01-01"),
                                   by="year")) %>%
    .[,season:=4] %>%
    .[,sample:=1:nrow(.)] %>%
    setkey(sample)
}

  t[,sample:=sum(date>=sampleDates$date),by=date]

  perf<-t[,.(perf=sum(performance)),by=sample]

  core<-data.table(sample=rep(sampleDates[season==4,sample],each=nYoy)) %>%
    .[,length:=rnorm(nrow(.),80,6)] %>%
    .[,tag:=1:nrow(.)] %>%
    setkey(sample) %>%
    .[,.SD[sampleDates],by=tag]
  for(tg in 1:max(core$tag)){
    surv<-rbinom(20,1,pSurv)
    lastSample<-max(c(suppressWarnings(min(which(surv==0)))-1,1))
    for(s in core[tag==tg&!is.na(length),min(sample)]:
             min(c(core[tag==tg&!is.na(length),sample]+lastSample),
                 max(sampleDates$sample))){
      startLength<-core[tag==tg&sample==s,length]
      core[tag==tg&sample==s+1,length:=startLength+
                                      rnorm(1,(beta1+startLength*beta2),eps)*
                                        perf[sample==s,perf]]
    }
  }

  core<-core[!is.na(length)] %>%
    .[,detected:=rbinom(nrow(.),1,pDetect)] %>%
    .[detected==0,length:=NA] %>%
    .[,detected:=NULL] %>%
    .[,firstDate:=suppressWarnings(min(date[which(!is.na(length))])),by=tag] %>%
    .[,firstObs:=date==firstDate] %>%
    .[date>=firstDate,] %>%
    .[,firstDate:=NULL] %>%
    .[,lastDate:=suppressWarnings(max(date[which(!is.na(length))])),by=tag] %>%
    .[date<=lastDate] %>%
    .[,lastDate:=NULL] %>%
    .[,N:=sum(!is.na(length)),by=tag] %>%
    .[N>1] %>%
    .[,N:=NULL] %>%
    .[,':='(time=which.min(
                        abs(
                          t$datetime-as.POSIXct(paste0(date," 00:00:00"))
                            )
                          )),
        by=.(tag,date)]


  t[,month:=round((month(date)+1)/3)]
  hoursPerMonth<-t[date>=as.Date("2003-01-01")&date<=as.Date("2014-12-31"),
                      .(hours=.N/length(unique(year(date)))),
                      by=month] %>%
    setkey(month)

  propMonth<-core[,.(tag,time)] %>%
    .[,startTime:=as.numeric(shift(time)),by=tag] %>%
    .[,obs:=1:nrow(.)] %>%
    .[is.na(startTime),startTime:=time-1]

  propMonth<-propMonth[!is.na(startTime),t[startTime:time,.N,by=month] %>%
                         setkey(month) %>%
                         .[hoursPerMonth] %>%
                         .[is.na(N),N:=0] %>%
                         .[,propMonth:=N/hours] %>%
                         .[,.(propMonth,month)],
                       by=.(obs)] %>%
    melt(id.vars=c("month","obs")) %>%
    acast(obs~month)

  jagsData<-list(lengthDATA=core$length,
                 firstObsRows=which(core$firstObs==1),
                 nFirstObsRows=length(which(core$firstObs==1)),
                 evalRows=which(core$firstObs==0),
                 nEvalRows=length(which(core$firstObs==0)),
                 propMonth=propMonth,
                 tempDATA=t$temperature,
                 nTimes=nrow(t),
                 time=core$time,
                 nMonths=max(hoursPerMonth$month)
                 )

#  createLengthModel()
  inits<-function(){list(beta1=0.015,
                         beta2= -6e-05,
                         tOpt=tOpt,
                         ctMax=ctMax)}

  parsToSave<-c("tOpt","ctMax","sigma","beta1","beta2","eps")

  out<-fitModel(jagsData=jagsData,inits=inits,modelFile=modelFile,
                parallel=T,nb=5000,ni=7000,nt=2,params=parsToSave)

  res<-out$summary %>%
       data.table(keep.rownames=T) %>%
       setnames(c("parameter","mean","sd","q2.5","q25",
                  "q50","q75","q97.5","rHat","nEff","overlap0","f")) %>%
       .[parameter %in% parsToSave[1:(length(parsToSave))],
         .(parameter,mean,q2.5,q50,q97.5,rHat)]
  res$trueValue<-unlist(mget(parsToSave[1:(length(parsToSave))]))
  res$seasonal<-seasonal
  res$nObs<-nrow(core[!is.na(length)])-length(unique(core$tag))
  res$riverTemp<-r

  return(res)
}


