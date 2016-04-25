#' simulates length based on performance curves then fits a model
#'
#' @export

#simulation setttings
pSim<-function(tOpt,ctMax,sigma,eps,sampleFreq="annual",
               river="wb jimmy",modelFile="modelLength.R",
               returnRaw=F,na=500,nb=5000,ni=7000,parallel=T){

  controls<-list(annual=list(nYoy=60,
                             pSurv=0.76^(4/1),
                             startDate=as.Date("2001-10-01"),
                             endDate=as.Date("2016-01-01"),
                             by="year"),
                 seasonal=list(nYoy=60,
                               pSurv=0.76,
                               startDate=as.Date("2001-10-01"),
                               endDate=as.Date("2016-01-01"),
                               by="quarter"),
                 monthly=list(nYoy=40,
                              pSurv=0.76^(4/12),
                              startDate=as.Date("2007-10-01"),
                              endDate=as.Date("2016-01-01"),
                              by="month"),
                 daily=list(nYoy=15,
                            pSurv=0.76^(4/365),
                            startDate=as.Date("2007-10-01"),
                            endDate=as.Date("2008-09-30"),
                            by="day"))


  pDetect<-1
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
    .[date>=controls[[sampleFreq]]$startDate] %>%
    setkey(datetime)

  t[,performance:=predictPerformance(temperature,tOpt,ctMax,sigma)]


  sampleDates<-data.table(date=seq(controls[[sampleFreq]]$startDate,
                                   controls[[sampleFreq]]$endDate,
                                   by=controls[[sampleFreq]]$by)) %>%
    .[,sample:=1:nrow(.)] %>%
    setkey(sample)

  t[,sample:=sum(date>=sampleDates$date),by=date]

  perf<-t[,.(perf=sum(performance)),by=sample]

  core<-data.table(sample=rep(sampleDates[month(date)==10&mday(date)==1,sample],
                              each=controls[[sampleFreq]]$nYoy)) %>%
    .[,length:=rnorm(nrow(.),80,6)] %>%
    .[,tag:=1:nrow(.)] %>%
    setkey(sample) %>%
    .[,.SD[sampleDates],by=tag]
  for(tg in 1:max(core$tag)){
    surv<-rbinom(365,1,controls[[sampleFreq]]$pSurv)
    lastSample<-max(c(suppressWarnings(min(which(surv==0)))-1,1))
    for(s in core[tag==tg&!is.na(length),min(sample)]:
        min(c(core[tag==tg&!is.na(length),sample]+lastSample),
            max(sampleDates$sample))){
      startLength<-core[tag==tg&sample==s,length]
      err<-rnorm(1,0,eps)
      core[tag==tg&sample==s+1,":="(
        length=startLength+(beta1+startLength*beta2+err)*
          perf[sample==s,perf],
        noise=err)
        ]
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
    .[,':='(time=min(which(t$sample==sample))),by=sample]


  # t[,month:=round((month(date)+1)/3)]
  # hoursPerMonth<-t[date>=as.Date("2003-01-01")&date<=as.Date("2014-12-31"),
  #                     .(hours=.N/length(unique(year(date)))),
  #                     by=month] %>%
  #   setkey(month)
  #
  # propMonth<-core[,.(tag,time)] %>%
  #   .[,startTime:=as.numeric(shift(time)),by=tag] %>%
  #   .[,obs:=1:nrow(.)] %>%
  #   .[is.na(startTime),startTime:=time-1]
  #
  # propMonth<-propMonth[!is.na(startTime),t[startTime:time,.N,by=month] %>%
  #                        setkey(month) %>%
  #                        .[hoursPerMonth] %>%
  #                        .[is.na(N),N:=0] %>%
  #                        .[,propMonth:=N/hours] %>%
  #                        .[,.(propMonth,month)],
  #                      by=.(obs)] %>%
  #   melt(id.vars=c("month","obs")) %>%
  #   acast(obs~month)

  jagsData<-list(lengthDATA=core$length,
                 firstObsRows=which(core$firstObs==1),
                 nFirstObsRows=length(which(core$firstObs==1)),
                 evalRows=which(core$firstObs==0),
                 nEvalRows=length(which(core$firstObs==0)),
                 tempDATA=t$temperature,
                 nTimes=nrow(t),
                 time=core$time
                 #,propMonth=propMonth
                 #,nMonths=max(hoursPerMonth$month)
                 )

  lengthInit<-approx(core$length,n=length(core$length))$y
  lengthInit[!is.na(core$length)]<-NA

#  createLengthModel()
  inits<-function(){list(lengthDATA=lengthInit,
                         beta1=0.015,
                         beta2=-6e-5,
                         eps=0.0015,
                         tOpt=15,
                         maxAdd=5,
                         sigma=4)}
  inits<-function(){list(lengthDATA=lengthInit
                         )
  }

  parsToSave<-c("tOpt","ctMax","sigma","beta1","beta2","eps")

  out<-fitModel(jagsData=jagsData,inits=inits,modelFile=modelFile,
                parallel=T,nb=nb,ni=ni,nt=1,params=parsToSave)
  if(returnRaw){return(out)}

  res<-out$summary %>%
       data.table(keep.rownames=T) %>%
       setnames(c("parameter","mean","sd","q2.5","q25",
                  "q50","q75","q97.5","rHat","nEff","overlap0","f")) %>%
       .[parameter %in% parsToSave[1:(length(parsToSave))],
         .(parameter,mean,q2.5,q50,q97.5,rHat)]
  res$trueValue<-unlist(mget(parsToSave[1:(length(parsToSave))]))
  res$sampleFreq<-sampleFreq
  res$nObs<-nrow(core[!is.na(length)])-length(unique(core$tag))
  res$riverTemp<-r

  return(res)
}


