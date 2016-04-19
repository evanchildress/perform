runStan<-function(){
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

nYoy<-60
tOpt<-14
ctMax<-18
sigma<-4
eps<-0.0015
seasonal=T
river<-"wb jimmy"


pSurv<-0.76
if(!seasonal){pSurv<-pSurv^4}

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
  .[date>as.Date("2001-09-30")] %>%
  setkey(datetime)

t[,performance:=predictPerformance(temperature,tOpt,ctMax,sigma)]

if(seasonal){
  sampleDates<-data.table(date=seq(as.Date("2001-10-01"),
                                   as.Date("2016-01-01"),
                                   by="month")) %>%
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
gr<-core[,.(growth=diff(length),
            startTime=time[1:(length(time)-1)],
            endTime=time[2:length(time)],
            startLength=length[1:(length(time)-1)]),by=tag] %>%
  setkey(startTime,endTime)

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

grPeriods<-unique(gr[,.(startTime,endTime)]) %>%
  .[,grPeriod:=1:nrow(.)] %>%
  setkey(startTime,endTime)

gr<-grPeriods[gr]
setkey(gr,tag,startTime)

stanData<-list(gr=gr$growth,
               temp=t$temperature,
               nTimes=nrow(t),
               nObs=nrow(gr),
               startLength=gr$startLength,
               grPeriod=gr$grPeriod,
               nGrPeriods=nrow(grPeriods),
               startTime=grPeriods$startTime,
               endTime=grPeriods$endTime
               #perfDuration=grPeriods$endTime-grPeriods$startTime
)


# inits<-function(){list(beta1=0.015,
#                        beta2=6e-5,
#                        tOpt=14,
#                        ctMax=18,
#                        eps=0.000375,
#                        sigma=4)}

parsToMonitor<-c("tOpt","ctMax","beta1","beta2","eps","sigma")
#parsToMonitor<-c("tOpt","eps","ctMax","beta2")

inits<-function(){list(tOpt=rnorm(1,14,1),
                       maxAdd=rnorm(1,4,1),
                       sigma=rnorm(1,4,1),
                       epsScaled=rnorm(1,0.0015,0.0001)*1000,
                       beta1Scaled=rnorm(1,0.015,0.001)*1000,
                       beta2Scaled= rnorm(1,-6e-5,1e-5)*1000)}

  out<-stan(file="modelGrowthTest.stan",data=stanData,pars=parsToMonitor,
          chains=3,iter=200,warmup=150,thin=1,init=inits,
          control=list(adapt_delta=0.999))
  return(out)
}



