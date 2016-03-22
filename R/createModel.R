#'Creates model file for performance model
#'
createModel<-function(fileOut="model.txt"){
  cat("model{
    #performance parameters
    ctMax~dnorm(16,0.01)
    tOpt~dnorm(22,1)
    sigma~dunif(0,15)

    #maximum hourly growth rate (which is then scaled by the performance function)
    # grMax~dunif(0,1)
for(g in 1:2){
    grInt[g]~dnorm(0,0.01)
    grSlope[g]~dnorm(0,0.01)
}
    for(i in 1:nObs){
     grMax[i]<-grInt+grSlope*medianLengthDATA[i]
    }

    eps~dunif(0,1000)
    tauEps<-1/pow(eps,2)

    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>tOpt,1-(((tempDATA[t])-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[t]-tOpt)/(2*sigma))^2))
    }
    for(i in 1:nObs){
      p[i]<-sum(perf[startTime[i]:endTime[i]])
    }

    for(i in 1:nObs){
      gr[i]<-p[i]*grMax[i]
      grDATA[i]~dnorm(gr[i],tauEps)
    }

    grDailyMax<-grMax*24
  }",file=fileOut)
}
