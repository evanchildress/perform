#'Creates model file for performance model
#'@export
createModelRandom<-function(fileOut="model.txt"){
  cat("model{

    #performance parameters
    ctMax~dnorm(21,1)
    tOpt~dnorm(11,0.5)
    sigma~dunif(0,15)

    #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
    beta[1]~dnorm(0,10000)T(0,0.1)
    beta[2]~dnorm(0,1000000)T(-0.1,0)

    # #individual random effect on grMax
    #    for(f in 1:nInd){
    #      ranInd[f]~dnorm(0,tauInd)
    #    }
    #    tauInd<-1/pow(sigmaInd,2)
    #    sigmaInd~dunif(0,2)

    #random month effect on grMax
      for(m in 1:6){
        ranMonth[m]~dnorm(0,tauMonth)
      }
      tauMonth<-1/pow(sigmaMonth,2)
      sigmaMonth~dunif(0,2)

    for(i in 1:nEvalRows){
      for(m in 1:6){
        monEff[evalRows[i]-1],m]<-ranMonth[m]*propMonth[evalRows[i],m]
      }
      monthEffect[evalRows[i]-1]<-sum(monEff[evalRows[i]-1,])
    }

    #density effect on grMax


    for(i in 1:nEvalRows){

      grMax[evalRows[i]-1]<-beta[1]+beta[2]*length[evalRows[i]-1] #von bert
                +monthEffect[evalRows[i]-1] #random month effect
                #+ranInd[ind[i]] #random individual effect
                #+densityEffect[i] #density effect
    }
    eps~dunif(0,1000)
    tauEps<-1/pow(eps,2)

    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>tOpt,1-(((tempDATA[t])-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[t]-tOpt)/(2*sigma))^2))
    }
    for(i in 1:nEvalRows){
      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]]])
    }

    for(i in 1:nFirstObsRows){
      length[firstObsRows[i]]~dnorm(90,0.001)
      lengthDATA[firstObsRows[i]]~dnorm(length[firstObsRows[i]],9/1)
    }
    for(i in 1:nEvalRows){
      gr[evalRows[i]-1]~dnorm(p[evalRows[i]-1]*grMax[evalRows[i]-1],tauEps)
      length[evalRows[i]]<-length[evalRows[i]-1]+gr[evalRows[i]-1]
      lengthDATA[evalRows[i]]~dnorm(length[evalRows[i]],9/1)
    }

    # grDailyMax<-grMax*24
  }",file=fileOut)
}
