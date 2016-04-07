model{
    #performance parameters
    maxAdd~dnorm(5,0.001)T(0,50)
    ctMax<-tOpt+maxAdd
    tOpt~dnorm(11,0.001)T(0,50)
    sigma~dunif(0,10)

    #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
    beta1~dnorm(0,10000)T(0,0.1)
    beta2~dnorm(0,1000000000)T(-0.1,0)

    #variation on growth rate at tOpt
    eps~dnorm(0,1e-5)T(0,0.01)
    #eps<-0.00000000001
    tauEps<-1/pow(eps,2)

    obsTau<-100000000000

    for(t in 1:nTimes){
      perf[t]<-ifelse(temp[t]>tOpt,1-(((temp[t])-tOpt)/(tOpt-ctMax))^2,
                    exp(-((temp[t]-tOpt)/(2*sigma))^2))
    }
    for(i in 1:nObs){

      err[i]~dnorm(0,tauEps)#variation on growth at tOpt
      grMax[i]<-beta1+beta2*startLength[i]+err[i] #von bert plus noise

      p[i]<-sum(perf[startTime[i]:endTime[i]]) #summed performance over growth period

      grExp[i]<-p[i]*grMax[i]
      gr[i]~dnorm(grExp[i],obsTau)
    }
  }
