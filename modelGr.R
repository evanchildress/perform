model{
    #performance parameters
    maxAdd~dnorm(5,0.001)T(0,50)
    ctMax<-tOpt+maxAdd
    tOpt~dnorm(11,0.001)T(0,50)
    sigma~dunif(0,10)

    #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
    beta1Scaled~dnorm(0,1)T(0,10)
    beta1<-beta1Scaled/100

    beta2Scaled~dnorm(0,1)T(-1,0)
    beta2<-beta2Scaled/10000

    #variation on growth rate at tOpt
    epsScaled~dnorm(0,1)T(0,100)
    eps<-epsScaled/10000
    #eps<-0.00000000001
    tauEpsScaled<-1/pow(epsScaled,2)

    obsTau<-1

    for(t in 1:nTimes){
      perf[t]<-ifelse(temp[t]>tOpt,1-(((temp[t])-tOpt)/(tOpt-ctMax))^2,
                    exp(-((temp[t]-tOpt)/(2*sigma))^2))
    }
    for(i in 1:nObs){

      errScaled[i]~dnorm(0,tauEpsScaled)#variation on growth at tOpt
      err[i]<-errScaled[i]/10000
      grMax[i]<-beta1+beta2*startLength[i]+err[i] #von bert plus noise

      p[i]<-sum(perf[startTime[i]:endTime[i]]) #summed performance over growth period

      grExp[i]<-p[i]*grMax[i]
      gr[i]~dnorm(grExp[i],obsTau)
    }
  }
