model{

  #performance parameters
  maxAdd~dnorm(5,0.001)T(0,100)
  ctMax<-maxAdd+tOpt
  tOpt~dnorm(11,0.0001)T(0,100)
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

    # #individual random effect on grMax
    #    for(f in 1:nInd){
    #      ranInd[f]~dnorm(0,tauInd)
    #    }
    #    tauInd<-1/pow(sigmaInd,2)
    #    sigmaInd~dunif(0,2)

#     #random month effect on grMax
#       for(m in 1:nMonths){
#         ranMonth[m]~dnorm(0,tauMonth)
#       }
#       tauMonth<-1/pow(sigmaMonth,2)
#       sigmaMonth~dunif(0,2)
#
#     for(i in 1:nEvalRows){
#       for(m in 1:nMonths){
#         monEff[evalRows[i]-1,m]<-ranMonth[m]*propMonth[evalRows[i],m]
#       }
#       monthEffect[evalRows[i]-1]<-sum(monEff[evalRows[i]-1,])
#     }

    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>tOpt,1-((tempDATA[t]-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[t]-tOpt)/(2*sigma))^2))
    }

  for(i in 1:nFirstObsRows){
    length[firstObsRows[i]]~dnorm(83.5,0.001)
    lengthDATA[firstObsRows[i]]~dnorm(length[firstObsRows[i]],10)
  }

    for(i in 1:nEvalRows){
      errScaled[i]~dnorm(0,tauEpsScaled)#variation on growth at tOpt
      err[i]<-errScaled[i]/10000

      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]]])

      grMax[evalRows[i]-1]<-beta1+beta2*length[evalRows[i]-1]+err[i] #von bert
                #+monthEffect[evalRows[i]-1] #random month effect
                #+ranInd[ind[i]] #random individual effect
                #+densityEffect[i] #density effect


      gr[evalRows[i]-1]<-grMax[evalRows[i]-1]*p[evalRows[i]-1]

      length[evalRows[i]]<-length[evalRows[i]-1]+gr[evalRows[i]-1]
      lengthDATA[evalRows[i]]~dnorm(length[evalRows[i]],10)
    }
}
