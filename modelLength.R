model{

  #performance parameters
  maxAdd~dnorm(5,0.001)T(0,100)
  ctMax<-maxAdd+tOpt
  tOpt~dnorm(11,0.001)T(0,100)
  sigma~dunif(0,10)

  #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
  beta1~dnorm(0,100)T(0,)
  beta2~dnorm(0,1000)T(,0)

  eps~dunif(0,0.1)
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
    lengthDATA[firstObsRows[i]]~dnorm(83.5,0.001)
  }



    for(i in 1:nEvalRows){
      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]]])

      grExp[evalRows[i]-1]<-(beta1+beta2*lengthDATA[evalRows[i]-1])*p[evalRows[i]-1] #von bert
                #+monthEffect[evalRows[i]-1] #random month effect
                #+ranInd[ind[i]] #random individual effect
                #+densityEffect[i] #density effect


      lengthDATA[evalRows[i]]~dnorm(lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1],eps*p[evalRows[i]-1])
    }
}
