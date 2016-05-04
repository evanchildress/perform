model{

  #performance parameters
  maxAdd~dnorm(5,0.01)T(0,50)
  ctMax<-maxAdd+tOpt
  tOpt~dnorm(11,0.01)T(0,50)
  sigma~dunif(0,10)

  #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
  beta1~dnorm(0,100)T(0,)
  beta2~dnorm(0,1000)T(,0)
  beta3~dnorm(0,1000)
  # beta4~dnorm(0,1000)
  # beta5~dnorm(0,1000)

  eps~dunif(0,0.1)
  #   #individual random effect on grMax
  #      for(f in 1:nInd){
  #        ranInd[f]~dnorm(0,tauInd)
  #      }
  #      tauInd<-1/pow(sigmaInd,2)
  #      sigmaInd~dunif(0,1)

#     #random month effect on grMax
    #   for(m in 1:nMonths){
    #     ranMonth[m]~dnorm(0,tauMonth)
    #     #ranMonth[m]<-exp(ranMonth[m])
    #   }
    #   tauMonth<-1/pow(sigmaMonth,2)
    #   sigmaMonth~dunif(0,0.1)
    #
    # for(i in 1:nEvalRows){
    #   for(m in 1:nMonths){
    #     monEff[evalRows[i]-1,m]<-ranMonth[m]*propMonth[evalRows[i],m]
    #   }
    #   monthEffect[evalRows[i]-1]<-sum(monEff[evalRows[i]-1,])
    # }
#        for(be in 1:5){
#          b[be]~dnorm(0,1000)
#        }
#
#     for(d in 1:366){
#       seasonalEffect[d]<-b[1]+b[2]*d+b[3]*d^2+b[4]*d^3+b[5]*d^4
#     }

    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>tOpt,1-((tempDATA[t]-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[t]-tOpt)/(2*sigma))^2))
    }



    for(i in 1:nEvalRows){

      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]]])
#
#       st[evalRows[i]-1]<-min(yday[evalRows[i]-1],yday[evalRows[i]])
#       en[evalRows[i]-1]<-max(yday[evalRows[i]-1],yday[evalRows[i]])
#
#       se[evalRows[i]-1]<-ifelse(yday[evalRows[i]-1]<yday[evalRows[i]],
#                                 sum(seasonalEffect[st[evalRows[i]-1]:en[evalRows[i]-1]]),
#                                 #sum(seasonalEffect[yday[evalRows[i]-1]:yday[evalRows[i]]]),
#                                 sum(seasonalEffect[yday[evalRows[i]-1]:365])+
#                                 sum(seasonalEffect[1:yday[evalRows[i]]])-
#                                 2*sum(seasonalEffect[st[evalRows[i]-1]:en[evalRows[i]-1]]))

      grExp[evalRows[i]-1]<-(beta1+beta2*lengthDATA[evalRows[i]-1]+
                               beta3*flowDATA[evalRows[i]-1])
                               # monthEffect[evalRows[i]-1]) #random individual effect)
                            *p[evalRows[i]-1] #von bert
                #+c
                #+densityEffect[i] #density effect


      lengthDATA[evalRows[i]]~dnorm(lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1],eps*p[evalRows[i]-1])
    }

  for(i in 1:nEvalRows){
    lengthExp[i]<-lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1]
  }
}
