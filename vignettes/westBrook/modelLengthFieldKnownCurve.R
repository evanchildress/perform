model{

  #performance parameters
  # tOpt<-14.27
  # ctMax<-23.78
  # sigma<-4.72

  #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
  beta1~dnorm(0,100)#T(0,1000)
  beta2~dnorm(0,1000)#T(-1000,0)
  beta3~dnorm(0,1000)
  beta4~dnorm(0,1000)
  beta5~dnorm(0,1000)

  eps~dunif(0,0.1)
    #individual random effect on grMax
       for(f in 1:nInd){
         ranInd[f]~dnorm(0,tauInd)
       }
       tauInd<-1/pow(sigmaInd,2)
       sigmaInd~dunif(0,1)

    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>14.27,1-((tempDATA[t]-14.27)/(14.27-ctMax))^2,
                    exp(-((tempDATA[t]-14.27)/(2*sigma))^2))
    }



    for(i in 1:nEvalRows){

      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]]])

      grExp[evalRows[i]-1]<-(beta1+beta2*lengthDATA[evalRows[i]-1]+
                               beta3*flowDATA[evalRows[i]-1]+
                               beta4*bktBiomassDATA[evalRows[i]-1]+
                               beta5*bntBiomassDATA[evalRows[i]-1]+
                               ranInd[ind[evalRows[i]]])
                            *p[evalRows[i]-1] #von bert



      lengthDATA[evalRows[i]]~dnorm(lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1],eps*p[evalRows[i]-1])
    }

  for(i in 1:nEvalRows){
    lengthExp[i]<-lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1]
  }
}
