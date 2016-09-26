model{

  #performance parameters
  ctMax~dnorm(ctMaxMean,ctMaxPrecision)T(tOpt,ctUltimate)
  tOpt~dnorm(tOptMean,tOptPrecision)T(0,ctUltimate)
  sigma~dunif(0,10)

  #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
  # beta1~dnorm(0,100)#T(0,1000)
  beta2~dnorm(0,1000)T(0,)
  beta3~dnorm(0,1000)
  beta4~dnorm(0,1000)
  beta5~dnorm(0,1000)

  eps~dunif(0,0.1)

    #individual random effect on grMax
    gamma~dnorm(0,0.0001)T(0,)
    psi~dunif(0,1)

    tauInd<-1/pow(sigmaInd,2)
    sigmaInd~dunif(0,1)

       for(f in 1:nInd){
         ranSlope[f]~dnorm(beta2,tauInd)T(0,)
         bla[f]<-pow(ranSlope[f],psi)
         beta1[f]<-gamma*bla[f]

       }




    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>tOpt,1-((tempDATA[t]-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[t]-tOpt)/(2*sigma))^2))
    }



    for(i in 1:nEvalRows){

      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]]])

      grExp[evalRows[i]-1]<-(beta1[ind[evalRows[i]-1]]-
                             ranSlope[ind[evalRows[i]-1]]*lengthDATA[evalRows[i]-1]+
                               beta3*flowDATA[evalRows[i]-1]+
                               beta4*bktBiomassDATA[evalRows[i]-1]+
                               beta5*bntBiomassDATA[evalRows[i]-1])
                            *p[evalRows[i]-1] #von bert



      lengthDATA[evalRows[i]]~dnorm(lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1],eps*p[evalRows[i]-1])
    }

  for(i in 1:nEvalRows){
    lengthExp[i]<-lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1]
  }
}
