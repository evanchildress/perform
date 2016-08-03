model{
for(r in 1:nRivers){
  #performance parameters
  ctMax[r]~dnorm(ctMaxMean,ctMaxPrecision)T(tOpt[r],ctUltimate)
  tOpt[r]~dnorm(tOptMean,tOptPrecision)T(0,ctUltimate)
  sigma[r]~dunif(0,10)

  #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
  beta1[r]~dnorm(0,100)#T(0,1000)
  beta2[r]~dnorm(0,1000)#T(-1000,0)
  beta3[r]~dnorm(0,1000)
  beta4[r]~dnorm(0,1000)
  beta5[r]~dnorm(0,1000)

  eps[r]~dunif(0,0.1)
}
    #individual random effect on grMax
       for(f in 1:nInd){
         ranInd[f]~dnorm(0,tauInd)
       }
       tauInd<-1/pow(sigmaInd,2)
       sigmaInd~dunif(0,1)

  #performance
  for(r in 1:nRivers){
    for(t in 1:nTimes){
      perf[t,r]<-ifelse(tempDATA[t,r]>tOpt[r],1-((tempDATA[t,r]-tOpt[r])/(tOpt[r]-ctMax[r]))^2,
                    exp(-((tempDATA[t,r]-tOpt[r])/(2*sigma[r]))^2))
    }
  }



    for(i in 1:nEvalRows){

      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]],riverDATA[evalRows[i]-1]])

grExp[evalRows[i]-1]<-(beta1[riverDATA[evalRows[i]-1]]+
                         beta2[riverDATA[evalRows[i]-1]]*lengthDATA[evalRows[i]-1]+
                         beta3[riverDATA[evalRows[i]-1]]*flowDATA[evalRows[i]-1]+
                         beta4[riverDATA[evalRows[i]-1]]*bktBiomassDATA[evalRows[i]-1]+
                         beta5[riverDATA[evalRows[i]-1]]*bntBiomassDATA[evalRows[i]-1]+
                             ranInd[ind[evalRows[i]]])
                           *p[evalRows[i]-1] #von bert



      lengthDATA[evalRows[i]]~dnorm(lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1],
                                    eps[riverDATA[evalRows[i]-1]]*p[evalRows[i]-1])
    }

  for(i in 1:nEvalRows){
    lengthExp[i]<-lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1]
  }
}
