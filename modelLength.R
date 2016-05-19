model{

  #Priors
  #performance parameters
  maxAdd~dnorm(5,0.01)T(0,50)
  ctMax<-maxAdd+tOpt
  tOpt~dnorm(11,0.01)T(0,50)
  sigma~dunif(0,10)

  #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
  beta1~dnorm(0,100)T(0,)
  beta2~dnorm(0,1000)T(,0)

  eps~dunif(0,0.1)

  #Likelihood
    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>tOpt,1-((tempDATA[t]-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[t]-tOpt)/(2*sigma))^2))
    }

    for(i in 1:nEvalRows){
      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]]])
      grExp[evalRows[i]-1]<-(beta1+beta2*lengthDATA[evalRows[i]-1])*p[evalRows[i]-1] #von bert
      lengthDATA[evalRows[i]]~dnorm(lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1],eps*p[evalRows[i]-1])
    }

  for(i in 1:nEvalRows){
    lengthExp[i]<-lengthDATA[evalRows[i]-1]+grExp[evalRows[i]-1]
  }
}
