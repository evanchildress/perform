model{

  #Priors
  #performance parameters
  ctMax~dnorm(24,0.0001)T(tOpt,28)
  tOpt~dnorm(15,0.0001)T(0,28)
  sigma~dunif(0,10)

  eps~dunif(0,5)
  tau<-1/pow(eps,2)
  #Likelihood
  for(i in 1:n){
    perfExp[i]<-ifelse(tempDATA[i]>tOpt,1-((tempDATA[i]-tOpt)/(tOpt-ctMax))^2,
                       exp(-((tempDATA[i]-tOpt)/(2*sigma))^2))
    perfDATA[i]~dnorm(perfExp[i],tau)T(,1.00001)
  }
}
