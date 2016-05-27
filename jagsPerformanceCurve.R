model{

  #Priors
  #performance parameters
  ctMax~dnorm(20,0.001)T(tOpt,28)
  tOpt~dnorm(11,0.001)T(0,28)
  sigma~dunif(0,10)

  eps~dunif(0,5)

  #Likelihood
    for(i in 1:n){
      perfExp[i]<-ifelse(tempDATA[i]>tOpt,1-((tempDATA[i]-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[i]-tOpt)/(2*sigma))^2))
      perfDATA[i]~dnorm(perfExp[i],eps)
    }
}
