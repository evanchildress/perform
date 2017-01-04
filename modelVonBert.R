model{
  kMean~dnorm(0,100)T(0,10)
  lInf~dnorm(0,0.001)T(0,1000)
  l0~dnorm(0,1000)T(,lInf)

  sigma~dunif(0,10)
  tau<-1/pow(sigma,2)

  #individual random effect on k
  for(f in 1:nInd){
    k[f]~dnorm(kMean,tauK)
  }
  sigmaK~dunif(0,1)
  tauK<-1/pow(sigmaK,2)

  for(i in 1:nEvalRows){
    lengthExp[i]<-lInf*(1-exp(-k[ind[evalRows[i]]]*age[evalRows[i]]))+
      l0*exp(-k[ind[evalRows[i]]]*age[evalRows[i]])
    lengthDATA[evalRows[i]]~dnorm(lengthExp[i],tau)
  }
}
