#'Fits the performance model
#'
#'@export

fitModel<-function(ni=11000,
                   nb=10000,
                   nc=3,
                   nt=3,
                   modelFile="model.txt",
                   jagsData,
                   inits=NULL,
                   parallel=T){
  params<-c("gr","ctMax","tOpt","sigma","eps","beta",
            "sigmaInd","ranMonth","sigmaMonth")

  out<-jags(data=jagsData,inits=NULL,params,"model.txt",n.chains=nc,n.iter=ni,
            n.thin=nt,n.burnin=nb,parallel=parallel,codaOnly="gr")
  return(out)
}
