#'Fits the performance model
#'
#'@export

fitModel<-function(ni=11000,
                   nb=10000,
                   nc=3,
                   nt=3,
                   na=500,
                   modelFile="model.txt",
                   jagsData,
                   inits=NULL,
                   parallel=T,
                   params=NULL){
  if(is.null(params)){
  params<-c("gr","ctMax","tOpt","sigma","eps","beta1","beta2",
            "sigmaInd","ranMonth","sigmaMonth")
  }

  out<-jags(data=jagsData,inits=NULL,params,modelFile,n.adapt=na,n.chains=nc,n.iter=ni,
            n.thin=nt,n.burnin=nb,parallel=parallel,codaOnly="gr")
  return(out)
}
