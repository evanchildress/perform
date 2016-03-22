#'Fits the performance model
#'
#'

fitModel<-function(ni=11000,
                   nb=10000,
                   nc=3,
                   nt=1,
                   modelFile="model.txt",
                   jagsData,
                   inits=NULL,
                   parallel=T){
  params<-c("ctMax","tOpt","sigma","eps","grInt","grSlope")

  out<-jags(data=jagsData,inits=NULL,params,"model.txt",n.chains=nc,n.iter=ni,
            n.thin=nt,n.burnin=nb,parallel=parallel)
  return(out)
}
