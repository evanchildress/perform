#'Fits the performance model
#'
#'

fitModel<-function(ni=5000,
                   nb=4000,
                   nc=3,
                   nt=1,
                   modelFile="model.txt",
                   jagsData){
  params<-c("ctMax","tOpt","sigma","eps","grDailyMax")

  out<-jags(data=jagsData,inits=NULL,params,"model.txt",n.chains=nc,n.iter=ni,
            n.thin=nt,n.burnin=nb,parallel=T)
  return(out)
}
