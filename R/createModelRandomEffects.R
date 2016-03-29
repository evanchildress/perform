#'Creates model file for performance model
#'@export
createModelRandom<-function(fileOut="model.txt"){
  cat("model{
    #performance parameters
    ctMax~dnorm(21,1)
    tOpt~dnorm(11,0.5)T(4,16)
    sigma~dunif(0,15)

    #maximum hourly growth rate (which is then scaled by the performance function)
     #constant across the life span
      # grMax~dunif(0,1)

     #derivative of the von Bert is linear
      beta[1]~dnorm(0,10000)T(0,0.1)
      beta[2]~dnorm(0,1000000)T(-0.1,0)
    #derivative of the gompertz is: a*b*c*exp(b*exp(c*x))*exp(c*x)
      # beta[1]~dnorm(150,0.01)
      # beta[2]~dnorm(-5,0.1)T(-10,0)
      # beta[3]~dnorm(-0.03,0.1)T(-10,0)
    #individual random effect on grMax
       for(f in 1:nInd){
         ranInd[f]~dnorm(0,tauInd)
       }
       tauInd<-1/pow(sigmaInd,2)
       sigmaInd~dunif(0,2)

    #random month effect on grMax
      for(m in 1:6){
        ranMonth[m]~dnorm(0,tauMonth)
      }
      tauMonth<-1/pow(sigmaMonth,2)
      sigmaMonth~dunif(0,2)

    for(i in 1:nObs){
      for(m in 1:6){
        monEff[i,m]<-ranMonth[m]*propMonth[i,m]
      }
      monthEffect[i]<-sum(monEff[i,])
    }

    #density effect on grMax


    for(i in 1:nObs){
#von Bert
      grMax[i]<-beta[1]+beta[2]*startLengthDATA[i]
                +monthEffect[i] #random month effect
                #+ranInd[ind[i]] #random individual effect
                #+densityEffect[i] #density effect

#Gompertz
     # grMax[i]<-beta[1]*beta[2]*beta[3]*
     #           exp(beta[2]*exp(beta[3]*medianLengthDATA[i]))*
     #           exp(beta[3]*medianLengthDATA[i])
    }

    eps~dunif(0,1000)
    tauEps<-1/pow(eps,2)

    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>tOpt,1-(((tempDATA[t])-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[t]-tOpt)/(2*sigma))^2))
    }
    for(i in 1:nObs){
      p[i]<-sum(perf[startTime[i]:endTime[i]])
    }

    for(i in 1:nObs){
      gr[i]<-p[i]*grMax[i]
      grDATA[i]~dnorm(gr[i],tauEps)
    }

    grDailyMax<-grMax*24
  }",file=fileOut)
}
