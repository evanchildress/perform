#'Creates model file for performance model
#'@export

createLengthModel<-function(fileOut="model.txt"){
  cat("model{

    #performance parameters
    maxAdd~dnorm(5,0.01)T(0,100)
    ctMax<-maxAdd+tOpt
    tOpt~dnorm(11,0.0001)T(0,100)
    sigma~dunif(0,10)

    #derivative of the von Bert is linear, intercept and slope(with length) of hourly growth rate
    beta1~dnorm(0,10000)T(0,0.1)
    beta2~dnorm(0,1000000000)T(-0.1,0)

    #variation on growth rate at tOpt
    #eps~dnorm(0,0.001)T(0,0.01)
    eps<-0.00000000001
    tauEps<-1/pow(eps,2)

    # #individual random effect on grMax
    #    for(f in 1:nInd){
    #      ranInd[f]~dnorm(0,tauInd)
    #    }
    #    tauInd<-1/pow(sigmaInd,2)
    #    sigmaInd~dunif(0,2)

#     #random month effect on grMax
#       for(m in 1:nMonths){
#         ranMonth[m]~dnorm(0,tauMonth)
#       }
#       tauMonth<-1/pow(sigmaMonth,2)
#       sigmaMonth~dunif(0,2)
#
#     for(i in 1:nEvalRows){
#       for(m in 1:nMonths){
#         monEff[evalRows[i]-1,m]<-ranMonth[m]*propMonth[evalRows[i],m]
#       }
#       monthEffect[evalRows[i]-1]<-sum(monEff[evalRows[i]-1,])
#     }

    for(t in 1:nTimes){
      perf[t]<-ifelse(tempDATA[t]>tOpt,1-((tempDATA[t]-tOpt)/(tOpt-ctMax))^2,
                    exp(-((tempDATA[t]-tOpt)/(2*sigma))^2))
    }

    for(i in 1:nFirstObsRows){
      length[firstObsRows[i]]~dnorm(90,0.001)
      lengthDATA[firstObsRows[i]]~dnorm(length[firstObsRows[i]],10000)
    }

    for(i in 1:nEvalRows){
      p[evalRows[i]-1]<-sum(perf[time[evalRows[i]-1]:time[evalRows[i]]])

      grMax[evalRows[i]-1]<-beta1+beta2*length[evalRows[i]-1] #von bert
                #+monthEffect[evalRows[i]-1] #random month effect
                #+ranInd[ind[i]] #random individual effect
                #+densityEffect[i] #density effect

    for(i in 1:nFirstObsRows){
      length[firstObsRows[i]]~dnorm(83.5,0.001)
      lengthDATA[firstObsRows[i]]~dnorm(length[firstObsRows[i]],9/1)
    }
    for(i in 1:nEvalRows){
      grRate[evalRows[i]-1]~dnorm(grMax[evalRows[i]-1],tauEps)
      #grRate[evalRows[i]-1]<-grMax[evalRows[i]-1]

      gr[evalRows[i]-1]<-grRate[evalRows[i]-1]*p[evalRows[i]-1]

      length[evalRows[i]]<-length[evalRows[i]-1]+gr[evalRows[i]-1]
      lengthDATA[evalRows[i]]~dnorm(length[evalRows[i]],10000)
    }

    # grDailyMax<-grMax*24
  }",file=fileOut)
}
