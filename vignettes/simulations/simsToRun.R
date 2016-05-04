res<-readRDS("vignettes/simulations/results/mergedSimResults.rds")
toRun<-res[parameter=="tOpt",.(numConverged=sum(converged),
                                   totalRun=length(converged)),by=.(tOpt,ctMax,eps,sampleFreq)] %>%
  .[ctMax!=17.5 & numConverged<100] %>%
  .[,":="(needed=100-numConverged,
          propConverged=numConverged/totalRun)] %>%
  .[,numToRun:=ceiling(needed/propConverged)] %>%
  .[,iterations:=ifelse(numToRun>500,30000,16000)] %>%
  .[numToRun>500,numToRun:=100]
toRun<-toRun[rep(1:nrow(toRun),numToRun)]

toRun[,whichSim:=as.numeric(rep(1:(ceiling(nrow(toRun)/50)),each=50))]

saveRDS(toRun,"vignettes/simulations/simsToRun.rds")
