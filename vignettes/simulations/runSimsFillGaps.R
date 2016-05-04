library(dplyr)
library(jagsUI)
library(data.table)
library(reshape2)

load("~/perform/wbTemps.RData")
load("~/perform/wbLengths.RData")


#grab the simulation number from the environment passed from the slurm call
simNum<- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric()

#set random seed based on simNum because all iterations were returning identical results
set.seed(simNum)

source("perform/pSimGrowth.R")
source("perform/fitModel.R")
source("perform/predictPerformance.R")


simList<-readRDS("perform/simsToRun.rds") %>%
          .[whichSim==simNum]
results<-NULL

r<-c("wb jimmy")

for(i in 1:nrow(simList)){

	  cat("starting iteration ",i," of ",nrow(simList))
    opt<-simList[i,tOpt]
    tMax<-simList[i,ctMax]
    e<-simList[i,eps]
    sampelFreq<-simList[i,sampleFreq]
          re<-pSimGrowth(tOpt=opt,ctMax=tMax,sigma=4,eps=e,sampleFreq=f,river=r,
                   nb=simList[i,iterations]-4000,ni=simList[i,iterations],modelFile="perform/modelGr.R")
	  re$modelIndex<-i
    results<-rbind(results,re)
}
results$simNum<-simNum
saveRDS(results,paste0("~/perform/output/pSim",simNum,".rds"))
print(paste("sim ",simNum," complete at ",Sys.time()))
