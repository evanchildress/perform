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

source("perform/pSim.R")
source("perform/fitModel.R")
source("perform/predictPerformance.R")

results<-NULL
iter<-1

#simulation settings
opts<-c(10,15,20)
maxes<-c(16,20,24,27)
epses<-c(0.000375,0.00075,0.0015)
seas<-c(T)
rivs<-c("wb jimmy")

#fewer sims for testing
#opts<-c(10)
#maxes<-c(16,20)
#epses<-c(0.000375)
#seas<-c(T)
#rivs<-c("wb jimmy")

totalSims<-length(opts)*length(maxes)*length(epses)*length(seas)*length(rivs)

for(opt in opts){
  for(tMax in maxes){
    for(r in rivs){
      for(e in epses){
        for(s in seas){
          if(tMax<=opt){next}
	  cat("starting iteration ",iter," of ",totalSims)
          re<-pSim(tOpt=opt,ctMax=tMax,sigma=4,eps=e,seasonal=s,nYoy=50,river=r,modelFile="perform/model.txt")
          results<-rbind(results,re)
	  iter<-iter+1
        }
      }
    }
  }
}
results$simNum<-simNum
saveRDS(results,paste0("~/perform/output/pSim",simNum,".rds"))
cat("sim ",simNum," complete at ",Sys.time())
