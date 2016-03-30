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
#for(opt in c(10,15,20)){
#  for(tMax in c(16,20,24,27)){
#    for(r in c("wb obear","wb mitchell")){
#      for(e in c(2,5,8)){
for(opt in c(10)){
  for(tMax in c(16)){
    for(r in c("wb obear")){
      for(e in c(2)){
        for(s in c(T)){
          if(tMax<=opt){next}
	  cat("starting iteration ",iter," of 60")
          pSim(tOpt=opt,ctMax=tMax,sigma=4,eps=e,seasonal=s,nYoy=4,river=r,modelFile="perform/model.txt")
          results<-rbind(results,r)
	  iter<-iter+1
        }
      }
    }
  }
}
saveRDS(results,paste0("~/perform/output/pSim",simNum,".rds"))
cat("sim ",simNum," complete")