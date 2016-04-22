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

results<-NULL
iter<-1

#simulation settings
opts<-c(8.5,13,16.5,19)
maxes<-c(17.5,20.5,23.5,26.5)
epses<-c(0.000375,0.00075,0.0015)
freq<-c("annual","seasonal","monthly","daily")
r<-c("wb jimmy")

#fewer sims for testing
#opts<-c(10)
#maxes<-c(16)
#epses<-c(0.000375)
#seas<-c(T)
#rivs<-c("wb jimmy")

totalSims<-length(opts)*length(epses)*length(seas)*length(r)


#opt<-opts[simNum %% 3 +1]
#tMax<-maxes[round((simNum+1)/3) %% 3 +1]
#e<-epses[round((simNum+14.5)/30) %% 3 +1]

tMax<-maxes[simNum %% 4 +1]
for(opt in opts){
  for(e in epses){
#     for(tMax in maxes){
         for(s in seas){
#           for(r in rivs){
          if(tMax<=opt){
            iter<-iter+1
            next}
	  cat("starting iteration ",iter," of ",totalSims)
          re<-pSimGrowth(tOpt=opt,ctMax=tMax,sigma=4,eps=e,sampleFreq=f,river=r,
                   modelFile="perform/modelGr.R")
	  re$modelIndex<-iter
    results<-rbind(results,re)
	  iter<-iter+1

  }
}
}
results$simNum<-simNum
saveRDS(results,paste0("~/perform/output/pSim",simNum,".rds"))
print(paste("sim ",simNum," complete at ",Sys.time()))
