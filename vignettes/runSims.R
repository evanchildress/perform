results<-NULL
for(opt in c(10,15,20)){
  for(tMax in c(16,20,24,27)){
    for(r in c("wb obear","wb mitchell")){
      for(e in c(2,5,8)){
        for(s in c(T)){
          if(tMax<=opt){next}
          pSim(tOpt=opt,ctMax=tMax,sigma=4,eps=e,seasonal=s,river=r)
          results<-rbind(results,r)
        }
      }
    }
  }
}
saveRDS(results,paste0("~/output/pSim",simNum,".rds"))
