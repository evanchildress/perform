library(perform)

rivers<-c("west brook","wb jimmy","wb mitchell","wb obear")
for(r in rivers){
  assign(paste0("out",which(r==rivers)),readRDS(paste0("vignettes/outW",substr(r,2,nchar(r)),".rds")))
  assign(paste0("gr",which(r==rivers)),getGrowth(rivers=r))

  get(paste0("gr",which(r==rivers)))[,predicted:=apply(get(paste0("out",which(r==rivers)))$sims.list$gr,2,mean)]
  get(paste0("gr",which(r==rivers)))[,residuals:=predicted-growth]
}

tiff.par("results/figures/performanceCurves.tif",col='gray90',bg='black',
         mfrow=c(2,2),mar=c(2.5,2.5,1,0))
for(r in rivers){
  plot(NA,xlim=c(0,22),ylim=c(-1,1),xlab="temp",ylab="performance",main=r)
  out<-get(paste0("out",which(r==rivers)))
  for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
    points(predictPerformance(0:22,tOpt=out$sims.list$tOpt[i],
                              sigma=out$sims.list$sigma[i],
                              ctMax=out$sims.list$ctMax[i])~I(0:22),
           type='l',col='gray')
  }
}
dev.off()


tiff.par("results/figures/ontogenyOfGMax.tif",col='gray90',bg='black',
         mfrow=c(2,2),mar=c(2.5,2.5,1,0))
for(r in rivers){
  plot(mmPerDay~startLength,data=get(paste0("gr",which(r==rivers))),
       col=gray(0.5,0.5),pch=19,xlab="startLength",ylab="growth (mm/day)",main=r)

  out<-get(paste0("out",which(r==rivers)))
  x<-seq(60,300)
  for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
    points(predictVonBert(x,
                          out$sims.list$beta[i,1],
                          out$sims.list$beta[i,2],
                          derivative=T)*24~x,
           type='l',col='blue')
  }
}
dev.off()

tiff.par("results/figures/obsVsPred.tif",col='gray90',bg='black',
         mfrow=c(2,2),mar=c(2.5,2.5,1,0))
for(r in rivers){
  plot(predicted~growth,data=get(paste0("gr",which(r==rivers))),
       col=gray(0.5,0.5),pch=19,xlab="observed growth (mm)",ylab="predicted growth (mm)",main=r)
  abline(0,1)
  a<-lm(predicted~growth,data=get(paste0("gr",which(r==rivers))))
  rsq<-round(summary(a)$r.squared,2)
  text(10,80,bquote(R^2~"="~.(rsq)))
}
dev.off()

tiff.par("results/figures/randomMonth.tif",col='gray90',bg='black',
         mfrow=c(2,2),mar=c(2.5,2.5,1,0))
for(r in rivers){
  plot(NA,xlim=c(1,12),ylim=c(-0.01,0.01),xlab="month",ylab="random month effect",main=r)
  out<-get(paste0("out",which(r==rivers)))
  for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
    points(out$sims.list$ranMonth[i,]~seq(1,12,2),type='l')
  }
}
dev.off()
