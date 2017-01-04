library(plotHacks)
tiff.par("vignettes/westBrook/results/figures/individualEffect.tif",
         mfrow=c(1,2),mar=c(2.5,3.8,0,0),width=6.5,height=3.5)
for(sp in c("bkt","bnt")){

  out<-readRDS(paste0("vignettes/westBrook/results/out",sp,"West brook",".rds"))
  core<-readRDS(paste0("vignettes/westBrook/results/core",sp,"West brook",".rds"))

  x<-seq(min(core$observedLength,na.rm=T),max(core$observedLength,na.rm=T,1))
  y<-array(NA,dim=c(length(x),length(out$mean$beta1)))
  #y<-array(NA,dim=c(length(x),10))
  for(i in 1:ncol(y)){
    y[,i]<-out$mean$beta1[i]+out$mean$beta2*x
  }

  plot(NA,xlim=range(x),ylim=c(0,0.023),xlab=bquote(Initial~Size~(L[i])),
       ylab="")
  title(ylab=bquote(Hourly~Growth~Rate~at~T[opt]~(G[opt])),line=2.5)
  panelLabel(c("a","b")[which(sp==c("bkt","bnt"))],yadj=0.03)

  for(i in 1:ncol(y)){
    points(y[,i]~x,type='l',col=gray(0.1,0.05))
  }
}

dev.off()
