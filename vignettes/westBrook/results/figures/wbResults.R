library(plotHacks)

parameter=c("beta1","beta2","beta3","beta4","beta5","tOpt",
                            "ctMax","sigma",'eps')

r<-"west brook"
tiff.par("vignettes/westBrook/results/figures/wbResults.tif",
         mfcol=c(3,2),mar=c(2.8,2.8,1,0),mgp=c(1.7,0.5,0))
for(sp in c("bkt","bnt")){

    out<-readRDS(paste0("vignettes/westBrook/results/out",sp,"West brook",".rds"))
    plot(NA,xlim=c(0,22),ylim=c(-1,1),
         xlab=bquote(Temperature~(degree*C)),ylab="Relative Performance",
         main=c("Brook Trout","Brown Trout")[which(sp==c("bkt",'bnt'))])
    for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
      points(predictPerformance(0:22,tOpt=out$sims.list$tOpt[i],
                                sigma=out$sims.list$sigma[i],
                                ctMax=out$sims.list$ctMax[i])~I(0:22),
             type='l',col=gray(0.45))
    }

    core<-readRDS(paste0("vignettes/westBrook/results/core",sp,"West brook",".rds"))
    core[,firstObs:=detectionDate==min(detectionDate),by=tag] %>%
      .[firstObs==F,predictedLength:=apply(out$sims.list$lengthExp,2,mean)]

    gr<-core[,.(obsGrowth=diff(observedLength),
                predGrowth=predictedLength[2:length(observedLength)]-
                 observedLength[1:(length(observedLength)-1)],
                date=detectionDate[2:length(detectionDate)],
                startLength=observedLength[1:(length(observedLength)-1)],
                obsGrowthRate=diff(observedLength)/as.numeric(diff(detectionDate))),
             by=tag] %>%
             .[,residual:=obsGrowth-predGrowth]

    plot(I(observedLength/10)~I(predictedLength/10),data=core,
         ylab="Observed Length (cm)",
         xlab="Predicted Length (cm)",pch=19,col=gray(0.45,0.5))
    abline(0,1)
    # a<-lm(predictedLength~observedLength,core)
    # text(75,150,bquote(R^2==.(round(summary(a)$r.squared,2))))

    plot(I(obsGrowth/10)~I(predGrowth/10),data=gr,pch=19,col=gray(0.45,0.5),
         xlab="Observed Growth (cm)",ylab="")
    title(ylab="Predicted Growth (cm)",line=1.5)
    a<-lm(I(obsGrowth/10)~I(predGrowth/10),gr)
    abline(a)
    # abline(0,1,lty=2)
    # text(5,15,bquote(R^2==.(round(summary(a)$r.squared,2))),col='black')


      # plot(obsGrowthRate~startLength,data=gr,
      #      pch=NA,main=r,xlab="Initial Length (mm)",
      #      ylab=bquote(Growth~(mm*day^-1)))
      # x<-seq(50,300)
      # for(i in sample(1:length(out$sims.list$tOpt),300,replace=T)){
      #   b1<-out$sims.list$beta1[i]*24
      #   b2<-out$sims.list$beta2[i]*24
      #   b3<-out$sims.list$beta3[i]*24
      #   b4<-out$sims.list$beta4[i]*24
      #   y<-b1 + b2*x + b3*2.1 + b4*-1.5
      #
      #   points(y~x,type='l')
      # }
      # points(obsGrowthRate~startLength,data=gr,col=gray(0.5,0.5),pch=19)
  res<-data.table(parameter=parameter)
  for(p in res$parameter){
    res[parameter==p,":="(mean=mean(out$sims.list[[p]]),
                          q2.5=quantile(out$sims.list[[p]],0.025),
                          q97.5=quantile(out$sims.list[[p]],0.975))]
  }
  res[!parameter %in% c("ctMax","sigma","tOpt"),":="(mean=mean*24,
                                                     q2.5=q2.5*24,
                                                     q97.5=q97.5*24)]
  res<-rbind(res,
             data.table(parameter="lInf",
                        mean=mean(out$sims.list$beta1/(-out$sims.list$beta2)),
                        q2.5=quantile(out$sims.list$beta1/(-out$sims.list$beta2),0.025),
                        q97.5=quantile(out$sims.list$beta1/(-out$sims.list$beta2),0.975))
  )
  assign(paste0("res",sp),res)
}
dev.off()

res<-merge(resbkt,resbnt,by="parameter",suffixes=c("Bkt","Bnt"))
write.csv(res,"wbParTable.csv")

