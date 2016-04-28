library(perform)
library(plotHacks)

res<-readRDS("vignettes/simulations/results/mergedSimResults.rds")

plotIterations<-function(data){
  x<-seq(0,26.5,0.25)
  for(i in unique(data$index)){
    y<-predictPerformance(x,
                          data[index==i&parameter=="tOpt",mean],
                          data[index==i&parameter=="ctMax",mean],
                          data[index==i&parameter=="sigma",mean])
    points(y~x,type='l',col=gray(0.5,0.5))
  }
  y<-predictPerformance(x,
                        data[index==i&parameter=="tOpt",trueValue],
                        data[index==i&parameter=="ctMax",trueValue],
                        data[index==i&parameter=="sigma",trueValue])
  points(y~x,type='l',col='black')
}

temp99<-temp[river=="wb jimmy"&!is.na(temperature),quantile(temperature,0.99)]
maxTemp<-temp[river=="wb jimmy"&!is.na(temperature),max(temperature)]


tiff.par("vignettes/simulations/results/figures/samplingEffect.tif",mfrow=c(4,4),oma=c(2,2,2,0),
         mar=c(1.5,1.5,0,0))
  for(opt in c(8.5,13,16.5,19)){
    for(fr in c("annual","seasonal","monthly","daily")){
    # for(max in c(17.5,20.5,23.5,26.5)){
      plot(NA,xlim=c(0,26.5),ylim=c(-1,1),xlab="",
           ylab="Relative Performance")
      if(opt==8.5){
        par(xpd=NA)
        title(main=fr,line=1)
        par(xpd=F)
      }
      abline(v=temp99,lty=2)
      abline(v=maxTemp)
#       text(4,-0.4,bquote(T[opt]~"="~.(opt)))
#       text(4,-0.7,bquote(CT[max]~"="~.(max)))
      plotIterations(res[converged==T&sampleFreq==fr&eps==0.00075&tOpt==opt&ctMax==20.5])
    }
  }
  mtext(bquote(Temperature~(degree~C)),1,outer=T,line=0.5)
  mtext("Relative Performance",2,outer=T,line=0.5,las=0)
dev.off()

tiff.par("vignettes/simulations/results/figures/epsEffect.tif",mfrow=c(4,3),oma=c(2,2,2,0),
         mar=c(1.5,1.5,0,0))
for(opt in c(8.5,13,16.5,19)){
  for(e in c(0.000375,0.00075,0.0015)){
    # for(max in c(17.5,20.5,23.5,26.5)){
    plot(NA,xlim=c(0,26.5),ylim=c(-1,1),xlab="",
         ylab="Relative Performance")
    if(opt==8.5){
      par(xpd=NA)
      title(main=e,line=1)
      par(xpd=F)
    }
    abline(v=temp99,lty=2)
    abline(v=maxTemp)
    #       text(4,-0.4,bquote(T[opt]~"="~.(opt)))
    #       text(4,-0.7,bquote(CT[max]~"="~.(max)))
    plotIterations(res[converged==T&sampleFreq=="seasonal"&eps==e&tOpt==opt&ctMax==20.5])
  }
}
mtext(bquote(Temperature~(degree~C)),1,outer=T,line=0.5)
mtext("Relative Performance",2,outer=T,line=0.5,las=0)
dev.off()

tiff.par("vignettes/simulations/results/figures/maxEffect.tif",mfrow=c(4,3),oma=c(2,2,2,0),
         mar=c(1.5,1.5,0,0))
for(opt in c(8.5,13,16.5,19)){
  for(max in c(20.5,23.5,26.5)){
    # for(max in c(17.5,20.5,23.5,26.5)){
    plot(NA,xlim=c(0,26.5),ylim=c(-1,1),xlab="",
         ylab="Relative Performance")
    if(opt==8.5){
      par(xpd=NA)
      title(main=max,line=1)
      par(xpd=F)
    }
    abline(v=temp99,lty=2)
    abline(v=maxTemp)
    #       text(4,-0.4,bquote(T[opt]~"="~.(opt)))
    #       text(4,-0.7,bquote(CT[max]~"="~.(max)))
    plotIterations(res[converged==T&sampleFreq=="seasonal"&eps=="0.000375"&tOpt==opt&ctMax==max])
  }
}
mtext(bquote(Temperature~(degree~C)),1,outer=T,line=0.5)
mtext("Relative Performance",2,outer=T,line=0.5,las=0)
dev.off()



for(er in c(0.000375,0.00075,0.0015)){
  for(fr in c("annual","seasonal","monthly","daily")){
    tiff.par(paste0("vignettes/simulations/results/figures/",er,fr,".tif"),mfrow=c(4,4))
    for(opt in c(8.5,13,16.5,19)){
      for(max in c(17.5,20.5,23.5,26.5)){
        if(max<=opt){
          plot(NA,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
          next
        }
        plot(NA,xlim=c(0,26.5),ylim=c(-1,1),xlab=bquote(Temperature~(degree~C)),
             ylab="Relative Performance")
        abline(v=temp99,lty=2)
        abline(v=maxTemp)
        text(4,-0.4,bquote(T[opt]~"="~.(opt)))
        text(4,-0.7,bquote(CT[max]~"="~.(max)))
        plotIterations(res[converged==T&sampleFreq==fr&eps==er&tOpt==opt&ctMax==max])

      }
    }
    dev.off()

  }
}
