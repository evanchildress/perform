library(perform)
library(plotHacks)

res<-readRDS("results/pSimResults.rds") %>%
  data.table() %>%
  .[,index:=rep(1:(nrow(.)/6),each=6)] %>%
  .[,converged:=all(rHat<1.1),by=index] %>%
  setkey(index) %>%
  .[,":="(tOpt=trueValue[which(parameter=="tOpt")],
          ctMax=trueValue[which(parameter=="ctMax")],
          eps=trueValue[which(parameter=="eps")]),
    by=index] %>%
  .[,withinCI:=q2.5<trueValue&q97.5>trueValue]

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

tiff.par("results/figures/simEstPerf0.00075.tif",mfrow=c(3,3))
for(opt in unique(res$tOpt)){
  for(max in unique(res$ctMax)){
    if(max==16) next
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
    plotIterations(res[converged==T&seasonal==T&eps==0.00075&tOpt==opt&ctMax==max])

  }
}
dev.off()

tiff.par("results/figures/simEstPerf0.0015.tif",mfrow=c(3,3))
for(opt in unique(res$tOpt)){
  for(max in unique(res$ctMax)){
    if(max==16) next
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
    plotIterations(res[converged==T&seasonal==T&eps==0.0015&tOpt==opt&ctMax==max])

  }
}
dev.off()
