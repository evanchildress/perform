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

tiff.par("results/figures/tOptBoxPlots.tif")
boxplot(mean~ctMax+trueValue,data=res[parameter=="tOpt"&converged==T&seasonal==T])
dev.off()

tiff.par("results/figures/ctMaxBoxPlots.tif",mfrow=c(3,1))
boxplot(mean~trueValue+tOpt,data=res[parameter=="ctMax"&converged==T&seasonal==T&eps==0.00075],main="eps = 5%")
boxplot(mean~trueValue+tOpt,data=res[parameter=="ctMax"&converged==T&seasonal==T&eps==0.0015],main="eps = 10%")
boxplot(mean~trueValue+tOpt,data=res[parameter=="ctMax"&converged==T&seasonal==T&eps==0.003],main="eps = 20%")
dev.off()

