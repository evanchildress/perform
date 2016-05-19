library(perform)
library(plotHacks)
library(ggplot2)

res<-readRDS("vignettes/simulations/results/mergedSimResults.rds")
res<-res[sampleFreq!="monthly"|tOpt!=8.5|ctMax!=20.5]
res<-res[ctMax!=17.5&converged==T]



bias<-res[converged==T&ctMax!=17.5,.(msd=sum(mean-trueValue)/.N,
       propWithinCI=sum(withinCI)/.N,
       cv=mean((q97.5-q2.5)/5/abs(mean)),
       .N,
       relativeBias=sum(mean-trueValue)/.N/mean(trueValue),
       p=pbinom(sum(withinCI),.N,0.95)),
    by=.(sampleFreq,tOpt,ctMax,eps,parameter)]
setkey(bias,sampleFreq,tOpt,ctMax)
tiff.par("vignettes/simulations/results/figures/biasHist.tif")
hist(bias[sampleFreq!="annual"&parameter!="ctMax",relativeBias],breaks=50)
hist(bias[sampleFreq!="annual"&parameter=="ctMax",relativeBias],breaks=50)
hist(bias[sampleFreq!="annual",propWithinCI],breaks=50)
hist(bias[sampleFreq!="annual"&parameter=="ctMax",relativeBias],breaks=50)
dev.off()

coverage<-res[converged==T,round(sum(withinCI)/.N,2),by=.(sampleFreq,parameter)] %>%
  melt(id.vars=c("sampleFreq","parameter")) %>%
  dcast(parameter~sampleFreq)
write.csv(coverage,"vignettes/simulations/results/coverageTable.csv")

mse<-res[sampleFreq!="annual",.(mse=var(mean)+(sum(mean-trueValue)/.N)^2),
         by=.(parameter,trueValue)]

annualMse<-res[sampleFreq=="annual",.(mse=sum((mean-trueValue)^2)/.N),
         by=parameter]

ctMse<-res[sampleFreq!="annual"&parameter=="ctMax",
           .(mse=sum((mean-trueValue)^2)/.N),
           by=.(tOpt,ctMax)]
tiff.par("vignettes/simulations/results/figures/ctMaxMse.tif",
         width=3,height=3)
plot(mse~tOpt,data=ctMse,pch=19,cex=(ctMax-18)^(1/2)/2,
     col=gray(0.5,0.5),ylab=bquote(MSE~of~CT[max]),
     xlab=bquote(T[opt]~(degree*C)))
legend(10,30,as.character(c(20.5,23.5,26.5)),pch=19,col=gray(0.5,0.5),
       bty='n',pt.cex=(c(20.5,23.5,26.5)-18)^(1/2)/2)
text(10,31,bquote(CT[max]),pos=4)
dev.off()
