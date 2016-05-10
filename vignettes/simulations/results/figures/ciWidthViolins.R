library(plotHacks)
library(data.table)
library(dplyr)



res<-readRDS("vignettes/simulations/results/mergedSimResults.rds") %>%
  .[ctMax!=17.5&converged==T&sampleFreq!="annual"] %>%
  .[sampleFreq!="monthly"|tOpt!=8.5|ctMax!=20.5]%>%
  .[,ciWidth:=q97.5-q2.5]%>%
  .[,ciPercent:=abs(ciWidth/mean)]

plotDensity<-function(par,response="ciWidth",opt=c(8.5,13,16.5,19),max=c(20.5,23.5,26.5),
                      err=0.000375,freq=c("seasonal","monthly","daily"),
                      add=F,...){
  dat<-res[parameter %in% par &
           tOpt %in% opt &
           ctMax %in% max &
           eps %in% err &
           sampleFreq %in% freq]
  fun<-plot
  if(add){fun<-points}
  fun(density(dat[[response]]),type='l',...)
}

colors<-c("black","red","blue","gray")

tiff.par("vignettes/simulations/results/figures/ciWidthByTOpt.tif",
         mfrow=c(3,1),width=3.5,height=4,oma=c(0.2,0.7,0,0))
lab<-1
for(s in c("daily","monthly","seasonal")){
  violin("ciWidth","tOpt",res[parameter=="tOpt"&sampleFreq==s],col='gray',
         ylim=c(0,1.5),names=unique(res$tOpt),ylab="",xlab="")
  letter<-c("a","b","c")[lab]
  lab<-lab+1
  panelLabel(bquote(bold(.(letter))),xadj=0.02)
}
mtext(bquote("Width of 95% CI for"~T[opt]),2,
      outer=T,las=0,cex=0.8,line=-0.7)
mtext(bquote(T[opt]),1,outer=T,las=0,cex=0.8,line=-0.7,adj=0.55)
dev.off()


tiff.par("vignettes/simulations/results/figures/ciWidthByCtMax.tif",
         mfrow=c(3,1),oma=c(0.2,0.25,0,0),width=3.5,height=4)
lab<-1
for(m in c(20.5,23.5,26.5)){
  violin("ciWidth","tOpt",res[parameter=="ctMax"&sampleFreq=="seasonal"&
                                ctMax==m&eps==0.000375],
         col='gray',ylim=c(0,27),names=c(8.5,13.5,16.5,19),xlab="",
          ylab="",pchMed=NA,colMed="black")
  letter<-c("a","b","c")[lab]
  lab<-lab+1
  panelLabel(bquote(bold(.(letter))),xadj=0.02)
  #title(main=bquote(CT[max]==.(m)),line=-0.5)
}
mtext(bquote("Width of 95% Credible Interval for"~CT[max]),
      2,outer=T,las=0,cex=0.8,line=-0.9)
mtext(bquote(T[opt]),1,outer=T,las=0,cex=0.8,line=-0.8,adj=0.55)
dev.off()
