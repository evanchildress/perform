library(perform)
library(plotHacks)

mcc<-data.frame(temp=c(7.1,9.8,12.4,15.4,17.9,19.5),
           growth=c(2.43,3.83,4.49,4.54,4.01,3.00),
           study="mccormick")
dwy<-data.frame(temp=c(4,7,10,13,16,19),
                growth=(c(1.5,5.4,19.5,33.5,36.5,28.5)-0.6)/140,
                study="dwyer")
dwy<-data.frame(temp=c(4,7,10,13,16,19),
                growth=c(1.5,5.4,19.5,33.5,36.5,28.5),
                study="dwyer")

cha<-data.frame(temp=c(15.507,17.732,20.029,22.355,24.348),
                growth=c(3.19,2.848,2.176,0.967,-0.875),
                study="chadwick")
mch<-data.frame(temp=c(8.009,8.009,8.008,12.021,12.021,12.006,
                       16.018,16.016,16.0278,20.022,20.021,20.021),
                growth=c(0.039,0.040,0.037,0.054,0.055,0.049,0.061,
                         0.055,0.047,0.042,0.037,0.035),
                study="mcmahon")

bal<-data.frame(temp=round(c(3.632,3.663,7.882,9.243,9.341,9.338,9.31,9.321,
                       13.203,13.247,13.276,13.239,17.136,17.167,17.202,
                       17.203,17.204,21.153,21.151,21.17,21.171,17.037,
                       17.117,17.085,9.162,9.165,9.13,13.123,13.147)),
                growth=c(2.573,1.915,7.068,8.561,8.668,9.409,9.382,10.179,
                           15.129,14.642,14.497,13.133,14.237,9.991,8.251,8.023,
                           7.738,2.715,-0.275,-1.615,-2.014,5.881,
                           8.257,9.909,7.44,7.779,5.786,10.221,10.561)/7,
                study="baldwin")

lab<-data.table(rbind(mcc,dwy,cha,mch,bal))
lab[,temp:=round(temp,1)]
lab[,rawGrowth:=growth]
lab[,growth:=growth/max(growth),by=study]

labMeans<-lab[,.(growth=mean(growth)),by=.(study,temp)]
labMeans[,growth:=growth/max(growth),by=study]
labMeans[,pch:=which(study==unique(labMeans$study)),by=study]

#lab<-lab[study!="baldwin"]
setkey(lab,study,temp)

plot(growth~temp,data=lab,col=as.factor(study))
#points(growth~temp,data=lab[study=="baldwin"],type='l')


jagsData<-list(tempDATA=labMeans[,temp],
               perfDATA=labMeans[,growth],
               n=nrow(labMeans[]))
parsToSave<-c("tOpt","ctMax","sigma","eps")
out<-jags(jagsData,inits=NULL,parameters.to.save=parsToSave,
          model.file="jagsPerformanceCurve.R",n.chains=3,n.iter=10000,
          n.burnin = 8000,n.thin=1)
tiff.par("vignettes/westBrook/results/figures/bktLabPerformance.tif",
         width=3.5,height=3.5,mgp=c(1.8,0.5,0),mar=c(2.7,2.7,0,0))
plot(NA,xlim=c(0,25),ylim=c(-0.5,1),
     xlab=bquote(Temperature~(degree*C)),
     ylab="Relative Performance")
# for(i in sample(length(out$sims.list$tOpt),500,replace=T)){
#   points(predictPerformance(0:25,out$sims.list$tOpt[i],
#                             out$sims.list$ctMax[i],
#                             out$sims.list$sigma[i]),type='l',col=gray(0.5,0.3))
# }
points(predictPerformance(seq(0,25,0.1),median(out$sims.list$tOpt),
                          median(out$sims.list$ctMax),
                          median(out$sims.list$sigma))~seq(0,25,0.1),
       type='l',lwd=2)
points(growth~temp,data=labMeans,pch=pch)
legend(-0.5,-0.05,c("Baldwin 1957","McCormick et al. 1972","Dwyer et al. 1983",
                "McMahon et al. 2007","Chadwick 2012"),
       pch=c(5,1,2,4,3),cex=0.75,pt.cex=1)
dev.off()

saveRDS(out,"vignettes/westBrook/results/outBktLabPerformance.rds")
