t<-readRDS("C:/Users/Evan/Desktop/Conte/trout_yoy/cjsInputs/tempData.rds") %>%
  .[river=="wb jimmy"&date>=as.Date("1997-06-01")&date<as.Date("2015-06-01")]

tOpt<-c(11,11,16,16)
ctMax<-c(20,25,20,25)
sigma<-4

grMax<-0.015

t[,growth1:=predictPerformance(temperature,tOpt[1],ctMax[1],sigma)*grMax]
t[,growth2:=predictPerformance(temperature,tOpt[2],ctMax[2],sigma)*grMax]
t[,growth3:=predictPerformance(temperature,tOpt[3],ctMax[3],sigma)*grMax]
t[,growth4:=predictPerformance(temperature,tOpt[4],ctMax[4],sigma)*grMax]

sampleDates<-as.Date(apply(expand.grid(1998:2015,c("-12-01","-04-01","-06-10","-09-10")),1,paste0,collapse=""))
#sampleDates<-as.Date(apply(expand.grid(1998:2015,c("-06-10")),1,paste0,collapse=""))
t[,sample:=sum(date>=sampleDates),by=date]

gr<-t[,.(growth1=sum(growth1),
         growth2=sum(growth2),
         growth3=sum(growth3),
         growth4=sum(growth4),
         meanTemp=mean(temperature)),by=sample]
insetCol<-"cornflowerblue"
mainCol<-"gray90"
tiff.par("vignettes/growthSimulations4.tif",mfrow=c(2,2),bg='black',col=mainCol)
for(bla in 1:4){
  par(col=mainCol,col.lab=mainCol,col.axis=mainCol)
  plot(get(paste0("growth",bla))~meanTemp,data=gr,col=gray(0.7,0.5),pch=19,
       ylab="Absolute Growth (mm)",xlab="Mean Temp")
  par(new=T,col=insetCol,col.lab=insetCol,col.axis=insetCol)
  plot(predictPerformance(0:22,tOpt[bla],ctMax[bla],sigma)~I(0:22),
       type='l',xlim=c(-5,100),ylim=c(-6,1),axes=F,ylab=NA,xlab=NA)
  axis(1,seq(0,20,10),pos=-1,col=insetCol)
  axis(2,seq(-1,1,1),pos=-1,col=insetCol)
}
dev.off()


tiff.par("vignettes/perfCurveExample.tif",col='gray90',bg='black',lwd=2)
plot(predictPerformance(0:22,tOpt[1],ctMax[1],sigma)~I(0:22),type='l',
     xlab="Temperature",ylab="Relative Performance")
dev.off()

tiff.par("vignettes/perfOverTimeSim.tif",col='gray90',bg='black',lwd=1,mgp=c(2,0.5,0),mar=c(1.5,3,0,0))
plot(I(growth1/grMax)~datetime,data=t[year(date)==2011],type='l',
     xlab="",ylab="Relative Performance")
dev.off()

lInf<-300
lZero<-0.2
k=-0.8
age<-seq(0.2,4,0.01)

length<-lInf*(1-exp(k*(age-lZero)))
tiff.par("vignettes/vonBertSim.tif",col='gray90',bg='black',mfrow=c(1,2),width=10,mgp=c(1,0.5,0),lwd=2)
plot(length~age,type='l',xlab="Age",ylab="Length",xaxt='n',yaxt='n')
plot(diff(length)~length[1:(length(length)-1)],type='l',
     xlab="Length",ylab="Growth",xaxt='n',yaxt='n')
dev.off()


tiff.par("vignettes/growthOverTimeSim.tif",col='gray90',bg='black',lwd=1,mgp=c(2,0.5,0),mar=c(1.5,3,0,0))
plot(growth1~datetime,data=t[year(date)==2011],type='l',
     xlab="",ylab="Growth (mm/hr)")
dev.off()
