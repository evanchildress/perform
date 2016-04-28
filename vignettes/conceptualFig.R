library(perform)
library(plotHacks)
library(plotrix)

layMat<-matrix(data=c(1, 1, 2, 2, 3, 3 ,
                      9 ,9 ,9 ,9 ,14,14,
                      15,4, 4, 5, 5, 6 ,
                      12,10,10,10,10,11,
                      13,7 ,7 ,7, 7, 8),
               nrow=5,ncol=6,byrow=T)
height<-c(3,0.8,3,0.8,0.8)
eqCex<-1.4
layout.show(layout(layMat,heights=height))
tiff.par("vignettes/conceptualFig.tif",width = 6.7,height=7,oma=c(0.1,0.1,0,0),
         mar=c(2.5,3,0,0),cex.lab=1.2)
layout(layMat,heights=height)

# First row
#1. Temperatures
t<-temp[river=="wb jimmy"&year(datetime)==2010]
plot(temperature~datetime,data=t,xaxt='n',
     type='l',xlab="",ylab=bquote(Temperature~(degree*C)))
axisDates<-seq.POSIXt(as.POSIXct("2010-01-01"),as.POSIXct("2011-01-01"),"quarter")
axis(1,axisDates,format(axisDates,"%b"))
text(as.POSIXct("2010-01-10"),22,bquote(bold("a")),cex=1.5)

#2. Performance curve
x<-seq(0,22,0.1)
y<-predictPerformance(x,14,20.5,4)
plot(y~x,type='l',xlab="",
     ylab="",ylim=c(-0.5,1.1))
points(c(14,20.5),c(1,0),pch=16,cex=1)
title(ylab="Relative Performance (P)",xlab=bquote(Temperature~(degree*C)),line=1.7)
par(xpd=NA)
text(14,1.1,bquote(T[opt]))
text(23,0,bquote(CT[max]))
lines(c(20.5,14,14),c(0,0,1),lty=2)
arrows(14,predictPerformance(6,14,20.5,4),6,predictPerformance(6,14,20.5,4),
       code=2,angle=90,length=0.05,lty=c(2))
text(10,predictPerformance(6,14,20.5,4)+0.05,bquote(2*sigma))
par(xpd=F)
text(0.5,1.1,quote(bold("b")),cex=1.5)

#3. Performance Equation
par(xpd=NA)
plot(NA,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
text(0.35,0.85,"(Eq. 1)",cex=eqCex)
text(0.35,0.6,
     bquote(P==bgroup("{",atop(e^{-(frac(temp-T[opt],2*sigma))^2}~"  ;"~temp<=T[opt],
                            1-(frac(temp-T[opt],T[opt]-CT[max]))^2~"  ;"~temp>T[opt]),
                      "")),cex=eqCex)
par(xpd=F)


#4. Performance over time
perf<-predictPerformance(t$temperature,tOpt=14,ctMax=20.5,sigma=4)
par(mar=c(2.5,3,0,0))
plot(perf~t$datetime,xlab="",ylab="",type='l',xaxt='n')
axis(1,axisDates,format(axisDates,"%b"))
title(ylab="P",line=1.8)
abline(v=as.POSIXct(c("2010-02-01","2010-06-01")),lty=2)
text(as.POSIXct("2010-02-10"),-0.5,bquote(t[i]))
text(as.POSIXct("2010-06-10"),-0.5,bquote(t[f]))
text(as.POSIXct("2010-01-10"),1,bquote(bold("c")),cex=1.5)


#5. Von bert
x<-seq(0,250,1)
y<-0.015+x*-6e-5
plot(y~x,ylab="",ylim=c(0,0.02),
     xlab=bquote(Initial~Size~(L[i])),type='l',lwd=2,yaxt='n',xaxt='n')
for(i in 1:500){
  points(I(y+rnorm(length(y),0,0.00075))~x,pch=16,cex=0.3,col=gray(0.6,0.03))
}
par(new=T)
plot(y~x,type='l',lwd=2,yaxt='n',xlab="",ylab="",ylim=c(0,0.02),xaxt='n')
axis(2,seq(0,0.015,0.005),labels=c(0,NA,NA,NA))
axis(1,seq(0,250,50),labels=c(0,NA,NA,NA,NA,NA))
title(ylab=expression(G[opt]),line=1,cex=1.2)
text(4,0.02,bquote(bold("d")),cex=1.5)

#6. Von bert equation
par(xpd=NA)
plot(NA,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
text(-0.5,0.9,"(Eq. 2)",cex=eqCex)
text(-0.5,0.8,bquote(G[opt]==k*(L[infinity]-L[i])+epsilon),cex=eqCex)
text(-0.5,0.7,bquote(epsilon %~% normal(0,sd)),cex=eqCex)
par(xpd=F)
#7.Growth over time
#Plot
# growth<-perf*0.01
# plot(growth~t$datetime,type='l',
#      xlab="",ylab=expression(G[t]),
#      yaxt='n')
# axis(2,seq(-0.005,0.01,0.005),labels=c(NA,0,NA,NA))
par(mar=c(0,0,0,0))
plot(NA,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",axes=F)
text(0.5,0.5,bquote(Size[t[f]]~"="~Size[t[i]]+sum(P[t] %*% G[opt],i=t[i],t[f])),cex=eqCex*1.2)
#par(mar=c(2.5,3,0,0))


#8. Growth summation bit
par(xpd=NA)
plot(NA,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",axes=F)
text(-0.8,0.5,"(Eq. 3)",cex=eqCex)
par(xpd=F)
#text(0.4,0.5,bquote(G[t[i]~to~t[f]]~"="~sum(P[t] %*% G[],i=t[i],t[f])),cex=1.2)

#9. plot arrows between first two rows
#par(mar=c(0,0,0,0))
plot(NA,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
x<-c(0.45,0.65)
radius<-0.06
lw<-3
draw.arc(x=x[1],y=1,radius=radius,2*pi*3/4,pi,lwd=lw,lend=2)
draw.arc(x=x[2],y=1,radius=radius,2*pi,3*pi/2,lwd=lw,lend=2)
lines(x,rep(0.6,2),lwd=lw)
arrows(0.55,0.58,0.55,0,lwd=lw,length=0.15)

#10. plot arrows between bottom two rows

plot(NA,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
x<-c(0.45,0.65)
radius<-0.06
draw.arc(x=x[1],y=1,radius=radius,2*pi*3/4,pi,lwd=lw,lend=2)
draw.arc(x=x[2],y=1,radius=radius,2*pi,3*pi/2,lwd=lw,lend=2)
lines(x,rep(0.6,2),lwd=lw)
arrows(0.55,0.58,0.55,0,lwd=lw,length=0.15)

#11. Equation for Gt
# par(xpd=NA)
# plot(NA,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",axes=F)
# text(-0.2,0.5,bquote(G[t]==P[t] %*% G[opt]),cex=1.2)
# par(xpd=F)

dev.off()
