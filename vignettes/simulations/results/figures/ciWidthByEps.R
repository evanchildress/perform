res<-readRDS("vignettes/simulations/results/mergedSimResults.rds") %>%
  .[ctMax!=17.5&converged==T]
res[,ciWidth:=q97.5-q2.5]
res[,ciPercent:=abs(ciWidth/mean)]
tiff.par("vignettes/simulations/results/figures/ciWidthByEps.tif",lwd=2)
plot(density(res[parameter=="tOpt"&converged==T&sampleFreq!="annual"&
                     ctMax!=17.5&eps==0.0015,ciWidth]),
     type='l',col="red",main="",ylim=c(0,17.5),
     xlab=bquote("Width of 95% Credible Interval for"~T[opt]))
points(density(res[parameter=="tOpt"&converged==T&sampleFreq!="annual"&
                     ctMax!=17.5&eps==0.00075,ciWidth]),type='l',col="black")
points(density(res[parameter=="tOpt"&converged==T&sampleFreq!="annual"&
                     ctMax!=17.5&eps==0.000375,ciWidth]),type='l',col="blue")
legend(1,12,c(expression(epsilon~"= 2.5%"),
              expression(epsilon~"= 5%"),
              expression(epsilon~"= 10%")),
              col=c("blue","black","red"),lty=1,bty='n',lwd=2)
dev.off()
