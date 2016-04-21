library(perform)

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

boxplot(mean~ctMax+trueValue,data=res[parameter=="tOpt"&converged==T&seasonal==T])
boxplot(mean~trueValue+tOpt,data=res[parameter=="ctMax"&converged==T&seasonal==T&eps==0.0015])

