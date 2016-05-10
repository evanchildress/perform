library(data.table)
library(dplyr)
res<-rbind(readRDS("vignettes/simulations/results/pSimResults.rds"),
           readRDS("vignettes/simulations/results/pSimResults2.rds"),
           readRDS("vignettes/simulations/results/pSimResults3.rds"))%>%
  data.table() %>%
  .[,index:=rep(1:(nrow(.)/6),each=6)] %>%
  .[,converged:=all(rHat<1.1),by=index] %>%
  setkey(index) %>%
  .[,":="(tOpt=trueValue[which(parameter=="tOpt")],
          ctMax=trueValue[which(parameter=="ctMax")],
          eps=trueValue[which(parameter=="eps")]),
    by=index] %>%
  .[,withinCI:=q2.5<trueValue&q97.5>trueValue]

saveRDS(res,"vignettes/simulations/results/mergedSimResults.rds")
