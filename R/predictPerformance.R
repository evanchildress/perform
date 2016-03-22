#' Predicts relative performance given known parameter inputs
#'
predictPerformance<-function(temp,tOpt,ctMax,sigma){
  perf<-rep(NA,length(temp))
  #gaussian for less than tOpt
  perf[which(temp<=tOpt)]<- exp(-((temp[which(temp<=tOpt)]-tOpt)/(2*sigma))^2)

  #quadratic decline for greater than tOpt
  perf[which(temp>tOpt)]<-1-((temp[which(temp>tOpt)]-tOpt)/(tOpt-ctMax))^2
  return(perf)
}
