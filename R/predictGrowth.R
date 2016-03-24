#'Gives growth rates from a Gompertz model when provided paramters
#'
#'@export
predictGompertz<-function(x,a,b,c){
  a*b*c*exp(b*exp(c*x))*exp(c*x)
}

#'Gives growth rates (or size) from a von Bert model provided parameters
predictVonBert<-function(x,lInf,k,lZero=NULL,derivative=F){
  if(!derivative) lInf*(1-exp(k))

  #if(derivative) k*(lInf-x)
  if(derivative) lInf+k*x
}
