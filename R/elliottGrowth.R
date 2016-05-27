#'Estimates growth based on the Elliott 1995 model adjusted to have asymptotic growth following Borsuk et al 2006 Ecological Modelling
#'
#'@param w0 is the starting weight in g
#'@param temp is the temperature for the growth period
#'@param t is frequency of temperature measurements (i.e., duration of growth increments)
#'@export

elliottGrowth<-function(w0,temps,dur=1/24){
  Tl<-3.56 #se = =0.04
  Tm<-13.11 # se  = 0.03
  Tu<-19.48 # se = 0.04
  h<-0.308 #0.002
  g<-2.80 #0.02
  wInf<-3133.8

  w<-rep(as.numeric(NA),length(temps)+1)
  w[1]<-w0

  for(i in 1:length(temps)){

  if(temps[i]<Tl|temps[i]>Tu){w[i+1]<-w[i]
  } else {
  if(temps[i]<=Tm){w[i+1]<-(w[i]^h+(h*g*(1-w[i]/wInf)*(temps[i]-Tl)*dur)/
                       (100*(Tm-Tl)))^(1/h)
  } else {
  w[i+1]<-(w[i]^h+(h*g*(1-w0/wInf)*(temps[i]-Tu)*dur)/
                       (100*(Tm-Tu)))^(1/h)
  }}
}
  wFinal<-w[i]
  return(wFinal)
}

getElliottGrowth<-function(time1,time2,weights,tempData){
  wExp<-rep(as.numeric(NA),length(time1))
  for(i in 2:length(time1)){
    wExp[i]<-elliottGrowth(weights[i-1],tempData[time1[i]:time2[i],temperature])
  }
  return(wExp)
}


