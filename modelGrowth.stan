data{
  int<lower=0> nObs;
  int<lower=0> startTime[nObs];
  int<lower=0> perfDuration[nObs];
  real<lower=0> startLength[nObs];
  real gr[nObs];
  int<lower=0> nTimes;
  real temp[nTimes];
  real obsSigma;
}

parameters{
  real<lower=0,upper=50> tOpt;
  real<lower=0,upper=50> maxAdd;
  real<lower=0,upper=10> sigma;
  real<lower=0,upper=0.1> beta1Scaled;
  real<lower=-0.1,upper=0> beta2Scaled;
  real<lower=0,upper=0.01> epsScaled;
  real errScaled[nObs];
}

transformed parameters{
  real ctMax;
  real beta1;
  real beta2;
  real eps;
  real err[nObs];
  real perf[nTimes];
  real p[nTimes];
  real grMax[nObs];
  real grExp[nObs];

  ctMax<-tOpt+maxAdd;

  //rescale parameters that were adjusted to improve sampling
  beta1<-beta1Scaled/100;
  beta2<-beta2Scaled/10000;
  eps<-epsScaled/10000;
  for(i in 1:nObs){
    err[i]<-errScaled[i]/10000;
  }

  for(t in 1:nTimes){
    if(temp[t]>tOpt){
      perf[t]<- 1-(((temp[t])-tOpt)/(tOpt-ctMax))^2;
    }
    else {
      perf[t]<- exp(-((temp[t]-tOpt)/(2*sigma))^2);
    }
  }

  for(i in 1:nObs){
    grMax[i]<-beta1+beta2*startLength[i]+err[i]; //von bert plus noise
    p[i]<-sum(segment(perf,startTime[i],perfDuration[i])); //summed performance over growth period
    grExp[i]<-p[i]*grMax[i];//expected growth rate
  }
}

model{
  for(i in 1:nObs){
    ((gr[i]/p[i])*10000)~normal(grExp[i]*10000,epsScaled);
  }

  //priors
  maxAdd~normal(5,30);
  tOpt~normal(11,30);
  sigma~uniform(0,10);

  beta1Scaled~normal(0,0.01*100);
  beta2Scaled~normal(0,3e-5*10000);

  epsScaled~normal(0,1e-5*10000);
}
