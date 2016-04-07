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
  real<lower=0,upper=0.1> beta1;
  real<lower=-0.1,upper=0> beta2;
  real<lower=0,upper=0.01> eps;
  real err[nObs];
}

transformed parameters{
  real ctMax;
  real perf[nTimes];
  real p[nTimes];
  real grMax[nObs];
  real grExp[nObs];

  ctMax<-tOpt+maxAdd;

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
  err~normal(0.0,eps);
  gr~normal(grExp,obsSigma);

  //priors
  maxAdd~normal(5,30);
  tOpt~normal(11,30);
  sigma~uniform(0,10);

  beta1~normal(0,0.01);
  beta2~normal(0,3e-5);

  eps~normal(0,1e-5);
}
