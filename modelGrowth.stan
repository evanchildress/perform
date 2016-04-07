data{
  int<lower=0> nObs;
  int<lower=0> startTime[nObs];
  int<lower=0> endTime[nObs];
  real gr[nObs];
  int<lower=0> nTimes;
  real temp[nTimes];
}

parameters{
  real<lower=0,upper=50> tOpt;
  real<lower=0,upper=50> maxAdd;
  real<lower=0,upper=10> sigma;
  real<lower=0,upper=0.1 beta1;
  real<lower=-0.1,upper=0> beta2;
  real<lower=0,0.01> eps;
  real err[nObs]
}

transformed parameters{
  real ctMax;
  real perf[nTimes];
  real p[nTimes];
  real grMax[nObs];
  real grExp[nObs];

  ctMax<-tOpt+maxAdd;

  for(t in 1:nTimes){
    perf[t]<-ifelse(temp[t]>tOpt,1-(((temp[t])-tOpt)/(tOpt-ctMax))^2,
                    exp(-((temp[t]-tOpt)/(2*sigma))^2));
  }

  for(i in 1:nObs){
    grMax[i]<-beta1+beta2*startLength[i]+err[i]; //von bert plus noise
    p[i]<-sum(perf[startTime[i]:endTime[i]]); //summed performance over growth period
    grExp[i]<-p[i]*grMax[i];//expected growth rate
  }
}

model{
  err~normal(0.0,tauEps);
  gr~normal(grExp,obsTau);

  //priors
  maxAdd~normal(5,30);
  tOpt~normal(11,30);
  sigma~uniform(0,10);

  beta1~dnorm(0,0.01);
  beta2~dnorm(0,3e-5);

  eps~normal(0,1e-5)
}
