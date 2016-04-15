
// would making the performance bit a function speed things up? If it could be vectorized it probably would
// functions{
//   real perform(real temp, real tOpt, real ctMax, real sigma){
//
//   }
// }

data{
  int<lower=0> nObs;
  int<lower=0> startTime[nObs];
  int<lower=0> perfDuration[nObs];
  vector<lower=0>[nObs] startLength;
  vector[nObs] gr;
  int<lower=0> nTimes;
  vector[nTimes] temp;
  real obsSigma;
}

parameters{
  real<lower=0,upper=30> tOpt;
  real<lower=0,upper=30> maxAdd;
  real<lower=0,upper=10> sigma;
  real<lower=0,upper=0.01*1000> epsScaled;
  real<lower=0> beta1Scaled;
  real<upper=0> beta2Scaled;

}

transformed parameters{
  real ctMax;

  real perf[nTimes];
  vector[nTimes] p;

  ctMax<-tOpt+maxAdd;

  for(t in 1:nTimes){
    if(temp[t]>tOpt){
      perf[t]<- 1-pow(((temp[t])-tOpt)/(tOpt-ctMax),2);
    }
    else {
      perf[t]<- exp(-pow((temp[t]-tOpt)/(2*sigma),2));
    }
  }

  for(i in 1:nObs){
    // grExp[i]<-beta1+beta2*startLength[i]; //von bert = expected growth at tOpt
    p[i]<-sum(segment(perf,startTime[i],perfDuration[i])); //summed performance over growth period
  }
}

model{

  vector[nObs] grRate;
  for(i in 1:nObs){
    grRate[i]<-gr[i]/p[i]*1000;//daily growth rate
  }
  grRate~normal(beta1Scaled+beta2Scaled*startLength,epsScaled);

  //priors
  maxAdd~normal(5,30);
  tOpt~normal(15,10);
  sigma~uniform(0,10);

  beta1Scaled~normal(0,15);
  beta2Scaled~normal(0,6e-5*2000);
  epsScaled~normal(0,0.0015*2000);
}

generated quantities{
  real beta1;
  real beta2;
  real eps;
  beta1<-beta1Scaled/1000;
  beta2<-beta2Scaled/1000;
  eps<-epsScaled/1000;
}

