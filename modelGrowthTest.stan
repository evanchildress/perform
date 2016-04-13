
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
  // real<lower=0,upper=100> epsScaled;
  real<lower=0,upper=0.01> eps;
  real<lower=0,upper=0.1> beta1;
  real<lower=-0.001,upper=0> beta2;
  real<lower=0,upper=30> tOpt;
}

transformed parameters{
  // real eps;

  real perf[nTimes];
  vector[nTimes] p;


  for(t in 1:nTimes){
    if(temp[t]>tOpt){
      perf[t]<- 1-pow(((temp[t])-tOpt)/(tOpt-18),2);
    }
    else {
      perf[t]<- exp(-pow((temp[t]-tOpt)/(2*4),2));
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
    grRate[i]<-gr[i]/p[i];
  }
  grRate~normal(beta1+beta2*startLength,eps);

  //priors
  // maxAdd~normal(5,30);
  // tOpt~normal(11,30);
  // sigma~uniform(0,10);
  //
  // beta1Scaled~normal(0,0.01*100);
  // beta2Scaled~normal(0,3e-5*10000);

  // epsScaled~normal(0,1e-5*10000);
  eps~normal(0,0.01);
  beta1~normal(0,0.1);
  beta2~normal(0,0.001);
  tOpt~normal(15,10);
}

