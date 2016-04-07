data{
  int<lower=0> nAll;
  int<lower=0> nEvalRows;
  int<lower=0> nFirstObsRows;
  real lengthDATA[nAll];
  int evalRows[nEvalRows];
  int firstObsRows[nFirstObsRows];
  real obsSigma;
}

parameters{
  real<lower=0> beta1;
  real<upper=0> beta2;
  real<lower=0,upper=10> sigma;
  real<lower=0> length[nAll];
  real gr[nEvalRows];
}

transformed parameters{
  for(e in 1:nEvalRows){
    length[e]<-length[evalRows[e]-1]+gr[e];
  }
}

model{
  real grExp[nEvalRows];

  for(f in 1:nFirstObsRows){
    length[f]~normal(83,100);
    lengthDATA[f]~normal(length[f],obsSigma);
  }
  for(e in 1:nEvalRows){
    grExp[e]<-beta1+beta2*length[evalRows[e]-1];
    gr[e]~normal(grExp[e],sigma);
    //length[e]<-length[evalRows[e]-1]+gr[e];
    lengthDATA[evalRows[e]]~normal(length[evalRows[e]],obsSigma);
  }

  beta1~normal(0.0,10);
  beta2~normal(0.0,10);
  sigma~uniform(0.0,10);
}
