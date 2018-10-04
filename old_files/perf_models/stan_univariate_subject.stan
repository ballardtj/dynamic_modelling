//estimates unique change intercept, change sd, and self-feedback parameter for each subject. 
//The initial score intercept and sd can only be estimated at the group level, because
//there is only one initial score for each subject.

data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] perf;
}  
 
parameters {
  real y0;
  real alpha[Nsubj];
  real beta[Nsubj];
  real<lower=0> sigma0;
  real<lower=0> sigma_change[Nsubj];
}

model {
   //priors
  y0 ~ normal(0,5);
  alpha ~ normal(0,5);
  beta ~ normal(0,5);
  sigma0 ~ normal(0,5);
  sigma_change ~ normal(0,5);
  
  //likelihood
  for(i in 1:Nsubj){
    perf[i,1] ~ normal(y0,sigma0);
    for(j in 2:Nobs){
      real change_score = alpha[i] + perf[i,j-1]*beta[i];
      perf[i,j] ~ normal(perf[i,j-1] + change_score,sigma_change[i]);
    }
  }
}
  