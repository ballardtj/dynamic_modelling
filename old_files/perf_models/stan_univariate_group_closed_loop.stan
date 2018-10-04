data {
  int Nsubj;
  int Nobs;
  matrix[Nsubj,Nobs] perf;
}  
 
parameters {
  real y0;
  real alpha;
  real beta;
  real<lower=0> sigma0;
  real<lower=0> sigma_change;
}

model {
  matrix[Nsubj,Nobs] predicted_perf;
  
  //priors
  y0 ~ normal(0,5);
  alpha ~ normal(0,5);
  beta ~ normal(0,5);
  sigma0 ~ normal(0,5);
  sigma_change ~ normal(0,5);
  
  //likelihood
  for(i in 1:Nsubj){
    predicted_perf[i,1] = y0;
    perf[i,1] ~ normal(predicted_perf[i,1],sigma0);
    for(j in 2:Nobs){
      real change_score = alpha + perf[i,j-1]*beta;
      predicted_perf[i,j] = predicted_perf[i,j-1] + change_score;
      perf[i,j] ~ normal(predicted_perf[i,j] ,sigma_change);
    }
  }
}
  