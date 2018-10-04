data {
  int Nsubj;
  int Nobs;
  matrix[Nsubj,Nobs] perf;
}  
 
parameters {
  real y0;
  ordered[2] alpha;
  real beta[2];
  real<lower=0> sigma0;
  real<lower=0> sigma_change;
  real<lower=0,upper=1> mix_weight[Nsubj]; // mixing proportion
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
      real change_score_mix1 = alpha[1] + perf[i,j-1]*beta[1]; //predicted change for mixture 1
      real change_score_mix2 = alpha[2] + perf[i,j-1]*beta[2]; //predicted change for mixture 2
      target += log_mix(mix_weight[i],
                        normal_lpdf(perf[i,j] | perf[i,j-1] + change_score_mix1, sigma_change),
                        normal_lpdf(perf[i,j] | perf[i,j-1] + change_score_mix2, sigma_change));
    }
  }
}