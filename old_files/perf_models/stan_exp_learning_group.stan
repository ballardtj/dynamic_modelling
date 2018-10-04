data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] perf;
  matrix[Nsubj,Nobs] diff;
}  
 
parameters {
  real y0_perf;
  real yinf_perf;
  real y0_diff;
  real yinf_diff;
  real<lower=0>  rate;
  real<lower=0> sigma_perf;
  real<lower=0>  sigma_diff;
}

model {
  //priors
  y0_perf ~ normal(0,5);
  yinf_perf ~ normal(0,5);
  y0_diff ~ normal(0,5);
  yinf_diff ~ normal(0,5);
  rate ~ normal(0,5);
  
  sigma_perf ~ normal(0,5);
  sigma_diff ~ normal(0,5);
 
  //likelihood
  for(i in 1:Nsubj){
    for(j in 1:Nobs){
      real predicted_perf = yinf_perf-(yinf_perf*y0_perf)*exp(-rate*j);
      real predicted_diff = yinf_diff-(yinf_diff*y0_diff)*exp(-rate*j);
      
      perf[i,j] ~ normal(predicted_perf,sigma_perf);   
      diff[i,j] ~ normal(predicted_diff,sigma_diff);  
  
    }
  }
}

generated quantities{
  matrix[Nsubj,Nobs] pred_perf;
  matrix[Nsubj,Nobs] pred_diff;
  
  for(i in 1:Nsubj){
    for(j in 1:Nobs){
      pred_perf[i,j] = yinf_perf-(yinf_perf*y0_perf)*exp(-rate*j);
      pred_diff[i,j] = yinf_diff-(yinf_diff*y0_diff)*exp(-rate*j);
  
    }
  }
}