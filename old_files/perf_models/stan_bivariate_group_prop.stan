data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] perf;
  matrix[Nsubj,Nobs] diff;
}  
 
parameters {
  real y0_perf;
  real alpha_perf;
  real beta_perf;
  real<lower=0> sigma0_perf;
  real<lower=0> sigma_change_perf;
  
  real y0_diff;
  real alpha_diff;
  real beta_diff;
  real<lower=0> sigma0_diff;
  real<lower=0> sigma_change_diff;
  
  real gamma_perf; //effect of diff on change in perf
  real gamma_diff; //effect of perf on change in diff
}

model {
  //priors
  y0_perf ~ normal(0,5);
  alpha_perf ~ normal(0,5);
  beta_perf ~ normal(0,5);
  sigma0_perf ~ normal(0,5);
  sigma_change_perf ~ normal(0,5);
  
  y0_diff ~ normal(0,5);
  alpha_diff ~ normal(0,5);
  beta_diff ~ normal(0,5);
  sigma0_diff ~ normal(0,5);
  sigma_change_diff ~ normal(0,5);
  
  gamma_perf ~ normal(0,5);
  gamma_diff ~ normal(0,5);
  
  //likelihood
  for(i in 1:Nsubj){
    perf[i,1] ~ normal(y0_perf,sigma0_perf);
    diff[i,1] ~ normal(y0_diff,sigma0_diff);
    for(j in 2:Nobs){
      real perf_change_score = alpha_perf + perf[i,j-1]*beta_perf + diff[i,j-1]*gamma_perf;
      real diff_change_score = alpha_diff + diff[i,j-1]*beta_diff + perf[i,j-1]*gamma_diff;
      
      perf[i,j] ~ normal(perf[i,j-1] + perf_change_score,sigma_change_perf);
      diff[i,j] ~ normal(diff[i,j-1] + diff_change_score,sigma_change_diff);
    }
  }
}

generated quantities{
  matrix[Nsubj,Nobs] predicted_perf;
  matrix[Nsubj,Nobs] predicted_diff;
  
  for(i in 1:Nsubj){
    predicted_perf[i,1] = normal_rng(y0_perf,sigma0_perf);
    predicted_diff[i,1] = normal_rng(y0_diff,sigma0_diff);
    for(j in 2:Nobs){
      real perf_change_score = alpha_perf + perf[i,j-1]*beta_perf + diff[i,j-1]*gamma_perf;
      real diff_change_score = alpha_diff + diff[i,j-1]*beta_diff + perf[i,j-1]*gamma_diff;
      
      predicted_perf[i,j] = normal_rng(perf[i,j-1] + perf_change_score,sigma_change_perf);
      predicted_diff[i,j] = normal_rng(diff[i,j-1] + diff_change_score,sigma_change_diff);
    }
  }
  
  
  
  
  
  
  
  
  
}