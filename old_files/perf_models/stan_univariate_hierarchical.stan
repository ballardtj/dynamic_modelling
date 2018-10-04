//estimates unique change intercept, change sd, and self-feedback parameter for each subject. 
//Also estimates population mean and sd for each of those subject-level parameters.
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
  real alpha_mean;          
  real<lower=0> alpha_sd;
  real beta_mean;
  real<lower=0> beta_sd;
  real<lower=0> sigma_change_mean;
  real<lower=0> sigma_change_sd;
}

model {
//priors
 y0 ~ normal(0,5);
 sigma0 ~ normal(0,5);
 sigma_change ~ normal(sigma_change_mean,sigma_change_sd);
 alpha ~ normal(alpha_mean,alpha_sd);
 beta ~ normal(beta_mean,beta_sd);
 alpha_mean ~ normal(0,5);
 alpha_sd ~ normal(0,5);
 beta_mean ~ normal(0,5);
 beta_sd ~ normal(0,5);
 sigma_change_mean ~ normal(0,5);
 sigma_change_sd ~ normal(0,5);

 //likelihood
  for(i in 1:Nsubj){
    perf[i,1] ~ normal(y0,sigma0);
    for(j in 2:Nobs){
      real change_score = alpha[i] + perf[i,j-1]*beta[i];
      perf[i,j] ~ normal(perf[i,j-1] + change_score,sigma_change[i]);
    }
  }
}
  