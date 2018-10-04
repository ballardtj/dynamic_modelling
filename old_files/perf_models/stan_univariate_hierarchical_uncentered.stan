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
  //group level parameters (i.e., hyperpriors)
  real y0;
  real<lower=0> sigma0;
  
  real alpha_mean;
  real<lower=0> alpha_sd;
  
  real beta_mean;
  real<lower=0> beta_sd;
  
  real<lower=0> sigma_change_mean;
  real<lower=0> sigma_change_sd;
  
  //subject level parameters
  real alpha_raw[Nsubj];
  real beta_raw[Nsubj];
  real<lower=0> sigma_change_raw[Nsubj];//_raw[Nsubj];
}

transformed parameters {
  real alpha[Nsubj];
  real beta[Nsubj];
  real sigma_change[Nsubj];
  
  for(i in 1:Nsubj){
    alpha[i] = alpha_mean + alpha_raw[i]*alpha_sd;
    beta[i] = beta_mean + beta_raw[i]*beta_sd;
    sigma_change[i] = sigma_change_mean + sigma_change_raw[i]*sigma_change_sd;
  }
}

model {
 //group level priors
 y0 ~ normal(0,5);
 sigma0 ~ normal(0,5);
 sigma_change ~ normal(0,5);
 alpha_mean ~ normal(0,5);
 alpha_sd ~ normal(0,5);
 beta_mean ~ normal(0,5);
 beta_sd ~ normal(0,5);
 sigma_change_mean ~ normal(0,5);
 sigma_change_sd ~ normal(0,5);
 
//sample subject level parameters
alpha_raw ~ normal(0,1);  // ~ normal(alpha_mean,alpha_sd);
beta_raw ~ normal(0,1); // ~ normal(beta_mean,beta_sd);
sigma_change_raw ~ normal(0,1); //~ normal(sigma_change_mean,sigma_change_sd);
  
  for(i in 1:Nsubj){
    perf[i,1] ~ normal(y0,sigma0);
    for(j in 2:Nobs){
      real change_score = alpha[i] + perf[i,j-1]*beta[i];
      perf[i,j] ~ normal(perf[i,j-1] + change_score,sigma_change[i]);
    }
  }
}
  