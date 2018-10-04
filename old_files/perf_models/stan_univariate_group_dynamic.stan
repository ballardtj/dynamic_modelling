data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] y;
}  
 
parameters {
  real perf1_int;
  real dperf_int;
  real sf;
  real<lower=0> perf1_sd;
  real<lower=0> dperf_sd;
  real<lower=0> resid_sd;
}

model {
  vector[Nobs] yhat;
  real yhat_change;
  for(i in 1:Nsubj){
    yhat[1] = y[i,1];
    y[i,1] ~ normal(perf1_int,perf1_sd);
    for(j in 2:Nobs){
      //yhat_change = dperf_int + yhat[j-1]*sf;
      //y[i,j] ~ normal(yhat[j-1] + yhat_change,dperf_sd);
      //yhat[j] = yhat[j-1] + yhat_change;
      yhat_change = dperf_int + yhat[j-1]*sf;
      yhat[j] = yhat[j-1] + yhat_change;
      yhat[j] ~ normal(yhat[j-1] + yhat_change,dperf_sd); //evaluate change in yhat given variability in latent change
      y[i,j] ~ normal(yhat[j],resid_sd);
    }
  }
}
  