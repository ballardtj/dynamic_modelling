data {
  int Nsubj;
  int Nobs;
  real discrepancy[Ntotal];
  real init_discrepancy[Ntotal];
  real effort[Ntotal];
  real time[Ntotal];
}  
 
parameters {
  real effort_init;
  real effort_change;
  real sigma;
}

model {
  //initialise variables
  real disc_change;
  real predicted_effort;
  
  //priors
  effort_init ~ normal(0,5);
  effort_change ~ normal(0,5);
  sigma ~ normal(0,5);
  
  //likelihood
  for(i in 1:Ntotal){
    if(time[i]==1){
      disc_change = discrepancy[i] - init_discrepancy[i];
      predicted_effort = effort_init + disc_change*effort_change;
    }
    if(time[i]>1){
      int disc_change = discrepancy[i] - discrepancy[i-1];
      predicted_effort = effort_init[i-1] + disc_change*effort_change;
    }
    
  }

  effort[i] ~ normal(predicted_effort,sigma);
}
  