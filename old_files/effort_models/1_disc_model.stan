data {
  int Nsubj;
  int Ntotal;
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
  //initialise other variables
  real prev_discrepancy;
  real prev_effort;
  real predicted_effort;
  
  //priors
  effort_init ~ normal(0,5);
  effort_change ~ normal(0,5);
  sigma ~ normal(0,5);
  
  //likelihood
  for(i in 1:Ntotal){
    
    if(time[i]==1){
      prev_discrepancy = init_discrepancy[i];
      prev_effort = effort_init;
    }
    if(time[i]>1){
      prev_discrepancy = discrepancy[i-1];
      prev_effort = effort[i-1];
    }
    
    if(discrepancy[i]>0){
      predicted_effort = prev_effort + (discrepancy[i]-prev_discrepancy)*effort_change;
    }
    
    if(discrepancy[i]<=0){
      predicted_effort = prev_effort;
    }
      
    effort[i] ~ normal(predicted_effort,sigma);
  }
}