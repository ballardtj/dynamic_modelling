data {
  int Nsubj;
  int Ntotal;
  real demands[Ntotal];
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
  real prev_demands;
  real prev_effort;
  real predicted_effort;
  
  //priors
  effort_init ~ normal(0,5);
  effort_change ~ normal(0,5);
  sigma ~ normal(0,5);
  
  //likelihood
  for(i in 1:Ntotal){
    
    if(time[i]==1){
      prev_demands = 0;
      prev_effort = effort_init;
    }
    if(time[i]>1){
      prev_demands = demands[i-1];
      prev_effort = effort[i-1];
    }
    
    predicted_effort = prev_effort + (demands[i]-prev_demands)*effort_change;
    effort[i] ~ normal(predicted_effort,sigma);
  }
}