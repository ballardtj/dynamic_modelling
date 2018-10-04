data {
  int Nsubj;
  int Ntotal;
  real demands[Ntotal];
  real init_discrepancy[Ntotal];
  real discrepancy[Ntotal];
  real effort[Ntotal];
  real time[Ntotal];
  int subject[Ntotal];
}  

parameters {
  real effort_init;
  real effort_change1;
  real effort_change2;
  real<lower=0> sigma;
  simplex[3] mix_weights[Nsubj];
}

transformed parameters {
  //initialise other variables
  matrix[Ntotal,3] predicted_effort;
   
  real prev_demands;
  real prev_discrepancy;
  real prev_effort1;
  real prev_effort2;
   
  for(i in 1:Ntotal){  
    if(time[i]==1){
      prev_demands = 0;
      prev_discrepancy = init_discrepancy[i];
      prev_effort1 = effort_init;
      prev_effort2 = effort_init;
    }
    if(time[i]>1){
      prev_demands = demands[i-1];
      prev_discrepancy = discrepancy[i-1];
      prev_effort1 = predicted_effort[i-1,1];
      prev_effort2 = predicted_effort[i-1,2];
    }
     
  
    predicted_effort[i,1] = prev_effort1 + (discrepancy[i]-prev_discrepancy)*effort_change1; 
    predicted_effort[i,2] = prev_effort2 + (demands[i]-prev_demands)*effort_change2;
    predicted_effort[i,3] = effort_init;
  }
}

model {
   real lps[3];

  //priors
  effort_init ~ normal(0,5);
  effort_change1 ~ normal(0,5);
  effort_change2 ~ normal(0,5);
  sigma ~ normal(0,5);
  
  //likelihood
  //print(predicted_effort);
  for(i in 1:Ntotal){
    for (k in 1:3){
      
      lps[k] = log(mix_weights[subject[i],k]); 
      lps[k] += normal_lpdf(effort[i] | predicted_effort[i,k], sigma);
    }
    target += log_sum_exp(lps);

  }
}