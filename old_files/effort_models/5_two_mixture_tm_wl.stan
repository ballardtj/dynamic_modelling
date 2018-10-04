data {
  int Nsubj;
  int Ntotal;
  real demands[Ntotal];
  real init_discrepancy[Ntotal];
  real discrepancy[Ntotal];
  real effort[Ntotal];
  real time[Ntotal];
  int subject[Ntotal];
  int condition[Ntotal];
}  
 
parameters {
  real e0[4];
  real k1[2];
  real k2[2];
  real k3[2];
  real<lower=0> tau[2];
  real<lower=0> sigma;
  real<lower=0,upper=1> mix_weight[Nsubj];
}

transformed parameters {
  //initialise other variables
  matrix[Ntotal,2] predicted_effort;
  real prev_effort[2];

  for(i in 1:Ntotal){  
    if(time[i]==1){
      prev_effort[1] = e0[condition[i]];
      prev_effort[2] = e0[condition[i]+2];
    }
    if(time[i]>1){
      prev_effort[1] = predicted_effort[i-1,1];
      prev_effort[2] = predicted_effort[i-1,2];
    }
    predicted_effort[i,1] = k1[condition[i]]*prev_effort[1] + k2[condition[i]]*discrepancy[i]/(1+tau[condition[i]]*(15-time[i])); 
    predicted_effort[i,2] = k1[condition[i]]*prev_effort[2] + k3[condition[i]]*demands[i];
  }
}

model {
  //initialise other variables
  real predicted_effort1;
  real predicted_effort2;
  real previous_effort1;
  real previous_effort2;
  
  //priors
  e0 ~ normal(0,5);
  k1 ~ normal(0,5);
  k2 ~ normal(0,5);
  k3 ~ normal(0,5);
  tau ~ normal(0,1);
  sigma ~ normal(0,5);
  
  //likelihood
  for(i in 1:Ntotal){
    target += log_mix(mix_weight[subject[i]],
                        normal_lpdf(effort[i] | predicted_effort[i,1], sigma),
                        normal_lpdf(effort[i] | predicted_effort[i,2], sigma));

  }
}