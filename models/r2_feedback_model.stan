data {
  int Ntotal;                   //Total number of trials in the dataset (600)
  real trial[Ntotal];           //Trial number
  real time[Ntotal];
  real overall_time[Ntotal];
  real difficulty[Ntotal];
  real effort[Ntotal];
  real score[Ntotal];     //score for each trial
  real goal[Ntotal];
  //real observed_goal[Ntotal];   //Goal level for each trial
}

parameters {
  real<lower=0> ability_0;
  real<lower=0> ability_max;
  real<lower=0> learning_rate;
  real alpha_int;
  real<lower=0> alpha_slope;
  real<lower=0> beta_int;
  real<lower=0> beta_slope;

  real<lower=0> eff_0;
  real eff_int;
  //real<lower=0> dp_int;         //linear change in performnace;
  real<lower=0> gain1;                   //initialize single gain1 parameter for entire sample
  real<lower=0> gain2;                   //initialize single gain2 parameter for entire sample
  //real gain3;                   //effect of difficulty on effort
  //real gain4;                   //effect of difficulty on change in performance;
  //real gain5;
  real<lower=0> sigma1;         //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma2;         //initialize single sigma parameter for entire sample and set lower bound at 0.
}

transformed parameters {
  real predicted_ability[Ntotal];
  real predicted_effort[Ntotal];
  real predicted_score[Ntotal];
  real predicted_alpha;//[Ntotal];
  real predicted_beta;//[Ntotal];

  //LIKELIHOOD
  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){
    predicted_ability[i] = ability_max - (ability_max - ability_0)*exp(-learning_rate*overall_time[i]);
    predicted_alpha = alpha_int + alpha_slope*predicted_ability[i];
    predicted_beta = beta_int + beta_slope*predicted_ability[i];

    //if the trial being considered is the first trial for that subject...
    if(time[i]==1){
      predicted_effort[i] = eff_0 + eff_int + goal[i]*gain1;//+ difficulty[i]*gain2; //discrepancy is equal to the goal at the start of the trial
      predicted_score[i] = gain2 / (1 + exp(-(predicted_alpha+predicted_beta*predicted_effort[i])));
    }
    if(time[i]>1){
      predicted_effort[i] = predicted_effort[i-1] + eff_int + (goal[i] - predicted_score[i-1])*gain1;// + difficulty[i]*gain2;
      predicted_score[i] = predicted_score[i-1] + gain2 / (1 + exp(-(predicted_alpha+predicted_beta*predicted_effort[i])));
    }
  }


}

model {
  //PRIORS
  ability_0 ~ normal(0,1);
  ability_max ~ normal(0,5);
  learning_rate ~ normal(0,1);
  alpha_int ~ normal(0,1);
  alpha_slope ~ normal(0,1);
  beta_int ~ normal(0,1);
  beta_slope ~ normal(0,1);
  eff_0 ~ normal(5,2);
  eff_int ~ normal(0,1);
  gain1 ~ normal(0,1);
  gain2 ~ normal(0,1);  //set prior on gain1
  sigma1 ~ normal(0,1);         //set prior on sigma1
  sigma2 ~ normal(0,1);         //set prior on sigma2

  for(i in 1:Ntotal){
    effort[i] ~ normal(predicted_effort[i],sigma1) T[0,];
    score[i] ~ normal(predicted_score[i],sigma2) T[0,];
  }
}

//NOTE: The generated quantities block (below) is NOT needed for the model to run. However, it can be useful for generating posterior predictives from the model. The posterior predictives from this model were presented in the "model evalutation" section of the paper.

generated quantities {
  real sampled_effort[Ntotal];
  real sampled_score[Ntotal];

  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){
    //if the trial being considered is the first trial for that subject...
    //evaluate likelihood of observed goal given a normal distribution with mean = predicted_goal and sd = sigma
    sampled_effort[i] = normal_rng(predicted_effort[i],sigma1);
    sampled_score[i] = normal_rng(predicted_score[i],sigma2);
  }
}
