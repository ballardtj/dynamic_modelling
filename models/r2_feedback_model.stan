data {
  int Ntotal;                   //Total number of trials in the dataset (600)
  real trial[Ntotal];           //Trial number
  real time[Ntotal];
  real difficulty[Ntotal];
  real effort[Ntotal];
  real score[Ntotal];     //score for each trial
  real goal[Ntotal];
  //real observed_goal[Ntotal];   //Goal level for each trial
}

parameters {
  real<lower=0> eff_0;
  real eff_int;
  real<lower=0> dp_int;         //linear change in performnace;
  real gain1;                   //initialize single gain1 parameter for entire sample
  real<lower=0> gain2;                   //initialize single gain2 parameter for entire sample
  real gain3;                   //effect of difficulty on effort
  real gain4;                   //effect of difficulty on change in performance;
  real gain5;
  real<lower=0> sigma1;         //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma2;         //initialize single sigma parameter for entire sample and set lower bound at 0.
}

model {
  real predicted_effort;          //initialize predicted goal level object to store predictions
  real predicted_score;

  //PRIORS
  eff_0 ~ normal(5,2);
  eff_int ~ normal(0,1);
  dp_int ~ normal(0,1);
  gain1 ~ normal(0,1);          //set prior on gain1
  gain2 ~ normal(0,1);          //set prior on gain2
  gain3 ~ normal(0,1);
  gain4 ~ normal(0,1);
  gain5 ~ normal(0,1);
  sigma1 ~ normal(0,1);         //set prior on sigma1
  sigma2 ~ normal(0,1);         //set prior on sigma2

  //LIKELIHOOD
  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){
    //if the trial being considered is the first trial for that subject...
    if(time[i]==1){
      predicted_effort = eff_0 + eff_int + goal[i]*gain1 + difficulty[i]*gain3; //discrepancy is equal to the goal at the start of the trial
      predicted_score = dp_int + predicted_effort*gain2 + difficulty[i]*gain4 + trial[i]*gain5; //score starts at 0
    }
    if(time[i]>1){
      predicted_effort += eff_int + (goal[i] - predicted_score)*gain1 + difficulty[i]*gain3;
      predicted_score += dp_int + predicted_effort*gain2 + difficulty[i]*gain4 + trial[i]*gain5;
    }

    //evaluate likelihood of observed goal given a normal distribution with mean = predicted_goal and sd = sigma
    effort[i] ~ normal(predicted_effort,sigma1);
    score[i] ~ normal(predicted_score,sigma2);
  }
}

//NOTE: The generated quantities block (below) is NOT needed for the model to run. However, it can be useful for generating posterior predictives from the model. The posterior predictives from this model were presented in the "model evalutation" section of the paper.

generated quantities {
  real predicted_effort;          //initialize predicted goal level object to store predictions
  real predicted_score;
  real sampled_effort[Ntotal];
  real sampled_score[Ntotal];

  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){
    //if the trial being considered is the first trial for that subject...
    if(time[i]==1){
      predicted_effort = eff_0 + eff_int + goal[i]*gain1 + difficulty[i]*gain3; //discrepancy is equal to the goal at the start of the trial
      predicted_score = dp_int + predicted_effort*gain2 + difficulty[i]*gain4 + trial[i]*gain5; //score starts at 0
    }
    if(time[i]>1){
      predicted_effort += eff_int + (goal[i] - predicted_score)*gain1 + difficulty[i]*gain3;
      predicted_score += dp_int + predicted_effort*gain2 + difficulty[i]*gain4 + trial[i]*gain5;
    }

    //evaluate likelihood of observed goal given a normal distribution with mean = predicted_goal and sd = sigma
    sampled_effort[i] = normal_rng(predicted_effort,sigma1);
    sampled_score[i] = normal_rng(predicted_score,sigma2);
  }
}
