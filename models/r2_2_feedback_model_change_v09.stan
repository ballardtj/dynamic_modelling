//version 6 - same as version 6, but includes return to non-zero baseline instead of effort intercept

//this model fits well

data {
  int Ntotal;                   //Total number of trials in the dataset (600)
  real trial[Ntotal];           //Trial number
  real time[Ntotal];
  real overall_time[Ntotal];
  real difficulty[Ntotal];
  real effort[Ntotal];
  real score[Ntotal];     //score for each trial
  //real goal[Ntotal];
  int Nglobal_trial;
  real goal[Nglobal_trial];
  int global_trial_number[Ntotal];
  //real observed_goal[Ntotal];   //Goal level for each trial
}

parameters {
  //real<lower=0> ability_0;
  //real<lower=0> max_ability_gain; //
  real<lower=0.05,upper=1> learning_rate;
  //real alpha_int;
  //real<lower=0> alpha_slope;
  //real<lower=0> beta_int;
  //real<lower=0> beta_slope;
  //real<lower=0> delta_int;
  //real<lower=0> delta_slope;

  real<lower=0,upper=10> eff_0;
  real<lower=0,upper=10> eff_baseline;
 // real perf_int;
  //real<lower=0> dp_int;         //linear change in performnace;
  real gain11;                    //discrepancy on effort
  real gain12;                     //ability on effort
  real gain13;                    //discrepancy x ability interaction
  real gain20;
  real<lower=0,upper=1> alpha;
  real<lower=0> gain21;            //effort on score
  real<lower=0> gain22;            //ability on score
  real<lower=0> gain23;
  real gain24;
 // real gain23;                    //effort x ability interaction
  //real gain4;                   //effect of difficulty on change in performance;
  //real gain5;
  real<lower=0,upper=1> g_alpha;
  real<lower=0> g_beta;
  real<lower=0> sigma11;         //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma12;         //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma21;         //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma22;         //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma3;
}

transformed parameters {
  real predicted_goal[Nglobal_trial];
  real predicted_ability[Ntotal];
  real predicted_effort[Ntotal];
  real predicted_score[Ntotal];
  real predicted_change_in_goal[Nglobal_trial];
  real predicted_change_in_effort[Ntotal];
  real predicted_change_in_score[Ntotal];
  real score_outcome[Ntotal];
  real effort_outcome[Ntotal];

  //real predicted_alpha;//[Ntotal];
  //real predicted_beta;//[Ntotal];
  real ability_max = 1;
  real ability_0 = 0;
  //real perf_int = 0;
  //real eff_start;
  //real g_alpha = 1;
  //real gain1 = 1;
  //real eff_int = 0;
  //real predicted_delta;//[Ntotal];

  //LIKELIHOOD
  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){
    //ability_max = ability_0 + max_ability_gain;
    predicted_ability[i] = ability_max - (ability_max - ability_0)*exp(-learning_rate*overall_time[i]);
    //predicted_alpha = alpha_int + alpha_slope*predicted_ability[i];
    //predicted_beta = beta_int + beta_slope*predicted_ability[i];
    //predicted_delta = delta_int + delta_slope*predicted_ability[i];
    //predicted_effort[i] = eff_int + gain1*(goal[i] - predicted_score[i-1])/predicted_ability[i];// + difficulty[i]*gain2;


    //if the trial being considered is the first trial for that subject...
    if(time[i]==1){
      if(trial[i]==1){
        predicted_change_in_goal[global_trial_number[i]] = 0;
        predicted_goal[global_trial_number[i]] = goal[global_trial_number[i]];

        predicted_effort[i] = (eff_0 - eff_baseline)*alpha + gain11*predicted_goal[global_trial_number[i]] +
                              gain12*predicted_ability[i]*predicted_goal[global_trial_number[i]];

        predicted_change_in_effort[i] = predicted_effort[i] - eff_0;

      }
      if(trial[i]>1){
        predicted_change_in_goal[global_trial_number[i]] = g_alpha*(predicted_score[i-1]-predicted_goal[global_trial_number[i]-1]) + g_beta;
        predicted_goal[global_trial_number[i]] = predicted_goal[global_trial_number[i]-1] + predicted_change_in_goal[global_trial_number[i]];

        predicted_effort[i] = (predicted_effort[i-1]-eff_baseline)*alpha + gain11*predicted_goal[global_trial_number[i]] +
                              gain12*predicted_ability[i]*predicted_goal[global_trial_number[i]];

        predicted_change_in_effort[i] = predicted_effort[i] -   predicted_effort[i-1];
      }


      //predicted_change_in_effort[i] = eff_int + gain11*predicted_goal[global_trial_number[i]] + gain12*predicted_ability[i]*predicted_goal[global_trial_number[i]];
      //predicted_effort[i] = eff_start + predicted_change_in_effort[i] ;

      predicted_change_in_score[i] = (gain23 + gain22*predicted_ability[i]) / (1 + exp(-(gain20 + gain24*predicted_ability[i] + gain21*predicted_effort[i]  ) )); //    perf_int +  +
      predicted_score[i] = predicted_change_in_score[i];

      effort_outcome[i] = predicted_effort[i];
      score_outcome[i] = predicted_score[i];
    }
    if(time[i]>1){
      //predicted_change_in_effort[i] = eff_int + gain11*(predicted_goal[global_trial_number[i]] - predicted_score[i-1]) + gain12*predicted_ability[i]*(predicted_goal[global_trial_number[i]] - predicted_score[i-1]);
      //predicted_effort[i] = predicted_effort[i-1] + predicted_change_in_effort[i];

      predicted_effort[i] = (predicted_effort[i-1]-eff_baseline)*alpha  + gain11*(predicted_goal[global_trial_number[i]] - predicted_score[i-1])  +
      gain12*predicted_ability[i]*(predicted_goal[global_trial_number[i]]- predicted_score[i-1]);

      predicted_change_in_effort[i] = predicted_effort[i] - predicted_effort[i-1];

       predicted_change_in_score[i] = (gain23 + gain22*predicted_ability[i]) / (1 + exp(-(gain20 + gain24*predicted_ability[i] + gain21*predicted_effort[i]  ) )); //    perf_int +  +
      predicted_score[i] = predicted_score[i-1] + predicted_change_in_score[i];

      effort_outcome[i] = predicted_change_in_effort[i];
      score_outcome[i] = predicted_change_in_score[i];
    }
  }
}

model {
  real change_in_goal;
  real change_in_effort;
  real change_in_score;
  //PRIORS
  //ability_0 ~ normal(0,1);
  //max_ability_gain ~ normal(0,5);
  //learning_rate ~ normal(0,5);
  //alpha_int ~ normal(0,10);
  //alpha_slope ~ normal(0,10);
  //beta_int ~ normal(0,10);
  //beta_slope ~ normal(0,10);
  //delta_int ~ normal(0,10);
  //delta_slope ~ normal(0,10);
  eff_0 ~ normal(5,1);
  //alpha ~ normal(0,1); uniform
 // perf_int ~ normal(0,1);

  gain11 ~ normal(0,1);
  gain12 ~ normal(0,1);  //set prior on gain1
  gain20 ~ normal(0,5);
  gain21 ~ normal(0,1);
  gain22 ~ normal(0,5);
  gain23 ~ normal(0,1);
  gain24 ~ normal(0,1);
  gain13 ~ normal(0,1);
//  gain23 ~ normal(0,10);
  sigma11 ~ normal(0,1);         //set prior on sigma1
  sigma21 ~ normal(0,1);         //set prior on sigma2
  sigma12 ~ normal(0,1);         //set prior on sigma1
  sigma22 ~ normal(0,1);         //set prior on sigma2
 // g_alpha ~ normal(0,1);
  //g_beta ~ normal(0,1);
  sigma3 ~ normal(0,1);

  //  eff_0 ~ normal(6.5,1);
  // eff_int ~ normal(-0.6,1);
  // perf_int ~ normal(0,0.1);
  // gain1 ~ normal(0.1,0.1);
  // gain2 ~ normal(0.2,0.1);  //set prior on gain1
  // //gain3 ~ normal(0,10);
  // sigma1 ~ normal(2,1);         //set prior on sigma1
  // sigma2 ~ normal(2,1);         //set prior on sigma2

  for(i in 1:Ntotal){
    if(time[i]==1){
      effort[i] ~ normal(effort_outcome[i],sigma11);
      score[i] ~ normal(score_outcome[i],sigma21);
      //change_in_effort[i] = predicted_effort[i];
      //change_in_score[i] = predicted_score[i];
      if(trial[i]>1){
        change_in_goal = goal[global_trial_number[i]]-goal[global_trial_number[i]-1];
        change_in_goal ~ normal(predicted_change_in_goal[global_trial_number[i]],sigma3); //T[0,];
      }
    }
    if(time[i]>1){
      //change_in_goal = goal[global_trial_number[i]]-goal[global_trial_number[i]-1];
      //change_in_goal ~ normal(predicted_change_in_goal[global_trial_number[i]],sigma3); //T[0,];

      change_in_effort = effort[i]-effort[i-1];
      change_in_score = score[i]-score[i-1];

      change_in_effort ~ normal(effort_outcome[i],sigma12); //T[0,];
      change_in_score ~ normal(score_outcome[i],sigma22); //T[0,];
    }


    //goal[i] ~ normal(predicted_goal[i],sigma3); //T[0,];
  }
}

//NOTE: The generated quantities block (below) is NOT needed for the model to run. However, it can be useful for generating posterior predictives from the model. The posterior predictives from this model were presented in the "model evalutation" section of the paper.

generated quantities {
  real sampled_effort[Ntotal];
  real sampled_score[Ntotal];
  real sampled_goal[Ntotal];

  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){
    if(time[i]==1){
      sampled_effort[i] = normal_rng(predicted_effort[i],sigma11);
      sampled_score[i] = normal_rng(predicted_score[i],sigma21);
      if(trial[i]==1){
        sampled_goal[i] = goal[global_trial_number[i]];
      }
      if(trial[i]>1){
        sampled_goal[i] = sampled_goal[i-1] + normal_rng(predicted_change_in_goal[global_trial_number[i]],sigma3);
      }
    }
    if(time[i]>1){
      sampled_goal[i] = sampled_goal[i-1];
      sampled_effort[i] = sampled_effort[i-1] + normal_rng(predicted_change_in_effort[i],sigma12);
      sampled_score[i] = sampled_score[i-1] + normal_rng(predicted_change_in_score[i],sigma22);
    }

    //if the trial being considered is the first trial for that subject...
    //evaluate likelihood of observed goal given a normal distribution with mean = predicted_goal and sd = sigma
    // sampled_effort[i] = normal_rng(predicted_effort[i],2);
    // sampled_score[i] = normal_rng(predicted_score[i],2);
    //sampled_goal[i] = normal_rng(predicted_goal[i],sigma3);
  }
}
