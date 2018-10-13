data {
  int Ntotal;  //Total number of trials in the dataset (600)
  int Nsubj;
  int Ntrial;
  real trial[Ntotal];           //Trial number
  real time[Ntotal];
  real goal[Nsubj*Ntrial];
  real eff[Ntotal];
  real score[Ntotal];
  int trial_index[Ntotal];
}

parameters {
  //real<lower=0> self_efficacy_0;             //initial level of self-efficacy
  //real<lower=0> k;                       //affect change in self-efficacy
  //real<lower=0> ability;                 //places max on performance

  //real<lower=0> value;
  real<lower=0> sigma_goal;          //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma_effort;          //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma_perf;          //initialize single sigma parameter for entire sample and set lower bound at 0.
}

transformed parameters {
  real self_efficacy_0;
  real k;
  real ability;
  real value;
  real capacity;
  real change_in_self_efficacy;
  real change_in_effort;
  vector[Nsubj*Ntrial] predicted_goal;          //initialize predicted goal level object to store predictions
  vector[Ntotal] predicted_performance;
  vector[Ntotal] predicted_self_efficacy;
  vector[Ntotal] predicted_effort;

  self_efficacy_0 = 0.5;
  k = 0.3;
  ability = 1;
  value = 0.3525;

  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){

    if(time[i]==1){
      if(trial[i]==1){
        //if time 1 of trial 1, predicted goal is based on initial self-efficacy.
        predicted_goal[trial_index[i]] = self_efficacy_0*value;
        predicted_effort[i] = predicted_goal[trial_index[i]]; //approach only
        predicted_performance[i] = predicted_effort[i]*ability; //fmin(predicted_effort[i]*ability,ability); // should replace this with saturating exponential
        capacity = predicted_performance[i]/predicted_effort[i];
        change_in_self_efficacy = k*(capacity-self_efficacy_0);
        predicted_self_efficacy[i] = self_efficacy_0 + change_in_self_efficacy;
      }
      if(trial[i]>1){
        //if time 1 of trial 2 of greater, predicted goal is based on previous self-efficacy (i.e., at the end of the previous trial)
        predicted_goal[trial_index[i]] = predicted_self_efficacy[i-1]*value;
        predicted_effort[i] = predicted_goal[trial_index[i]]; //approach only
        predicted_performance[i] = predicted_effort[i]*ability; //fmin(predicted_effort[i]*ability,ability); // should replace this with saturating exponential
        capacity = predicted_performance[i]/predicted_effort[i];
        change_in_self_efficacy = k*(capacity-predicted_self_efficacy[i-1]);
        predicted_self_efficacy[i] = predicted_self_efficacy[i-1] + change_in_self_efficacy;
      }
    }

    if(time[i]>1){

       change_in_effort = predicted_goal[trial_index[i]] - score[i];
       predicted_effort[i] = predicted_effort[i-1] + change_in_effort; //Jeff's model multitplies change_in_effort x time step, but since time step is an arbitrary constant we omit this.
       predicted_performance[i] = predicted_effort[i]*ability;  //fmin(predicted_effort[i]*ability,ability); // should replace this with saturating exponential
       capacity = predicted_performance[i]/predicted_effort[i];
       change_in_self_efficacy = k*(capacity-predicted_self_efficacy[i-1]);
       predicted_self_efficacy[i] = predicted_self_efficacy[i-1] + change_in_self_efficacy;
    }
  }
}

model {

  //PRIORS
  //self_efficacy_0 ~ normal(0,1);          //set prior on alpha
  //k ~ normal(0,1);                    //set prior on beta
  //ability ~ normal(0,1);              //set prior on sigma
  sigma_goal ~ normal(0,1);
  sigma_effort ~ normal(0,1);
  sigma_perf ~ normal(0,1);
  //value ~ normal(0,1);

  //LIKELIHOOD

  goal ~ normal(predicted_goal,sigma_goal);
  eff ~ normal(predicted_effort,sigma_effort);
  score ~ normal(predicted_performance,sigma_perf);

}

//NOTE: The generated quantities block (below) is NOT needed for the model to run. However, it can be useful for generating posterior predictives from the model. The posterior predictives from this model were presented in the "model evalutation" section of the paper.

generated quantities {
  real sampled_goal[Ntotal];   //initialize object to store set of goal level samples (i.e., the posterior predictives)
  real sampled_eff[Ntotal];
  real sampled_perf[Ntotal];

  //loop through all trials in the dataset generating predictions in the same way as above
  for(i in 1:Ntotal){
      //instead of evaluating the likelihood of the observed goal, we sample a goal level from the distribution of the predicted goal. This distribution has a mean of predicted_goal (i.e., the goal predicted by the model), and a sd equal to sigma. The sigma parameter is needed to model the uncertainty in the predicted goal level.
    sampled_goal[i] = normal_rng(predicted_goal[trial_index[i]],sigma_goal);
    sampled_eff[i] = normal_rng(predicted_effort[i],sigma_effort);
    sampled_perf[i] = normal_rng(predicted_effort[i],sigma_perf);

  }
}
