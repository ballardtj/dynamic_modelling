data {
  int Ntotal;  //Total number of trials in the dataset (600)
  int Nsubj;
  int Ntrial;
  real trial[Ntotal];           //Trial number
  real time[Ntotal];
  real goal[Nsubj*Ntrial];
  real eff[Ntotal];
  real score[Ntotal];
}

parameters {
  real personality;             //initial level of self-efficacy
  real k;                       //affect change in self-efficacy
  real ability;                 //places max on performance

  real<lower=0> value;
  real<lower=0> sigma_goal;          //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma_effort;          //initialize single sigma parameter for entire sample and set lower bound at 0.
  real<lower=0> sigma_perf;          //initialize single sigma parameter for entire sample and set lower bound at 0.
}

model {
  int trial_index;
  vector[Nsubj*Ntrial] predicted_goal;          //initialize predicted goal level object to store predictions
  vector[Ntotal] predicted_performance;
  vector[Ntotal] predicted_self_efficacy;
  vector[Ntotal] predicted_effort;

  //PRIORS
  personality ~ normal(0,1);          //set prior on alpha
  k ~ normal(0,1);                    //set prior on beta
  ability ~ normal(0,1);              //set prior on sigma
  sigma_goal ~ normal(0,1);
  sigma_effort ~ normal(0,1);
  sigma_perf ~ normal(0,1);
  value ~ normal(0,1);

  //LIKELIHOOD
  //loop through all trials in the dataset performing bracketed operations on each one

  trial_index = 0;
  for(i in 1:Ntotal){

    if(time[i]==1){
      //predict goal
      trial_index = trial_index + 1;
      predicted_goal[trial_index] = predicted_self_efficacy[i-1]*value;


      predicted_effort[i] = goal[trial_index]; //approach only
      predicted_performance[i] = fmin(predicted_effort[i]*ability,ability); // should replace this with saturating exponential
      capacity = predicted_performance[i]/predicted_effort[i];

      if(trial[i]==1){
        change_in_self_efficacy = k*(capacity-personality);
      }
      if(trial[i]>1){
        change_in_self_efficacy = k*(capacity-predicted_self_efficacy[i-1]);
      }

      predicted_self_efficacy[i] = predicted_self-efficacy[i-1] + change_in_self_efficacy;
    }

    if(time[i]>1){

       change_in_effort = goal - score;
       predicted_effort[i] + predicted_effort[i-1] + change_in_effort; //Jeff's model multitplies change_in_effort x time step, but since time step is an arbitrary constant we omit this.
       predicted_performance[i] = fmin(predicted_effort[i]*ability,ability); // should replace this with saturating exponential
       capacity = predicted_performance[i]/predicted_effort[i];
       change_in_self_efficacy = k*(capacity-self_efficacy[i-1]);
       predicted_self_efficacy[i] = predicted_self-efficacy[i-1] + change_in_self_efficacy;
    }

  }

  goal ~ normal(predicted_goal,sigma_goal);
  effort ~ normal(predicted_effort,sigma_effort);
  score ~ normal(predicted_performance,sigma_perf);

}

//NOTE: The generated quantities block (below) is NOT needed for the model to run. However, it can be useful for generating posterior predictives from the model. The posterior predictives from this model were presented in the "model evalutation" section of the paper.

// generated quantities {
//   real predicted_goal;         //initialize object to store goal level predicted by the model
//   real sampled_goal[Ntotal];   //initialize object to store set of goal level samples (i.e., the posterior predictives)
//
//   //loop through all trials in the dataset generating predictions in the same way as above
//   for(i in 1:Ntotal){
//     if(trial[i]==1){
//       predicted_goal = observed_goal[i];
//     }
//     if(trial[i]>1){
//       predicted_goal += alpha*(performance[i-1]-predicted_goal) + beta;
//     }
//
//     //instead of evaluating the likelihood of the observed goal, we sample a goal level from the distribution of the predicted goal. This distribution has a mean of predicted_goal (i.e., the goal predicted by the model), and a sd equal to sigma. The sigma parameter is needed to model the uncertainty in the predicted goal level.
//     sampled_goal[i] = normal_rng(predicted_goal,sigma);
//   }
// }
