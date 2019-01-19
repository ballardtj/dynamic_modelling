//version 34 - like version 25 except fully linear equations predicting levels of variables

//return to baseline model of effort

data {
  int Ntotal;                   //Total number of trials in the dataset (600)
  real trial[Ntotal];           //Trial number
  real time[Ntotal];
  real practice[Ntotal];
//  real difficulty[Ntotal];
  real effort[Ntotal];
  real performance[Ntotal];
  int Nglobal_trial;
  real goal[Nglobal_trial];
  int global_trial_number[Ntotal]; //change to episode number
}

parameters {
  //parameters relating to effort variable
  real effort_intercept;
  real effort_on_gpd;

  //paramaeter relating to skill variable
  real skill_intercept;
  real skill_on_practice;

  //parameters relating to performance variable
  real performance_intercept;
  real performance_on_effort;
  real performance_on_skill;

  //parameters relating to goal variable
  real goal_intercept;
  real goal_on_performance;

  //parmaters for measurement model
  real<lower=0> sigma_effort;
  real<lower=0> sigma_performance;
  real<lower=0> sigma_goal;
}

transformed parameters {
  //In this section, the model calculates the predicted changes in the values of effort, performance, skill and goal.

  //initialize variables that are calculated from parameters and observed variables
  real gpd[Ntotal];
  real predicted_effort[Ntotal];
  real predicted_skill[Ntotal];
  real predicted_change_in_performance[Ntotal];
  real predicted_performance[Ntotal];
  real predicted_goal[Nglobal_trial];


  for(i in 1:Ntotal){

     // ##### CALCULATE GOAL LEVEL #####

     if(time[i] == 1){
      if(trial[i] == 1){
        /*if the first goal striving episode, predicted goal is not modelled.
        This is because there are no previous values of the performance variable,
        which are needed for calculating the goal level. However, stan requires all
        observations of each variable to be set to a real number. So we just set goal
        to its observed value here.*/
        predicted_goal[global_trial_number[i]] = goal[global_trial_number[i]];
      }

      if(trial[i] > 1){
        //if not the first trial, the change in goal level is calculated based on the performance
        //at the end of the previous episode.
         predicted_goal[global_trial_number[i]] = goal_intercept + goal_on_performance*predicted_performance[i-1];
      }
    }

    // ##### CALCULATE EFFORT #####

    if(time[i] == 1){
      gpd[i] = predicted_goal[global_trial_number[i]]; //Note, gpd represents the gpd at the start of the time window (not sure how this should be indexed in the paper)
    }

    if(time[i] > 1 ){
      gpd[i] = predicted_goal[global_trial_number[i]] - predicted_performance[i-1];
    }

    predicted_effort[i] = effort_intercept + effort_on_gpd*gpd[i];

   // ##### CALCULATE SKILL #####

    predicted_skill[i] = skill_intercept + skill_on_practice*practice[i];

    // ##### CALCULATE PERFORMANCE #####

    //predicted change in performance is calculated according to Equation 4 ## NEED TO UPDATE THIS!
    predicted_change_in_performance[i] = performance_intercept + performance_on_effort*predicted_effort[i] +
        performance_on_skill*predicted_skill[i];

    if(time[i] == 1){
      //if start of goal striving episode, predicted performance is just equal to the predicted change in performance for
      //that time window, because initial performance is 0.
      predicted_performance[i] = predicted_change_in_performance[i];
    }

    if(time[i] > 1){
      //if not start of goal striving episode, predicted performance is incremented based on its previous value (according to Equation 3)
      predicted_performance[i] = predicted_performance[i-1] + predicted_change_in_performance[i];
    }
  }
}

model {
  //Initialize variables that are needed for calculating the likelihood function
  real change_in_performance;

  //Specify Priors

  //priors for parameters relating to effort variable
  effort_intercept ~ normal(0,2);
  effort_on_gpd ~ normal(0,2);

  //priors for paramaeters relating to skill variable
  //delta is bounded between 0.05 and 1, so by default will have a uniform prior within this range.
  skill_intercept ~ normal(0,2);
  skill_on_practice ~ normal(0,2);

  //priors for parameters relating to performance variable
  performance_intercept ~ normal(0,2);
  performance_on_effort ~ normal(0,2);
  performance_on_skill ~ normal(0,2);

  //priors for parameters relating to goal variable
  goal_intercept ~ normal(0,2);
  goal_on_performance ~ normal(0,2);

  //priors parmaters for standard deviation parameters
  sigma_effort ~ normal(0,2);
  sigma_performance ~ normal(0,2);
  sigma_goal ~ normal(0,2);

  //LIKELIHOOD
  for(i in 1:Ntotal){

    //Evaluate likelihood of observed goal level
    if( time[i] == 1){

      if(trial[i]>1){

        goal[global_trial_number[i]] ~ normal(predicted_goal[global_trial_number[i]],sigma_goal);

      }
    }

     //Evaluate likelihood of observed effort
    effort[i] ~ normal(predicted_effort[i],sigma_effort);

    //Evaluate likelihood of observed performance
    if( time[i] == 1 ){
      //if it's the first observation in the goal striving episode...
      performance[i] ~ normal(predicted_performance[i],sigma_performance);
    }

    if( time[i] > 1 ){

       change_in_performance = performance[i]-performance[i-1];
       change_in_performance ~ normal(predicted_change_in_performance[i],sigma_performance); //T[0,];

    }
  }
}

//NOTE: The generated quantities block (below) is NOT needed for the model to run. However, it can be useful for generating posterior predictives from the model. The posterior predictives from this model were presented in the "model evalutation" section of the paper.

generated quantities {
  real sampled_effort[Ntotal];
  real sampled_performance[Ntotal];
  real sampled_goal[Ntotal];
  real log_lik[Ntotal];
  real change_in_performance;

  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){


    sampled_effort[i] = normal_rng(predicted_effort[i],sigma_effort);

    if(time[i]==1){
      sampled_performance[i] = normal_rng(predicted_performance[i],sigma_performance);

      if(trial[i]==1){
        sampled_goal[i] = goal[global_trial_number[i]];
      }
      if(trial[i]>1){
        sampled_goal[i] = normal_rng(predicted_goal[global_trial_number[i]],sigma_goal);
      }
    }
    if(time[i]>1){
      sampled_goal[i] = sampled_goal[i-1];
      sampled_performance[i] = sampled_performance[i-1] + normal_rng(predicted_change_in_performance[i],sigma_performance);
    }

  //calculate log likelihood for observation
  if(time[i] == 1){
     if(trial[i]==1){
        log_lik[i] = normal_lpdf(effort[i] | predicted_effort[i],sigma_effort) +
                     normal_lpdf(performance[i] | predicted_performance[i],sigma_performance);
      }

     if(trial[i]>1){

        log_lik[i] = normal_lpdf(goal[global_trial_number[i]] | predicted_goal[global_trial_number[i]],sigma_goal) +
                     normal_lpdf(effort[i] | predicted_effort[i],sigma_effort) +
                     normal_lpdf(performance[i] | predicted_performance[i],sigma_performance);
      }
    }

    if(time[i] > 1){
      change_in_performance = performance[i]-performance[i-1];

      log_lik[i] = normal_lpdf(effort[i] | predicted_effort[i],sigma_effort) +
                   normal_lpdf(change_in_performance | predicted_change_in_performance[i],sigma_performance);

    }
  }
}
