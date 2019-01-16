//version 21 - like version 21 except without skill influence on performance slope

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
  real<lower=0,upper=1> alpha;
  real<lower=0,upper=10> effort_baseline;
  real gamma;
  real<lower=0,upper=10> effort_0;

  //paramaeter relating to skill variable
  real<lower=0.05,upper=1> delta;

  //parameters relating to performance variable
  real gain20;
  real<lower=0> gain21;            //effort on score
  real<lower=0> gain22;            //ability on score
  real<lower=0> gain23;

  //parameters relating to goal variable
  real<lower=0,upper=1> theta;
  real lambda;
  real<lower=0,upper=10> goal_0;

  //parmaters for measurement model
  real<lower=0> sigma_effort_0;
  real<lower=0> sigma_effort_change;
  real<lower=0> sigma_performance_change;
  real<lower=0> sigma_goal_0;
  real<lower=0> sigma_goal_change;
}

transformed parameters {
  //In this section, the model calculates the predicted changes in the values of effort, performance, skill and goal.

  //initialize variables that are calculated from parameters and observed variables
  real gpd[Ntotal];
  real predicted_change_in_effort[Ntotal];
  real predicted_effort[Ntotal];
  real predicted_skill[Ntotal];
  real predicted_change_in_performance[Ntotal];
  real predicted_performance[Ntotal];
  real predicted_change_in_goal[Nglobal_trial];
  real predicted_goal[Nglobal_trial];

  for(i in 1:Ntotal){

     // ##### CALCULATE GOAL LEVEL #####

    if(time[i] == 1){
      if(trial[i] == 1){
        /*if the first goal striving episode, predicted goal is equal to initial goal parameter.
        This is because there are no previous values of the goal and performance variables,
        which are needed for calculating change in goal. Thus, we just treat goal at the first
        episode as a free parameter.*/
        predicted_change_in_goal[global_trial_number[i]] = 0;
        predicted_goal[global_trial_number[i]] = goal_0;
      }

      if(trial[i] > 1){
        //if not the first trial, the change in goal level is calculated according to Equation 2.
        predicted_change_in_goal[global_trial_number[i]] = theta*(predicted_performance[i-1]-predicted_goal[global_trial_number[i]-1]) + lambda;
        predicted_goal[global_trial_number[i]] = predicted_goal[global_trial_number[i]-1] + predicted_change_in_goal[global_trial_number[i]];
      }
    }

    // ##### CALCULATE EFFORT #####

    if(time[i] == 1){
      /*if start of goal striving episode, predicted effort is equal to initial effort parameter.
      This is because we do not have there are no previous values of the effort and gpd variables,
      which are needed for calculating change in effort. Thus, we just treat effort at the first
      time point within each episode as a free parameter. */
      gpd[i] = predicted_goal[global_trial_number[i]]; //Note, gpd represents the gpd at the start of the time window (not sure how this should be indexed in the paper)
      predicted_change_in_effort[i] = 0;
      predicted_effort[i] = effort_0;
    }

    if(time[i] > 1 ){
      //if not start of goal striving episode, the change in effort is calculated according to Equation 1.
      gpd[i] = predicted_goal[global_trial_number[i]] - predicted_performance[i-1];
      predicted_change_in_effort[i] = alpha*(effort_baseline  - predicted_effort[i-1]) + gamma*(gpd[i]-gpd[i-1]);
      predicted_effort[i] = predicted_effort[i-1] + predicted_change_in_effort[i];
    }

    // ##### CALCULATE SKILL #####

    //skill is determined according to Equation 2.
    predicted_skill[i] = 1-exp(-delta*practice[i]);

    // ##### CALCULATE PERFORMANCE #####

    //predicted change in performance is calculated according to Equation 4 ## NEED TO UPDATE THIS!
    predicted_change_in_performance[i] = (gain23 + gain22*predicted_skill[i]) / (1 + exp(-(gain20 + gain21*predicted_effort[i]  ) ));

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
  real change_in_effort;
  real change_in_performance;
  real change_in_goal;

  //Specify Priors

  //priors for parameters relating to effort variable
  //alpha is bounded between 0 and 1, so by default will have a uniform prior within this range.
  //effort_baseline is bounded between 0 and 10, so by default will have a uniform prior within this range.
  gamma ~ normal(0,1);
  //effort_0 is bounded between 0 and 10, so by default will have a uniform prior within this range.

  //priors for paramaeters relating to skill variable
  //delta is bounded between 0.05 and 1, so by default will have a uniform prior within this range.

  //priors for parameters relating to performance variable
  gain20 ~ normal(0,5);
  gain21 ~ normal(0,1);
  gain22 ~ normal(0,5);
  gain23 ~ normal(0,1);

  //priors for parameters relating to goal variable
  //theta is bounded between 0 and 1, so by default will have a uniform prior within this range.
  lambda ~ normal(0,1);

  //priors parmaters for standard deviation parameters
  sigma_effort_0 ~ normal(0,1);
  sigma_effort_change ~ normal(0,1);
  sigma_performance_change ~ normal(0,1);
  sigma_goal_0 ~ normal(0,1);
  sigma_goal_change ~ normal(0,1);

  //LIKELIHOOD
  for(i in 1:Ntotal){

    //Evaluate likelihood of observed goal level
    if( time[i] == 1){

      if(trial[i]==1){

        goal[global_trial_number[i]] ~ normal(predicted_goal[global_trial_number[i]],sigma_goal_0);
      }

      if(trial[i]>1){

        change_in_goal = goal[global_trial_number[i]]-goal[global_trial_number[i]-1];
        change_in_goal ~ normal(predicted_change_in_goal[global_trial_number[i]],sigma_goal_change);

      }
    }

     //Evaluate likelihood of observed effort and performance observations

    if( time[i] == 1 ){
      //if it's the first observation in the goal striving episode...
      effort[i] ~ normal(predicted_effort[i],sigma_effort_0);
      performance[i] ~ normal(predicted_performance[i],sigma_performance_change);
    }

    if( time[i] > 1 ){

       change_in_effort = effort[i]-effort[i-1];
       change_in_effort ~ normal(predicted_change_in_effort[i],sigma_effort_change);

       change_in_performance = performance[i]-performance[i-1];
       change_in_performance ~ normal(predicted_change_in_performance[i],sigma_performance_change); //T[0,];

    }
  }
}

//NOTE: The generated quantities block (below) is NOT needed for the model to run. However, it can be useful for generating posterior predictives from the model. The posterior predictives from this model were presented in the "model evalutation" section of the paper.

generated quantities {
  real sampled_effort[Ntotal];
  real sampled_performance[Ntotal];
  real sampled_goal[Ntotal];

  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){
    if(time[i]==1){
      sampled_performance[i] = normal_rng(predicted_performance[i],sigma_performance_change);
      sampled_effort[i] = normal_rng(predicted_effort[i],sigma_effort_0);
      if(trial[i]==1){
        sampled_goal[i] = normal_rng(predicted_goal[global_trial_number[i]],sigma_goal_0);
      }
      if(trial[i]>1){
        sampled_goal[i] = sampled_goal[i-1] + normal_rng(predicted_change_in_goal[global_trial_number[i]],sigma_goal_change);
      }
    }
    if(time[i]>1){
      sampled_goal[i] = sampled_goal[i-1];
      sampled_effort[i] = sampled_effort[i-1] + normal_rng(predicted_change_in_effort[i],sigma_effort_change);
      sampled_performance[i] = sampled_performance[i-1] + normal_rng(predicted_change_in_performance[i],sigma_performance_change);
    }

    //if the trial being considered is the first trial for that subject...
    //evaluate likelihood of observed goal given a normal distribution with mean = predicted_goal and sd = sigma
    // sampled_effort[i] = normal_rng(predicted_effort[i],2);
    // sampled_performance[i] = normal_rng(predicted_performance[i],2);
    //sampled_goal[i] = normal_rng(predicted_goal[i],sigma3);
  }
}
