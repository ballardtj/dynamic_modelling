data {
  int Ntotal;                   //Total number of trials in the dataset (600)
  real trial[Ntotal];           //Trial number
  real observed_goal[Ntotal];   //Goal level for each trial
  real performance[Ntotal];     //Performance for each trial
}  
 
parameters {
  real alpha;                   //initialize single alpha parameter for entire sample
  real beta;                    //initialize single beta parameter for entire sample
  real<lower=0> sigma;          //initialize single sigma parameter for entire sample and set lower bound at 0.
}

model {
  real predicted_goal;          //initialize predicted goal level object to store predictions
  
  //PRIORS
  alpha ~ normal(0,1);          //set weekly informative prior on alpha
  beta ~ normal(0,1);           //set weekly informative prior on beta
  sigma ~ normal(0,1);          //set weekly informative prior on sigma
  
  //LIKELIHOOD
  //loop through all trials in the dataset performing bracketed operations on each one
  for(i in 1:Ntotal){         
    //if the trial being considered is the first trial for that subject... 
    if(trial[i]==1){            
      //set predicted_goal to be equal to observed_goal for that trial
      predicted_goal = observed_goal[i];  
    }
    //if the trial being considered is not the first trial for that subject...
    if(trial[i]>1){                         
      //increment predicted_goal according to the theory of change
      predicted_goal += alpha*(performance[i-1]-predicted_goal) + beta; 
    }
    //evaluate likelihood of observed goal given a normal distribution with mean = predicted_goal and sd = sigma
    observed_goal[i] ~ normal(predicted_goal,sigma);
  }
}

//NOTE: The generated quantities block (below) is NOT needed for the model to run. However, it can be useful for generating posterior predictives from the model. The posterior predictives from this model were presented in the "model evalutation" section of the paper.

generated quantities {
  real predicted_goal;         //initialize object to store goal level predicted by the model
  real sampled_goal[Ntotal];   //initialize object to store set of goal level samples (i.e., the posterior predictives)

  //loop through all trials in the dataset generating predictions in the same way as above
  for(i in 1:Ntotal){
    if(trial[i]==1){
      predicted_goal = observed_goal[i];
    }
    if(trial[i]>1){
      predicted_goal += alpha*(performance[i-1]-predicted_goal) + beta;
    }

    //instead of evaluating the likelihood of the observed goal, we sample a goal level from the distribution of the predicted goal. This distribution has a mean of predicted_goal (i.e., the goal predicted by the model), and a sd equal to sigma. The sigma parameter is needed to model the uncertainty in the predicted goal level.
    sampled_goal[i] = normal_rng(predicted_goal,sigma);
  }
}