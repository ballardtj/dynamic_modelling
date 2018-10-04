data {
  int Ntotal;                   //Total number of trials in the dataset (600)
  real trial[Ntotal];           //Trial number
  real observed_goal[Ntotal];   //Goal level for each trial
  real performance[Ntotal];     //Performance for each trial
  int Nsubj;                    //Number of subjects
  int subject[Ntotal];          //Subject number
}  
 
parameters {
  real alpha[Nsubj];            //initialize unique alpha parameter for each subject
  real beta[Nsubj];             //initialize unique beta parameter for each subject
  real<lower=0> sigma;          //initialize single sigma parameter for entire sample and set lower bound to 0.
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
      //increment predicted_goal according to the theory of change, using parameters associated with the individual subject
      predicted_goal += alpha[subject[i]]*(performance[i-1]-predicted_goal) + beta[subject[i]];
    }
    //evaluate likelihood of observed goal given a normal distribution with mean = predicted_goal and sd = sigma
    observed_goal[i] ~ normal(predicted_goal,sigma);
  }
}