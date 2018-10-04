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
  real alpha_mean;              //initialize population mean parameter for the alpha distribution
  real<lower=0>  alpha_sd;      //initialize population sd parameter for the alpha distribution and set lower bound to 0.
  real beta_mean;               //initialize population mean parameter for the beta distribution
  real<lower=0>  beta_sd;       //initialize population sd parameter for the beta distribution and set lower bound to 0.
}

model {
  real predicted_goal;          //initialize predicted goal level object to store predictions
  
  //PRIORS
  alpha ~ normal(alpha_mean,alpha_sd);  //set hierarchical prior on alpha
  beta ~ normal(beta_mean,beta_sd);     //set hierarchical prior on beta
  sigma ~ normal(0,1);                  //set weekly informative prior on sigma
  alpha_mean  ~ normal(0,1);            //set weekly informative prior on mean of alpha
  alpha_sd  ~ normal(0,1);              //set weekly informative prior on sd of alpha
  beta_mean  ~ normal(0,1);             //set weekly informative prior on mean of beta
  beta_sd  ~ normal(0,1);               //set weekly informative prior on sd of beta
  
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