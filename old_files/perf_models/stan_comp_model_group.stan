data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] perf;
  matrix[Nsubj,Nobs] goal;
}  
 
parameters {
  real<lower=0,upper=1> alpha;
  real beta;
  real<lower=0> error;
}

model {
  real expected_perf;
  real predicted_goal;
  
  for(i in 1:Nsubj){
    expected_perf = goal[i,1]-beta;
    //goal[i,1] ~ normal(goal1_int,goal1_sd[condition[i]]);
    for(j in 2:Nobs){
      expected_perf = expected_perf + alpha*(perf[i,j-1] - expected_perf);
      predicted_goal = expected_perf + beta;
      //or simply: goal_change = alpha*(perf[i,j-1] - expected_perf)
      goal[i,j] ~ normal(predicted_goal,error);
    }
  }
}