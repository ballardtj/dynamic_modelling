data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] perf;
  matrix[Nsubj,Nobs] goal;
  int<lower=0> condition[Nsubj];
}  
 
parameters {
  real perf1_int[2];
  real dperf_int[2];
  real perf_sf[2];
  real<lower=0> perf1_sd[2];
  real<lower=0> dperf_sd[2];
  
  real goal1_int[2];
  real dgoal_int[2];
  real goal_sf[2];
  real<lower=0> goal1_sd[2];
  real<lower=0> dgoal_sd[2];
  
  real dperf_on_goal[2];
  real dgoal_on_perf[2];
}

model {
  real perf_change;
  real goal_change;
  
  for(i in 1:Nsubj){
    perf[i,1] ~ normal(perf1_int[condition[i]],perf1_sd[condition[i]]);
    goal[i,1] ~ normal(goal1_int[condition[i]],goal1_sd[condition[i]]);
    for(j in 2:Nobs){
      perf_change = dperf_int[condition[i]] + perf[i,j-1]*perf_sf[condition[i]] + goal[i,j-1]*dperf_on_goal[condition[i]];
      perf[i,j] ~ normal(perf[i,j-1] + perf_change,dperf_sd[condition[i]]);
      
      goal_change = dgoal_int[condition[i]] + goal[i,j-1]*goal_sf[condition[i]] + perf[i,j-1]*dgoal_on_perf[condition[i]];
      goal[i,j] ~ normal(goal[i,j-1] + goal_change,dgoal_sd[condition[i]]);
    }
  }
}