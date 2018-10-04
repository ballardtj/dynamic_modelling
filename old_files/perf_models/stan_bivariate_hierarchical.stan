data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] perf;
  matrix[Nsubj,Nobs] goal;
}  
 
parameters {
  //subject level priors
  real dperf_int[Nsubj];
  //real perf_sf[Nsubj];
  real<lower=0> dperf_sd[Nsubj];
  
  real dgoal_int[Nsubj];
  //real goal_sf[Nsubj];
  real<lower=0> dgoal_sd[Nsubj];
  
  real sf[Nsubj];
  real dperf_on_goal[Nsubj];
  real dgoal_on_perf[Nsubj];
  
  //group level priors
  real perf1_int;
  real<lower=0> perf1_sd;
  
  real dperf_int_mean;
  real<lower=0> dperf_int_sd;
  
  //real perf_sf_mean;
  //real<lower=0> perf_sf_sd; 
  
  real<lower=0> dperf_sd_mean;
  real<lower=0> dperf_sd_sd;
  
  real goal1_int;
  real<lower=0> goal1_sd;
  
  real dgoal_int_mean;
  real<lower=0> dgoal_int_sd;
  
  //real goal_sf_mean;
  //real<lower=0> goal_sf_sd; 
  
  real<lower=0> dgoal_sd_mean;
  real<lower=0> dgoal_sd_sd;
  
  real sf_mean;
  real<lower=0> sf_sd;
  
  real dgoal_on_perf_mean;
  real<lower=0> dgoal_on_perf_sd;
  
  real dperf_on_goal_mean;
  real<lower=0> dperf_on_goal_sd;
}

model {
  real perf_change;
  real goal_change;
  
  //priors
  dperf_int ~ normal(dperf_int_mean,dperf_int_sd);
  //perf_sf ~ normal(perf_sf_mean,perf_sf_sd);
  dperf_sd ~ normal(dperf_sd_mean,dperf_sd_sd);
  
  dgoal_int ~ normal(dgoal_int_mean,dgoal_int_sd);
  //goal_sf ~ normal(goal_sf_mean,goal_sf_sd);
  dgoal_sd ~ normal(dgoal_sd_mean,dgoal_sd_sd);
  
  sf ~ normal(sf_mean,sf_sd);
  dperf_on_goal ~ normal(dperf_on_goal_mean,dperf_on_goal_sd);
  dgoal_on_perf ~ normal(dgoal_on_perf_mean,dgoal_on_perf_sd);
  
  for(i in 1:Nsubj){
    perf[i,1] ~ normal(perf1_int,perf1_sd);
    goal[i,1] ~ normal(goal1_int,goal1_sd);
    for(j in 2:Nobs){
      //perf_change = dperf_int[i] + perf[i,j-1]*perf_sf[i] + goal[i,j-1]*dperf_on_goal[i];
      perf_change = dperf_int[i] + perf[i,j-1]*sf[i] + goal[i,j-1]*dperf_on_goal[i];
      perf[i,j] ~ normal(perf[i,j-1] + perf_change,dperf_sd[i]);
      
      //goal_change = dgoal_int[i] + goal[i,j-1]*goal_sf[i] + perf[i,j-1]*dgoal_on_perf[i];
      goal_change = dgoal_int[i] + goal[i,j-1]*sf[i] + perf[i,j-1]*dgoal_on_perf[i];
      goal[i,j] ~ normal(goal[i,j-1] + goal_change,dgoal_sd[i]);
    }
  }
}