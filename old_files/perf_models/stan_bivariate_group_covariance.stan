data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] perf;
  matrix[Nsubj,Nobs] goal;
}  
 
parameters {
  real perf1_int;
  real dperf_int;
  real perf_sf;
  real<lower=0> perf1_sd;
  real<lower=0> dperf_sd;
  
  real goal1_int;
  real dgoal_int;
  real goal_sf;
  real<lower=0> goal1_sd;
  real<lower=0> dgoal_sd;
  
  real dperf_on_goal;
  real dgoal_on_perf;
  real t1_cov;
  real d_cov;
}

transformed parameters {
  matrix[2, 2] t1_Sigma;
  matrix[2, 2] d_Sigma;
   
  t1_Sigma[1, 1] = square(perf1_sd); t1_Sigma[1, 2] = t1_cov;
  t1_Sigma[2, 1] = t1_cov;      t1_Sigma[2, 2] = square(goal1_sd);
  
  d_Sigma[1, 1] = square(dperf_sd); d_Sigma[1, 2] = d_cov;
  d_Sigma[2, 1] = d_cov;      d_Sigma[2, 2] = square(dgoal_sd);
}

model {
  vector[2] mu;
  vector[2] y;
  real perf_change;
  real goal_change;

  for(i in 1:Nsubj){
    
    mu[1] = perf1_int;
    mu[2] = goal1_int;
    y[1] = perf[i,1];
    y[2] = goal[i,1];
    y ~ multi_normal(mu, t1_Sigma);
    
    for(j in 2:Nobs){
      perf_change = dperf_int + perf[i,j-1]*perf_sf + goal[i,j-1]*dperf_on_goal;
      goal_change = dgoal_int + goal[i,j-1]*goal_sf + perf[i,j-1]*dgoal_on_perf;
      
      mu[1] = perf[i,j-1] + perf_change;
      mu[2] = goal[i,j-1] + goal_change;
      
      y[1] = perf[i,j];
      y[2] = goal[i,j];
   
      y ~ multi_normal(mu, d_Sigma);
    }
  }
}