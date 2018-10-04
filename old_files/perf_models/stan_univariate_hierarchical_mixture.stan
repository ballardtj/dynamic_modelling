data {
  int<lower=0> Nsubj;
  int<lower=0> Nobs;
  matrix[Nsubj,Nobs] perf;
}  
 
parameters {
  vector<lower=0,upper=1>[Nsubj] mixing_prop; // mixing proportion
  
 ordered[2] perf1_int;
 vector<lower=0>[2] perf1_sd;
  //real<lower=0> perf1_sd;
  
  //real perf1_int;
  //real<lower=0> perf1_sd;
  
  vector[2] dperf_int_mean;
  //real<lower=0> dperf_int_sd;
  vector<lower=0>[2] dperf_int_sd;
  
  //real<lower=0> dperf_sd;
  // real<lower=0> dperf_sd_mean;
  // real<lower=0> dperf_sd_sd;
  // 
  // real sf_mean;
  // real<lower=0> sf_sd;
  // 
  vector<lower=0>[2] dperf_sd_mean;
  vector<lower=0>[2] dperf_sd_sd;

  
  vector[2] sf_mean;
  vector<lower=0>[2] sf_sd;
  // real<lower=0> sf_sd;
  
  //subject level parameters
  vector[Nsubj] dperf_int;
  vector<lower=0>[Nsubj] dperf_sd;
  vector[Nsubj] sf;
}

model {
  real perf_change;
  
  //hierarchical priors
  dperf_int_mean[1] ~ normal(4,1);
  dperf_int_mean[2] ~ normal(7,1);
  dperf_int_sd ~ normal(0,1);
  dperf_sd_mean ~ normal(0,2);
  dperf_sd_sd ~ normal(0,1);
  sf_mean ~ normal(0,2);
  sf_sd ~ normal(0,1);
  perf1_int ~ normal(0,2);
  perf1_sd ~ normal(0,1);
  
   // dperf_sd ~ normal(dperf_sd_mean,dperf_sd_sd);
   // sf ~ normal(sf_mean,sf_sd);
   // 
    
  //sample subject level parameters
  for(i in 1:Nsubj){
     // target += log_sum_exp(
     //              log_mix(mixing_prop[i],
     //                  normal_lpdf(dperf_int[i] | dperf_int_mean[1], dperf_int_sd),
     //                  normal_lpdf(dperf_int[i] | dperf_int_mean[2], dperf_int_sd)),
     //              log_mix(mixing_prop[i],
     //                   normal_lpdf(sf[i] | sf_mean[1], sf_sd),
     //                   normal_lpdf(sf[i] | sf_mean[2], sf_sd)));
                       
    vector[4] increments;
    
    increments[1]=log_mix(mixing_prop[i],
                  normal_lpdf(dperf_int[i] | dperf_int_mean[1], dperf_int_sd[1]),
                  normal_lpdf(dperf_int[i] | dperf_int_mean[2], dperf_int_sd[2]));
    //hierarchical prior on dperf_sd              
    increments[2]=log_mix(mixing_prop[i],
                  normal_lpdf(dperf_sd[i] | dperf_sd_mean[1], dperf_sd_sd[1]),
                  normal_lpdf(dperf_sd[i] | dperf_sd_mean[2], dperf_sd_sd[2])); 
    //hierarchical prior on sf              
    increments[3]=log_mix(mixing_prop[i],
                  normal_lpdf(sf[i] | sf_mean[1], sf_sd[1]),
                  normal_lpdf(sf[i] | sf_mean[2], sf_sd[2])); 

    //increment lp for initial performance
    increments[4]=log_mix(mixing_prop[i],
                  normal_lpdf(perf[i,1] | perf1_int[1], perf1_sd[1]),
                  normal_lpdf(perf[i,1] | perf1_int[2], perf1_sd[2]));
                  
    target+= log_sum_exp(increments);              
                       
    // target += log_sum_exp( 
    //             log(mixing_prop[i]) + normal_lpdf(dperf_int[i] | dperf_int_mean[1], dperf_int_sd),
    //             log1m(mixing_prop[i]) + normal_lpdf(dperf_int[i] | dperf_int_mean[2], dperf_int_sd),      
    //             log(mixing_prop[i]) + normal_lpdf(sf[i] | sf_mean[1], sf_sd),
    //             log1m(mixing_prop[i]) + normal_lpdf(sf[i] | sf_mean[2], sf_sd));
    // 

                            
    // target += log_mix(mixing_prop[i],
    //                    normal_lpdf(dperf_int[i] | dperf_int_mean[1], dperf_int_sd[1]),
    //                    normal_lpdf(dperf_int[i] | dperf_int_mean[2], dperf_int_sd[2]));

    

    //target += log_mix(mixing_prop[i],
    //                  normal_lpdf(dperf_sd[i] | dperf_sd_mean[1], dperf_sd_sd[1]),
    //                  normal_lpdf(dperf_sd[i] | dperf_sd_mean[2], dperf_sd_sd[2]));
// 
//     target += log_mix(mixing_prop[i],
//                        normal_lpdf(sf[i] | sf_mean[1], sf_sd),
//                        normal_lpdf(sf[i] | sf_mean[2], sf_sd));
    // 
    //target += log_mix(mixing_prop[i],
    //                   normal_lpdf(perf[i,1] | perf1_int[1], perf1_sd),
    //                   normal_lpdf(perf[i,1] | perf1_int[2], perf1_sd));
    
    //perf[i,1] ~ normal(perf1_int,perf1_sd);
    
    for(j in 2:Nobs){
      perf_change = dperf_int[i] + perf[i,j-1]*sf[i]; //predicted change for mixture 1
      //perf_change[2] = dperf_int[i] + perf[i,j-1]*sf[i]; //predicted change for mixture 2
      perf[i,j] ~ normal( perf[i,j-1] + perf_change, dperf_sd[i]);
      //target += log_mix(mixing_prop[i],
      //                  normal_lpdf(perf[i,j] | perf[i,j-1] + perf_change[1], dperf_sd[i]),
      //                  normal_lpdf(perf[i,j] | perf[i,j-1] + perf_change[2], dperf_sd[i]));
    }
  }
}