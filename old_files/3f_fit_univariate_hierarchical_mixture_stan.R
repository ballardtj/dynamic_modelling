#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)

#load data
load(file="../clean_data/wide_data.RData")

#prepare data for stan
vars = names(wide_data)[grep('perf',names(wide_data))]

model_data = list(
  Nsubj = dim(wide_data)[1],
  Nobs = length(vars),
  perf = as.matrix( wide_data[,vars])
)



fit = stan(file="./models/stan_univariate_hierarchical_mixture.stan",
           data=model_data,
           chains=4,
           iter=2000,
           warmup=1000,
           thin=1,
           cores=4,
           control = list(max_treedepth = 15,adapt_delta=0.99))

parms = c("mixing_prop[1]" ,   "mixing_prop[2]","perf1_int[1]",      "perf1_int[2]",      "perf1_sd[1]",       "perf1_sd[2]" ,     
          "dperf_int_mean[1]", "dperf_int_mean[2]", "dperf_int_sd[1]",   "dperf_int_sd[2]" ,  "dperf_sd_mean[1]",  "dperf_sd_mean[2]",  "dperf_sd_sd[1]",    "dperf_sd_sd[2]",   
          "sf_mean[1]",        "sf_mean[2]" ,       "sf_sd[1]",          "sf_sd[2]")

parms = c("mixing_prop[1]" ,   "mixing_prop[2]","perf1_int[1]",      "perf1_int[2]",      "perf1_sd[1]",       "perf1_sd[2]" ,     
          "dperf_int_mean[1]", "dperf_int_mean[2]", "dperf_int_sd[1]",   "dperf_int_sd[2]" ,     
          "sf_mean[1]",        "sf_mean[2]" ,       "sf_sd[1]",          "sf_sd[2]")


parms = c("mixing_prop[1]" ,   "mixing_prop[2]","perf1_int[1]",      "perf1_int[2]",      "perf1_sd[1]",       "perf1_sd[2]" ,     
          "dperf_int_mean[1]", "dperf_int_mean[2]", "dperf_int_sd")

parms = c("mixing_prop[3]" ,   "mixing_prop[4]","perf1_int[1]",      "perf1_int[2]",      "perf1_sd[1]",       "perf1_sd[2]" ,     
          "dperf_int_sd[1]",   "dperf_int_sd[2]" ,  "dperf_sd[1]",  "dperf_sd[2]")

parms = c("mixing_prop[3]" ,   "mixing_prop[4]","perf1_int[1]",      "perf1_int[2]",      "perf1_sd[1]",       "perf1_sd[2]" ,     
          "dperf_int[1]", "dperf_int[2]", "dperf_sd[1]")

parms = c("mixing_prop[3]" ,   "mixing_prop[4]","perf1_int","perf1_sd",   
          "dperf_int[1]", "dperf_int[2]", "dperf_sd[1]")

parms=c("mixing_prop[59]",   "mixing_prop[60]",  
"perf1_int[1]",      "perf1_int[2]",      "perf1_sd[1]"  ,     "perf1_sd[2]",      
"dperf_int_mean[1]", "dperf_int_mean[2]", "dperf_int_sd[1]",   "dperf_int_sd[2]" , 
"dperf_sd_mean[1]",  "dperf_sd_mean[2]",  "dperf_sd_sd[1]",    "dperf_sd_sd[2]",   
"sf_mean[1]"   ,     "sf_mean[2]",        "sf_sd[1]"  ,        "sf_sd[2]") 

traceplot(fit,pars=parms)
save(fit,file="../model_output/univariate_mixture_stan_samples.RData")

smry=summary(fit)
smry$summary
pairs(fit,pars=parms)

rownames(smry$summary)

write.csv(smry$summary,file="../model_output/univariate_mixture_stan_smry.csv")
