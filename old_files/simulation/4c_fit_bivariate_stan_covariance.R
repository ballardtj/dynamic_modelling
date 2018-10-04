#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)

#load data
load(file="../clean_data/simulated_bivariate_LCSM_data.RData")

#prepare data for stan
model_data = list(
  Nsubj = dim(simulated_data)[1],
  Nobs = 10,
  perf = as.matrix(select(simulated_data ,perf1:perf10)),
  goal = as.matrix(select(simulated_data ,goal1:goal10))
)

#The dynamic model misses the variance associated with
#the latent change score. I'm not sure why this is, but
#suspect it has something to do with the fact that
#the model is no longer conditionalising on y, so perhaps the 
#variance is no longer associated with just the change, it's 
#associated with the raw value.
# fit_dynamic = stan(file="./models/stan_perf_dynamic.stan",
#            data=model_data,cores=1)
# 
# #fit_static=fit
# fit_dynamic=fit
# round(summary(fit_dynamic)$summary,2)


#Static model recovers dynamically generated data and
#matches Lavaan's LCSM coeffcients for observed data.
fit = stan(file="./models/stan_perf_goal_covariance.stan",
           data=model_data,cores=1)

save(fit,file="../model_output/bivariate_stan_cov_to_LCSM_sim.RData")

smry=summary(fit)

write.csv(smry$summary,file="../model_output/bivariate_stan_cov_to_LCSM_sim_smry.csv")
