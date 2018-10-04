#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)

#load data
load(file="../clean_data/wide_data.RData")

#prepare data for stan
model_data = list(
  Nsubj = dim(wide_data)[1],
  Nobs = 10,
  perf = as.matrix(select(wide_data ,perf1:perf10)),
  goal = as.matrix(select(wide_data ,goal1:goal10))
)

fit = stan(file="./models/stan_perf_goal_covariance.stan",
           data=model_data,cores=1)

save(fit,file="../model_output/bivariate_stan_cov_samples.RData")

smry=summary(fit)

write.csv(smry$summary,file="../model_output/bivariate_stan_cov_smry.csv")
