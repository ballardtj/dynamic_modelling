#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)

#load data
load(file="../clean_data/wide_data_v3.RData")

#prepare data for stan
vars = names(wide_data)[grep('perf',names(wide_data))]

model_data = list(
  Nsubj = dim(wide_data)[1],
  Nobs = length(vars),
  perf = as.matrix( wide_data[,vars])
)

fit_mix = stan(file="./models/stan_univariate_mixture.stan",
           data=model_data,cores=4)

save(fit_mix,file="../model_output/stan_univariate_mixture_samples.RData")