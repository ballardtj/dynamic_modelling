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
  perf = as.matrix( wide_data[,vars]),
  condition = as.numeric(wide_data$condition), #1=Approach, 2=Avoidance
  Ncond = 2
)


fit_mg = stan(file="./models/stan_univariate_multiple_group.stan",
           data=model_data,cores=4)

save(fit_mg,file="../model_output/stan_univariate_multiple_group_samples.RData")

