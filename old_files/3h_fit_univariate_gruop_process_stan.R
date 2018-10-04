#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)
library(HDInterval)

#load data
load(file="../clean_data/wide_data.RData")

#prepare data for stan
vars = names(wide_data)[grep('perf',names(wide_data))]

model_data = list(
  Nsubj = dim(wide_data)[1],
  Nobs = length(vars),
  perf = as.matrix( wide_data[,vars])
)


fit_group_process = stan(file="./models/stan_univariate_group_process.stan",
           data=model_data,cores=4)


