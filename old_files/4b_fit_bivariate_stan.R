#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)

#load data
load(file="../clean_data/wide_data_v4.RData")

#prepare data for stan
perf_vars = names(wide_data)[grep('perf',names(wide_data))]
diff_vars = names(wide_data)[grep('diff',names(wide_data))]

model_data = list(
  Nsubj = dim(wide_data)[1],
  Nobs = length(perf_vars),
  perf = as.matrix(wide_data[,perf_vars]),
  diff = as.matrix(wide_data[,diff_vars])
)


fit_biv = stan(file="./models/stan_bivariate_group.stan",
           data=model_data,cores=4)

save(fit_biv,file="../model_output/stan_bivariate_group_samples.RData")