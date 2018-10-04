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


fit_hier = stan(file="./models/stan_univariate_hierarchical.stan",
           data=model_data,cores=4,control=list(adapt_delta=.95))

fit_hier_unc = stan(file="./models/stan_univariate_hierarchical_uncentered.stan",
                data=model_data,cores=4)

save(fit_hier,file="../model_output/stan_univariate_hierarchical_samples.RData")

# smry=summary(fit)
# smry$summary
# 
# write.csv(smry$summary,file="../model_output/stan_univariate_hierarchical_smry.csv")
