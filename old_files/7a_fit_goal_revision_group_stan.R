#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(lavaan)

#load data
load(file="../clean_data/wide_data.RData")

#prepare data for stan
model_data = list(
  Nsubj = dim(wide_data)[1],
  Nobs = 10,
  perf = as.matrix(select(wide_data ,perf1:perf10)),
  goal = as.matrix(select(wide_data ,goal1:goal10)),
  condition = as.numeric(wide_data$condition) #1 = approach, #2 = avoidance
)


fit = stan(file="./models/stan_goal_revision_group.stan",
           data=model_data,cores=1)

save(fit,file="../model_output/goal_revision_group_samples.RData")

smry=summary(fit)
smry$summary

write.csv(smry$summary,file="../model_output/condition_stan_smry.csv")

#Model estimates well. No real differences in condition though
