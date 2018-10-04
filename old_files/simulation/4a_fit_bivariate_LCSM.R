#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(lavaan)

#load data
load(file="../clean_data/simulated_bivariate_LCSM_data.RData")

#source model
source("models/LCSM_perf_goal.R")

#fit model
fit <- lavaan(model, data=simulated_data,estimator='mlr',fixed.x=T,missing='fiml')

summary(fit)

#recovers well with 1000 subjects
