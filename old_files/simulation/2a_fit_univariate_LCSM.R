#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(lavaan)

#load data
load(file="../clean_data/simulated_univariate_dynamic_data.RData")

#source model
source("models/LCSM_perf_v2.R")

#fit model
fit <- lavaan(model, data=simulated_data,estimator='mlr',fixed.x=FALSE,missing='fiml')

summary(fit)

#recovers well with 1000 subjects
