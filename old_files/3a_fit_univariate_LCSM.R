#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(lavaan)
library(semPlot)

#load data
load(file="../clean_data/wide_data.RData")

#source model
source("models/LCSM_perf.R")

#fit model
fit <- lavaan(model, data=wide_data,estimator='mlr',fixed.x=FALSE,missing='fiml')

summary(fit)

#source model
source("models/LCSM_perf_v2.R")

#fit model
fit2 <- lavaan(model, data=wide_data,estimator='mlr',fixed.x=FALSE,missing='fiml')

summary(fit2)


#semPaths(model, title = FALSE, curvePivot = TRUE)