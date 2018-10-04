#clear workspace
rm(list=ls())

#Load packages
library(lavaan)
library(tidyverse)

#--------------------------------------------------------
#set parameters for simulation
Nsubj<-1000

#generating parameters
# dperf_int = 0.3 #intercept of change score at t1
# dperf_var = 1  #variance of change score at t1
# perf1_int = 2  #intecept of X1
# perf1_var = 1 #variance of X1
# sf = .3 #self-feedback parameter

#----------------------------------------------
# Simulate LSC model
source("./models/LCSM_perf_sim.R")

simulated_data <-simulateData(model,sample.nobs = Nsubj,meanstructure = T) #Simulate data

simulated_data = simulated_data %>% select(perf1,perf2,perf3,perf4,perf5,perf6,perf7,perf8,perf9,perf10) #reorder perf variables because Lavaan has them in wrong order

save(simulated_data,file="../clean_data/simulated_univariate_LCSM_data.RData")
