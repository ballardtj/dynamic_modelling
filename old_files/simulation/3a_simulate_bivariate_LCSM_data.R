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
# dperf_var = 1   #variance of change score at t1
# perf1_int = 2   #intecept of X1
# perf1_var = 1   #variance of X1
# perf_sf = 0.4
#          
# dgoal_int = 0.5
# dgoal_var = 0.4
# goal1_int = 3
# goal1_var = 3.5
# goal_sf = 0.3
#          
# dperf_on_goal = 0.2 #coupling parameter for dperf (how much it is affected by goal)
# dgoal_on_perf = 0.1 #coupling parameter for dgoal (how much it is affected by perf)
# t1_cov = 0.4  #t1 covariance
# d_cov = 0.2  #covariance in change scores

#----------------------------------------------
# Simulate LSC model
source("./models/LCSM_perf_goal_sim.R")

simulated_data <-simulateData(model,sample.nobs = Nsubj,meanstructure = T) #Simulate data

simulated_data = simulated_data %>% 
  select(perf1,perf2,perf3,perf4,perf5,perf6,perf7,perf8,perf9,perf10,
         goal1,goal2,goal3,goal4,goal5,goal6,goal7,goal8,goal9,goal10) #reorder perf variables because Lavaan has them in wrong order

save(simulated_data,file="../clean_data/simulated_bivariate_LCSM_data.RData")
