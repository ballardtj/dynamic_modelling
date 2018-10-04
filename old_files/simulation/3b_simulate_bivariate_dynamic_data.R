#clear workspace
rm(list=ls())

#Load packages
library(tidyverse)
library(MASS)

#--------------------------------------------------------
#set parameters for simulation
Nsubj<-10000

#----------------------------------------------
# Simulate dynamic model

#Create function to simulate dynamic model
simulate_dynamic_model  <- function(dperf_int = 0.3, #intercept of change score at t1
                                    dperf_var = 1,   #variance of change score at t1
                                    perf1_int = 2,   #intecept of X1
                                    perf1_var = 1,   #variance of X1
                                    perf_sf = 0.4,
                                    
                                    dgoal_int = 0.5,
                                    dgoal_var = 0.4,
                                    goal1_int = 3,
                                    goal1_var = 3.5,
                                    goal_sf = 0.3,
                                    
                                    dperf_on_goal = 0.2, #coupling parameter for dperf (how much it is affected by goal)
                                    dgoal_on_perf = 0.1, #coupling parameter for dgoal (how much it is affected by perf)
                                    t1_cov = 0.4,  #t1 covariance
                                    d_cov = 0.2,  #covariance in change scores
                                    t,
                                    Nsubj){
  
  perf = matrix(NA,nrow=Nsubj,ncol=t)   
  goal = matrix(NA,nrow=Nsubj,ncol=t)
  
  #Initialise
  t1 = mvrnorm(n=Nsubj,mu=c(perf1_int,goal1_int),Sigma=matrix(c(perf1_var,t1_cov,t1_cov,goal1_var),ncol=2),empirical = T)
  perf[,1] <-  t1[,1]
  goal[,1] <-  t1[,2]
  
  for(i in 2:t){
    #Note for some reason need to add t1_cov to desired d_cov when simulating errors to make changes achieve desired covariance.
    #Laavan appears to have the same problem though.
    #change_error = mvrnorm(n=Nsubj,mu=c(0,0),Sigma=matrix(c(dperf_var,(d_cov+t1_cov),(d_cov+t1_cov),dgoal_var),ncol=2),empirical = F)
    change_error = mvrnorm(n=Nsubj,mu=c(0,0),Sigma=matrix(c(dperf_var,d_cov,d_cov,dgoal_var),ncol=2),empirical = F)
    
    
    perf_change = dperf_int + perf_sf*perf[,i-1] + dperf_on_goal*goal[,i-1] + change_error[,1]
    perf[,i] = perf[,i-1] + perf_change
    
    goal_change = dgoal_int + goal_sf*goal[,i-1] + dgoal_on_perf*perf[,i-1] + change_error[,2]
    goal[,i] = goal[,i-1] + goal_change
  } 
  perf_data = as.data.frame(perf)
  names(perf_data)=str_replace(names(perf_data),"V","perf")
  goal_data = as.data.frame(goal)
  names(goal_data)=str_replace(names(goal_data),"V","goal")
  return(bind_cols(perf_data,goal_data))
}

simulated_bivariate_data=simulate_dynamic_model(t=10,Nsubj=Nsubj)

save(simulated_bivariate_data,file="../clean_data/simulated_bivariate_data.RData")
