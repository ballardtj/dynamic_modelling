#clear workspace
rm(list=ls())

#Load packages
library(lavaan)
library(tidyverse)

#--------------------------------------------------------
#set parameters for simulation
Nsubj<-100000

#generating parameters
dperf_int = 0.3 #intercept of change score at t1
dperf_var = 1  #variance of change score at t1
perf1_int = 2  #intecept of X1
perf1_var = 1 #variance of X1
sf = .3 #self-feedback parameter

#----------------------------------------------
# Simulate dynamic model

#Create function to simulate dynamic model
simulate_dynamic_model  <- function(X1_int,X1_var,dX_int,dX_var,sf,t,Nsubj){
  simulated_data = matrix(NA,nrow=Nsubj,ncol=t)   
  
  #Initialise
  simulated_data[,1] <- rnorm(n=Nsubj,mean=X1_int,sd=sqrt(X1_var))
  
  for(i in 2:t){
    change = dX_int + sf*simulated_data[,i-1] +  rnorm(n=Nsubj,mean=0,sd=sqrt(dX_var))
    simulated_data[,i] = simulated_data[,i-1] + change
  } 
  return(as.data.frame(simulated_data))
}

simulated_data=simulate_dynamic_model(perf1_int,perf1_var,dperf_int,dperf_var,sf,t=10,Nsubj)
names(simulated_data) <- str_replace(names(simulated_data),"V","perf")

save(simulated_data,file="../clean_data/simulated_univariate_dynamic_data.RData")
