

##############################
#### Univariate LCS Model ####
##############################

#Load packages
library(lavaan)
library(tidyverse)
library(reshape2)

#----------------------------------------------------------
#Step 1 simulate LCS model and recover 

#Fix sample size
samplesize<-1000
simulations<-100

#Parameter values
generating_parms = expand.grid(dX1_int = c(1,10,100),
                               X1_int = c(1,10,100),
                               dX1_var = c(0.1,1,10),
                               X1_var = c(0.1,1,10),
                               sf = c(-1,0,1))

#Specify the Univariate Latent Change Score model that will be fitted to simulated data
ULCS<-'

X2 ~ 1*X1       # Fixed regression of X2 on X1
dX1 =~ 1*X2     # Fixed regression of dX1 on X2
X2 ~ 0*1        # This line constrains the intercept of X2 to 0
X2 ~~ 0*X2      # This fixes the variance of the X2 to 0 

dX1 ~ 1             # This estimates the intercept of the change scores 
X1 ~  1             # This estimates the intercept of X1 
dX1 ~~  dX1         # This estimates the variance of the change scores 
X1 ~~   X1          # This estimates the variance of X1 
dX1~X1              # This estimates the self-feedback parameter

'

recovery_list=list()

for(i in 1:nrow(generating_parms)){
  
  print(i)
  
  #generating parameters
  dX1_int = generating_parms$dX1_int[i] #intercept of change score
  X1_int = generating_parms$X1_int[i]  #intecept of X1
  dX1_var = generating_parms$dX1_var[i]  #variance of change score
  X1_var = generating_parms$X1_var[i] #variance of X1
  sf = generating_parms$sf[i] #self-feedback parameter
  
  
  
  #Simulation specification
  ULCS_simulate<-paste0('

#####     The following lines specify the core assumptions of the LCS 
#####     and should not generally be modified

X2 ~ 1*X1       # Fixed regression of X2 on X1
dX1 =~ 1*X2     # Fixed regression of dX1 on X2
X2 ~ 0*1        # This line constrains the intercept of X2 to 0
X2 ~~ 0*X2      # This fixes the variance of the X2 to 0  


###### The following five parameters will be estimated in the model. 
###### Values can be modified manually to examine the effect on the model

dX1 ~ ',dX1_int,'*1          # This fixes the intercept of the change score to 10 
X1 ~ ',X1_int,'*1           # This fixes the intercept of X1 to 50 

dX1 ~~ ',dX1_var,'*dX1        # This fixes the variance of the change scores to 5. 
X1 ~~ ',X1_var,'*X1          # This fixes the variance of the X1 to 8. 

dX1 ~ ',sf,'*X1        # This fixes the self-feedback parameter to -0.1. 

')
  
  
  recovery=data.frame(dX1_int = rep(NA,simulations),X1_int=NA,dX1_var=NA,X1_var=NA,sf=NA,sim=NA)
  
  for(j in 1:simulations){
    #Simulate data
    #set.seed(1234)
    simdatULCS<-simulateData(ULCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
    
    
    #Fit model
    fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
    
    #Store parameter estimates
    recovery$dX1_int[i]=parameterEstimates(fitULCS)$est[5]
    recovery$X1_int[i]=parameterEstimates(fitULCS)$est[6]
    recovery$dX1_var[i]=parameterEstimates(fitULCS)$est[7]
    recovery$X1_var[i]=parameterEstimates(fitULCS)$est[8]
    recovery$sf[i]=parameterEstimates(fitULCS)$est[9]
    recovery$sim[i]=i
  }
  recovery$parms=i
  recovery_list[[i]]=recovery
  
}








plot_data = recovery %>% gather(key=parameter,value=value,dX1_int:sf)

ggplot(data=subset(plot_data,parameter=="sf")) +
  geom_line(aes(x=sim,y=value)) +
  facet_grid(.~parameter,scale="free")



summary(fitULCS, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

parameterEstimates(fitULCS)
