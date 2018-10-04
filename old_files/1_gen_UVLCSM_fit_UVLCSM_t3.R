

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


#generating parameters
dX1_int = 1 #intercept of change score at t1
dX2_int = 1 #intercept of change score at t2
dX1_var = 1  #variance of change score at t1
dX2_var = 1  #variance of change score at t2
X1_int = 2  #intecept of X1
X1_var = 0.2 #variance of X1
sf = 0 #self-feedback parameter

#Simulation specification
ULCS_simulate<-paste0('
                      
    #####     The following lines specify the core assumptions of the LCS 
    #####     and should not generally be modified
                      
    X2 ~ 1*X1       # Fixed regression of X2 on X1
    X3 ~ 1*X2       # Fixed regression of X3 on X2

    dX1 =~ 1*X2     # Fixed regression of dX1 on X2
    dX2 =~ 1*X3     # Fixed regression of dX2 on X3

    X2 ~ 0*1        # This line constrains the intercept of X2 to 0
    X3 ~ 0*1        # This line constrains the intercept of X3 to 0

    X2 ~~ 0*X2      # This fixes the variance of the X2 to 0  
    X3 ~~ 0*X3      # This fixes the variance of the X3 to 0               
                      
    ###### The following five parameters will be estimated in the model. 
    ###### Values can be modified manually to examine the effect on the model
                      
    dX1 ~ ',dX1_int,'*1          # This fixes the intercept of the T1 change score to 10
    dX2 ~ ',dX2_int,'*1          # This fixes the intercept of the T2 change score to 10

    X1 ~ ',X1_int,'*1           # This fixes the intercept of X1 to 50 
                      
    dX1 ~~ ',dX1_var,'*dX1        # This fixes the variance of the change scores to 5. 
    dX2 ~~ ',dX2_var,'*dX2        # This fixes the variance of the change scores to 5. 

    X1 ~~ ',X1_var,'*X1          # This fixes the variance of the X1 to 8. 
                      
    dX1 ~ ',sf,'*X1        # This fixes the self-feedback parameter to -0.1. 
    dX2 ~ ',sf,'*X2        # This fixes the self-feedback parameter to -0.1.
                      
')


#Specify the Univariate Latent Change Score model that will be fitted to simulated data
ULCS<-'

#Fixed Parameters
X2 ~ 1*X1       # Fixed regression of X2 on X1
X3 ~ 1*X2       # Fixed regression of X3 on X2

dX1 =~ 1*X2     # Fixed regression of dX1 on X2
dX2 =~ 1*X3     # Fixed regression of dX2 on X3

X2 ~ 0*1        # This line constrains the intercept of X2 to 0
X3 ~ 0*1        # This line constrains the intercept of X3 to 0

X2 ~~ 0*X2      # This fixes the variance of the X2 to 0  
X3 ~~ 0*X3      # This fixes the variance of the X3 to 0   

#Estimated Parameters
dX1 ~ 1             # This estimates the intercept of the change scores 
dX2 ~ 1             # This estimates the intercept of the change scores 
X1 ~  1             # This estimates the intercept of X1 
dX1 ~~  dX1         # This estimates the variance of the change scores 
dX2 ~~  dX2         # This estimates the variance of the change scores 
X1 ~~   X1          # This estimates the variance of X1 
dX1~X1              # This estimates the self-feedback parameter
dX2~X2              # This estimates the self-feedback parameter

'

recovery=data.frame(dX1_int = rep(NA,simulations),dX2_int=NA,X1_int=NA,dX1_var=NA,dX2_var=NA,X1_var=NA,sf1=NA,sf2=NA,sim=NA)
pb <- txtProgressBar(min = 0, max = simulations, style = 3)
for(j in 1:simulations){
  #Simulate data
  #set.seed(1234)
  simdatULCS<-simulateData(ULCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
  
  
  #Fit model
  fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
  
  #Store parameter estimates
  recovery$dX1_int[j]=parameterEstimates(fitULCS)$est[9]
  recovery$dX2_int[j]=parameterEstimates(fitULCS)$est[10]
  recovery$X1_int[j]=parameterEstimates(fitULCS)$est[11]
  recovery$dX1_var[j]=parameterEstimates(fitULCS)$est[12]
  recovery$dX2_var[j]=parameterEstimates(fitULCS)$est[13]
  recovery$X1_var[j]=parameterEstimates(fitULCS)$est[14]
  recovery$sf1[j]=parameterEstimates(fitULCS)$est[15]
  recovery$sf2[j]=parameterEstimates(fitULCS)$est[16]
  recovery$sim[j]=j
  setTxtProgressBar(pb, j)
}

#Plot observed levels of variables

level_data = simdatULCS %>% mutate(s = 1:samplesize) %>% gather(key=time,value=value,X1:X3)
ggplot(data=level_data) +
  geom_line(aes(x=time,y=value,group=s))


summary(fitULCS)

plot_data = recovery %>% gather(key=parameter,value=value,dX1_int:sf2)

ggplot(data=plot_data) +
  geom_line(aes(x=sim,y=value)) +
  facet_grid(.~parameter,scale="free")

semPaths(fitULCS, title = FALSE, curvePivot = TRUE)
