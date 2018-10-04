

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

#Create function to simulate dynamic model
simulate_dynamic_model  <- function(X1_int,X1_var,dX_int,dX_var,sf,t,samplesize){
  simulated_data = matrix(NA,nrow=samplesize,ncol=t)   
  
  #Initialise
  simulated_data[,1] <- rnorm(n=samplesize,mean=X1_int,sd=sqrt(X1_var))
 
  for(i in 2:t){
    change = dX_int + sf*simulated_data[,i-1] +  rnorm(n=samplesize,mean=0,sd=sqrt(dX_var))
    simulated_data[,i] = simulated_data[,i-1] + change
  } 
  return(as.data.frame(simulated_data))
}


simdatULCS=simulate_dynamic_model(X1_int,X1_var,dX1_int,dX1_var,sf,3,samplesize)



#Specify the Univariate Latent Change Score model that will be fitted to simulated data
ULCS<-'

#Fixed Parameters
V2 ~ 1*V1       # Fixed regression of X2 on X1
V3 ~ 1*V2       # Fixed regression of X3 on X2

dV1 =~ 1*V2     # Fixed regression of dX1 on X2
dV2 =~ 1*V3     # Fixed regression of dX2 on X3

V2 ~ 0*1        # This line constrains the intercept of X2 to 0
V3 ~ 0*1        # This line constrains the intercept of X3 to 0

V2 ~~ 0*V2      # This fixes the variance of the X2 to 0  
V3 ~~ 0*V3      # This fixes the variance of the X3 to 0   

#Estimated Parameters
dV1 ~ label("dX_int")*1             # This estimates the intercept of the change scores 
dV2 ~ label("dX_int")*1             # This estimates the intercept of the change scores 
V1 ~  1             # This estimates the intercept of X1 
dV1 ~~  label("dX_var")*dV1         # This estimates the variance of the change scores 
dV2 ~~  equal("dX_var")*dV2         # This estimates the variance of the change scores 
V1 ~~   V1          # This estimates the variance of X1 
dV1~label("sf")*V1              # This estimates the self-feedback parameter
dV2~equal("sf")*V2              # This estimates the self-feedback parameter

'
#Test
#fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
#summary(fitULCS)


recovery=data.frame(dX_int = rep(NA,simulations),X1_int=NA,dX_var=NA,X1_var=NA,sf=NA,sim=NA)
pb <- txtProgressBar(min = 0, max = simulations, style = 3)
for(j in 1:simulations){
  #Simulate data
  #set.seed(1234)
  #simdatULCS<-simulateData(ULCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
  simdatULCS=simulate_dynamic_model(X1_int,X1_var,dX1_int,dX1_var,sf,3,samplesize)
  
  #Fit model
  fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
  
  #Store parameter estimates
  recovery$dX_int[j]=parameterEstimates(fitULCS)$est[9]
  recovery$X1_int[j]=parameterEstimates(fitULCS)$est[11]
  recovery$dX_var[j]=parameterEstimates(fitULCS)$est[12]
  recovery$X1_var[j]=parameterEstimates(fitULCS)$est[14]
  recovery$sf[j]=parameterEstimates(fitULCS)$est[15]
  recovery$sim[j]=j
  setTxtProgressBar(pb, j)
}


plot_data = recovery %>% gather(key=parameter,value=value,dX_int:sf)

ggplot(data=plot_data) +
  geom_line(aes(x=sim,y=value)) +
  facet_grid(.~parameter,scale="free")

level_data = simdatULCS %>% mutate(s = 1:samplesize) %>% gather(key=time,value=value,V1:V3)
ggplot(data=level_data) +
  geom_line(aes(x=time,y=value,group=s))


#semPaths(fitULCS, title = FALSE, curvePivot = TRUE)
