

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
samplesize<-60
simulations<-100

#generating parameters
dX1_int = 0.3 #intercept of change score at t1
dX1_var = 1  #variance of change score at t1
X1_int = 2  #intecept of X1
X1_var = 1 #variance of X1
sf = .3 #self-feedback parameter

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


simdatULCS=simulate_dynamic_model(X1_int,X1_var,dX1_int,dX1_var,sf,t=10,samplesize)



#Specify the Univariate Latent Change Score model that will be fitted to simulated data
ULCS<-'

#Fixed Parameters
V2 ~ 1*V1       # Fixed regression of X2 on X1
V3 ~ 1*V2       # Fixed regression of X3 on X2
V4 ~ 1*V3       # Fixed regression 
V5 ~ 1*V4       # Fixed regression 
V6 ~ 1*V5       # Fixed regression
V7 ~ 1*V6       # Fixed regression 
V8 ~ 1*V7       # Fixed regression 
V9 ~ 1*V8       # Fixed regression 
V10 ~ 1*V9       # Fixed regression 

dV1 =~ 1*V2     # Fixed regression of dX1 on X2
dV2 =~ 1*V3     # Fixed regression of dX2 on X3
dV3 =~ 1*V4     # Fixed regression 
dV4 =~ 1*V5     # Fixed regression 
dV5 =~ 1*V6     # Fixed regression 
dV6 =~ 1*V7     # Fixed regression 
dV7 =~ 1*V8     # Fixed regression 
dV8 =~ 1*V9     # Fixed regression 
dV9 =~ 1*V10     # Fixed regression 

V2 ~ 0*1        # This line constrains the intercept of X2 to 0
V3 ~ 0*1        # This line constrains the intercept of X3 to 0
V4 ~ 0*1        # This line constrains the intercept 
V5 ~ 0*1        # This line constrains the intercept 
V6 ~ 0*1        # This line constrains the intercept 
V7 ~ 0*1        # This line constrains the intercept 
V8 ~ 0*1        # This line constrains the intercept 
V9 ~ 0*1        # This line constrains the intercept 
V10 ~ 0*1        # This line constrains the intercept 

V2 ~~ 0*V2      # This fixes the variance of the X2 to 0  
V3 ~~ 0*V3      # This fixes the variance of the X3 to 0 
V4 ~~ 0*V4      # This fixes the variance
V5 ~~ 0*V5      # This fixes the variance
V6 ~~ 0*V6      # This fixes the variance
V7 ~~ 0*V7      # This fixes the variance
V8 ~~ 0*V8      # This fixes the variance
V9 ~~ 0*V9      # This fixes the variance
V10 ~~ 0*V10      # This fixes the variance


#Estimated Parameters
V1 ~  1             # This estimates the intercept of X1 
V1 ~~   V1          # This estimates the variance of X1 

dV1 ~ label("dX_int")*1             # This estimates the intercept of the change scores 
dV2 ~ equal("dX_int")*1             # This estimates the intercept of the change scores 
dV3 ~ equal("dX_int")*1             # This estimates the intercept of the change scores 
dV4 ~ equal("dX_int")*1             # This estimates the intercept of the change scores 
dV5 ~ equal("dX_int")*1             # This estimates the intercept of the change scores 
dV6 ~ equal("dX_int")*1             # This estimates the intercept of the change scores 
dV7 ~ equal("dX_int")*1             # This estimates the intercept of the change scores 
dV8 ~ equal("dX_int")*1             # This estimates the intercept of the change scores 
dV9 ~ equal("dX_int")*1             # This estimates the intercept of the change scores 


dV1 ~~  label("dX_var")*dV1         # This estimates the variance of the change scores 
dV2 ~~  equal("dX_var")*dV2         # This estimates the variance of the change scores 
dV3 ~~  equal("dX_var")*dV3         # This estimates the variance of the change scores 
dV4 ~~  equal("dX_var")*dV4         # This estimates the variance of the change scores 
dV5 ~~  equal("dX_var")*dV5         # This estimates the variance of the change scores 
dV6 ~~  equal("dX_var")*dV6         # This estimates the variance of the change scores 
dV7 ~~  equal("dX_var")*dV7         # This estimates the variance of the change scores 
dV8 ~~  equal("dX_var")*dV8         # This estimates the variance of the change scores 
dV9 ~~  equal("dX_var")*dV9         # This estimates the variance of the change scores 


dV1~label("sf")*V1              # This estimates the self-feedback parameter
dV2~equal("sf")*V2              # This estimates the self-feedback parameter
dV3~equal("sf")*V3              # This estimates the self-feedback parameter
dV4~equal("sf")*V4              # This estimates the self-feedback parameter
dV5~equal("sf")*V5              # This estimates the self-feedback parameter
dV6~equal("sf")*V6              # This estimates the self-feedback parameter
dV7~equal("sf")*V7              # This estimates the self-feedback parameter
dV8~equal("sf")*V8              # This estimates the self-feedback parameter
dV9~equal("sf")*V9              # This estimates the self-feedback parameter

'
#Test
fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
#summary(fitULCS)


recovery=data.frame(dX_int = rep(NA,simulations),X1_int=NA,dX_var=NA,X1_var=NA,sf=NA,sim=NA)
pb <- txtProgressBar(min = 0, max = simulations, style = 3)
for(j in 1:simulations){
  #Simulate data
  #set.seed(1234)
  #simdatULCS<-simulateData(ULCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
  simdatULCS=simulate_dynamic_model(X1_int,X1_var,dX1_int,dX1_var,sf,t=10,samplesize)
  
  #Fit model
  fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
  
  #Store parameter estimates
  recovery$dX_int[j]=parameterEstimates(fitULCS)$est[39]
  recovery$X1_int[j]=parameterEstimates(fitULCS)$est[37]
  recovery$dX_var[j]=parameterEstimates(fitULCS)$est[48]
  recovery$X1_var[j]=parameterEstimates(fitULCS)$est[38]
  recovery$sf[j]=parameterEstimates(fitULCS)$est[57]
  recovery$sim[j]=j
  setTxtProgressBar(pb, j)
}


plot_data = recovery %>% gather(key=parameter,value=value,dX_int:sf)

ggplot(data=plot_data) +
  geom_line(aes(x=sim,y=value)) +
  facet_grid(.~parameter,scale="free") +
  coord_cartesian(ylim=c(-10:10))

level_data = simdatULCS %>% 
  mutate(s = 1:samplesize) %>% 
  gather(key=time,value=value,V1:V10) %>%
  mutate(time=factor(time,levels=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"),labels=c("1","2","3","4","5","6","7","8","9","10")  ))

ggplot(data=level_data) +
  geom_line(aes(x=time,y=value,group=s))


#semPaths(fitULCS, title = FALSE, curvePivot = TRUE)
