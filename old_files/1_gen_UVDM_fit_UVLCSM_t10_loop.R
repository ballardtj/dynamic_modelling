

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


#-------------------------------
# Specify dynamic model

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

#------------------------------------
#Specify latent change score model

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
#fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
#summary(fitULCS)


#-------------------------------------
#specify grid of parameters to test
generating_parms = expand.grid(dXint = c(0.1,1,10,100),
                               Xint = c(0.1,1,10,100),
                               dXvar = c(0.1,1,10,100),
                               Xvar = c(0.1,1,10,100),
                               sf = c(0.1))


#---------------------------------------
#run recovery analysis

recovery_list=list() #list object to store recovery objects
pb <- txtProgressBar(min = 0, max = simulations, style = 3)
for(i in 1:nrow(generating_parms)){
  
  print(i)
  
  recovery=data.frame(est_dXint = rep(NA,simulations),est_Xint=NA,est_dXvar=NA,
                      est_Xvar=NA,est_sf=NA,sim=NA,true_dXint=generating_parms$dXint[i],
                      true_Xint=generating_parms$Xint[i],true_dXVar=generating_parms$dXvar[i],
                      true_XVar=generating_parms$Xvar[i],true_sf=generating_parms$sf[i],parms=i )
  
  for(j in 1:simulations){
    
    
    simdatUDM=simulate_dynamic_model(generating_parms$Xint[i],
                                     generating_parms$Xvar[i],
                                     generating_parms$dXint[i],
                                     generating_parms$dXvar[i],
                                     generating_parms$sf[i],
                                     t=10,
                                     samplesize)
    
    #Fit model
    fitULCS <- lavaan(ULCS, data=simdatUDM, estimator='mlr',fixed.x=FALSE,missing='fiml')
    
    #Store parameter estimates
    recovery$est_dXint[j]=parameterEstimates(fitULCS)$est[39]
    recovery$est_Xint[j]=parameterEstimates(fitULCS)$est[37]
    recovery$est_dXvar[j]=parameterEstimates(fitULCS)$est[48]
    recovery$est_Xvar[j]=parameterEstimates(fitULCS)$est[38]
    recovery$est_sf[j]=parameterEstimates(fitULCS)$est[57]
    recovery$sim[j]=j
    setTxtProgressBar(pb, j)
  }
  
  recovery_list[[i]]=recovery
}  

recovery_all = bind_rows(recovery_list)
save(recovery_all,file="recovery_all.RData")

#plot_data = recovery %>% gather(key=parameter,value=value,dX_int:sf)

names(recovery_all)[names(recovery_all)=='true_dXVar']<-'true_dXvar'
names(recovery_all)[names(recovery_all)=='true_XVar']<-'true_Xvar'

# plot_data = recovery_all %>%
#   filter(sim==1) %>%
#   select(sim,parms,est_dXint,est_Xint ,est_dXvar,  est_Xvar,   est_sf, true_dXint, true_Xint,  true_dXvar, true_Xvar,  true_sf   ) %>%
#   gather(key=parameter,value=value,est_dXint:true_sf) %>%
#   extract(col="parameter",into=c("source","parameter"),regex="(.*)\\_(.*)") %>%
#   spread(key=parameter,value=value)

#Recovery of change intercept is good
ggplot(data=recovery_all) +
  geom_jitter(aes(x=factor(true_dXint),y=est_dXint,group=sim)) +
  facet_grid(true_Xint~interaction(factor(true_Xvar),factor(true_dXvar)))

#Recovery of initial value intercept is good
ggplot(data=recovery_all) +
  geom_jitter(aes(x=factor(true_Xint),y=est_Xint,group=sim)) +
  facet_grid(true_dXint~interaction(factor(true_Xvar),factor(true_dXvar)))

#Recovery of variance in change variable is good generally, but a bit worse when variance is higher
ggplot(data=recovery_all) +
  geom_jitter(aes(x=factor(true_dXvar),y=est_dXvar,group=sim)) +
  facet_grid(true_Xint~interaction(factor(true_Xvar),factor(true_dXint)))

#Recovery of variance in intecept is good generally, but a bit worse when variance is higher
ggplot(data=recovery_all) +
  geom_jitter(aes(x=factor(true_Xvar),y=est_Xvar,group=sim)) +
  facet_grid(true_Xint~interaction(factor(true_dXvar),factor(true_dXint))) +
  coord_cartesian(ylim=c(0,2))




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
