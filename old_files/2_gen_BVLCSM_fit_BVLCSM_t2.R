
rm(list=ls())
##############################
#### Bivariate LCS Model ####
##############################

#Load packages
library(lavaan)
library(tidyverse)
library(reshape2)
library(semPlot)

#----------------------------------------------------------
#Step 1 simulate LCS model and recover 

#Fix sample size
samplesize<-100
simulations<-100

#generating parameters
dX1int = 1 #intercept of change score
X1int = 2  #intecept of X1
dX1var = 1 #variance of change score
X1var = 5  #variance of X1
sfX = -0.5  #self-feedback parameter for variable X

dY1int = 1  #intercept of change score
Y1int = 2   #intecept of X1
dY1var = 1  #variance of change score
Y1var = 5   #variance of X1
sfY = -0.5   #self-feedback parameter for variable X

dXonY = 1    #Effect of variable Y on change in variable X (coupling parameter)
dYonX = -1   #Effect of variable X on change in variable Y (coupling parameter)
XYcov = 0.4    #Covariance between X and Y  
dXdYcov = -0.2 #Covariance between changes in X and Y

#Simulation specification
BLCS_simulate<-paste0('

  #####     The following lines specify the core assumptions of the LCS 
  #####     and should not generally be modified
                        
  X2 ~ 1*X1     # This parameter regresses X2 perfectly on X1
  dX1 =~ 1*X2     # This defines the latent change score factor as measured perfectly by scores on X2
  X2 ~ 0*1          # This line constrains the intercept of X2 to 0
  X2 ~~ 0*X2    # This fixes the variance of the X2 intercept to 0  

  ###### The following five parameters will be estimated in the model. 
  ###### Values can be modified manually to examine the effect on the model
                        
  dX1 ~ ',dX1int,'*1          # This fixes the intercept of the change score to dX1_int. 
  X1 ~ ',X1int,'*1           # This fixes the intercept of X1 to X1_int. 
                        
  dX1 ~~ ',dX1var,'*dX1        # This fixes the variance of the change scores to dX1_var. 
  X1 ~~ ',X1var,'*X1          # This fixes the variance of the X1 intercept to X1_var. 
                        
  dX1 ~ ',sfX,'*X1        # This fixes the self-feedback parameter to sfX.

  ############ The following lines specify the core assumptions of the LCS 

  Y2 ~ 1*Y1    # This parameter regresses Y2 perfectly on Y1
  dY1 =~ 1*Y2    # This defines the latent change score factor as measured perfectly by scores on Y2
  Y2 ~ 0*1         # This line constrains the intercept of Y2 to 0
  Y2 ~~ 0*Y2   # This fixes the variance of the Y2 intercept to 0  

  ###### The following five parameters will be estimated in the model. Their values can be modified manually to examine the effect on the raw data and model fit

  dY1 ~ ',dY1int,'*1          # This fixes the intercept of the change score to dY1_int. 
  Y1 ~ ',Y1int,'*1           # This fixes the intercept of Y1 to Y1_int. 
                      
  dY1 ~~ ',dY1var,'*dY1        # This fixes the variance of the change scores to dY1_var. 
  Y1 ~~ ',Y1var,'*Y1          # This fixes the variance of the Y1 intercept to Y1_var. 
                      
  dY1 ~ ',sfY,'*Y1        # This fixes the self-feedback parameter to sfY.

  ###### The following four parameters are additionally estimated in the bivariate model.

  dY1~',dYonX,'*X1     # This fixes the coupling parameter for dY. 
  dX1~',dXonY,'*Y1     # This fixes the coupling parameter for dX.
                      
  Y1~~',XYcov,'*X1     # This fixes the covariance in scores at T1 to XYcov.
  dX1~~',dXdYcov,'*dY1  # This fixes the covariance in change scores to dXdYcov.
                        
')

#Specify the Bivariate Latent Change Score model that will be fitted to simulated data

BLCS<-'

X2 ~ 1*X1     # This parameter regresses X2 perfectly on X1
dX1 =~ 1*X2     # This defines the latent change score factor as measured perfectly by scores on X2
dX1 ~ 1             # This estimates the intercept of the change score 
X1 ~  1           # This estimates the intercept of X1 
X2 ~ 0*1          # This constrains the intercept of X2 to 0

Y2 ~ 1*Y1     # This parameter regresses Y2 perfectly on Y1
dY1 =~ 1*Y2     # This defines the latent change score factor as measured perfectly by scores on Y2
Y2 ~ 0*1          # This line constrains the intercept of Y2 to 0
Y2 ~~ 0*Y2    # This fixes the variance of the Y1 to 0  

dX1 ~~  dX1       # This estimates the variance of the change scores
X1 ~~   X1    # This estimates the variance of the X1 
X2 ~~ 0*X2    # This fixes the variance of the X2 to 0  

dY1 ~ 1             # This estimates the intercept of the change score 
Y1 ~ 1            # This estimates the intercept of Y1 
dY1 ~~ dY1        # This estimates the variance of the change scores 
Y1 ~~ Y1      # This estimates the variance of Y1 

dY1~X1+Y1   # This estimates the X to Y coupling parameter and the X to X self-feedback
dX1~Y1+X1   # This estimates the Y to X coupling parameter and the Y to Y self-feedback

X1 ~~  Y1     # This estimates the X1 Y1 covariance
dX1~~dY1          # This estimates the dX and dY covariance
'

recovery=data.frame(sim=1:simulations,
                    dX1int = NA, #intercept of change score
                    X1int = NA,  #intecept of X1
                    dX1var = NA, #variance of change score
                    X1var = NA,  #variance of X1
                    sfX = NA, #self-feedback parameter for variable X
                    dY1int = NA,  #intercept of change score
                    Y1int = NA,   #intecept of X1
                    dY1var = NA,  #variance of change score
                    Y1var = NA,   #variance of X1
                    sfY = NA,   #self-feedback parameter for variable X
                    dXonY = NA,    #Effect of variable Y on change in variable X (coupling parameter)
                    dYonX = NA,   #Effect of variable X on change in variable Y (coupling parameter)
                    XYcov = NA,    #Covariance between X and Y  
                    dXdYcov = NA) #Covariance between changes in X and Y
                    
  
pb <- txtProgressBar(min = 0, max = simulations, style = 3)
for(j in 1:simulations){
  #Simulate data
  #set.seed(1234)
  simdatBLCS<-simulateData(BLCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
  
  
  #Fit model
  fitBLCS <- lavaan(BLCS, data=simdatBLCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
  
  #Store parameter estimates
  recovery$dX1int[j]=parameterEstimates(fitBLCS)$est[3]
  recovery$X1int[j]=parameterEstimates(fitBLCS)$est[4]
  recovery$dX1var[j]=parameterEstimates(fitBLCS)$est[10]
  recovery$X1var[j]=parameterEstimates(fitBLCS)$est[11]
  recovery$sfX[j]=parameterEstimates(fitBLCS)$est[20]
  recovery$dY1int[j]=parameterEstimates(fitBLCS)$est[13]
  recovery$Y1int[j]=parameterEstimates(fitBLCS)$est[14]
  recovery$dY1var[j]=parameterEstimates(fitBLCS)$est[15]
  recovery$Y1var[j]=parameterEstimates(fitBLCS)$est[16]
  recovery$sfY[j]=parameterEstimates(fitBLCS)$est[18]
  recovery$dXonY[j]=parameterEstimates(fitBLCS)$est[19]
  recovery$dYonX[j]=parameterEstimates(fitBLCS)$est[17]
  recovery$XYcov[j]=parameterEstimates(fitBLCS)$est[21]
  recovery$dXdYcov[j]=parameterEstimates(fitBLCS)$est[22]
  recovery$sim[j]=j
  setTxtProgressBar(pb, j)
}


plot_data = recovery %>% gather(key=parameter,value=value,dX1int:dXdYcov)

ggplot(data=plot_data) +
  geom_line(aes(x=sim,y=value)) +
  facet_grid(.~parameter,scale="free")

semPaths(fitULCS, title = FALSE, curvePivot = TRUE)
