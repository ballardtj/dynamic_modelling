#This script simulates, then fits, data for a multiple indicator univariate Latent Change Score model.
#This script is part of the manuscript 
#'Developmental cognitive neuroscience using Latent Change Score models: A tutorial and applications'
#Rogier A. Kievit, Andreas M. Brandmaier, Gabriel Ziegler, Anne-Laura van Harmelen, 
#Susanne de Mooij, Michael Moutoussisa, Ian Goodyer, Ed Bullmore, Peter Jones, 
#Peter Fonagy, NSPN Consortium, Ulman Lindenberger & Raymond J. Dolan                                                         


#This code was written by Rogier A. Kievit (rogier.kievit@mrc-cbu.cam.ac.uk), 30 January 2017.
#It may be used, (re)shared and modified freely under a CC-BY license 

# DISCLAIMER: SEM model choices and conventions vary - final responsibility for 
# model fitting and interpretation lies with the researcher 

# Uncomment the following lines to download and install the relevant packages

# install.packages('lavaan') 
# install.packages('ggplot2')
# install.packages('reshape2')

library(lavaan)
library(ggplot2)
library(reshape2)

#This script simulates, then saves, then fits, data for a multiple indicator univariate Latent Change Score model.

#####  Simulate data for a multiple indicator Univariate Latent Change Score model ####
#Fix sample size
samplesize<-500

#Simulate data for a Univariate Latent Change Score model. 
MILCS_simulate<-'

####    The following two lines specify the measurement model for multiple indicators (X1-X3) 
####    measured on two occasions (T1-T2)
COG_T1=~.8*T1X1+.9*T1X2+.7*T1X3   # This specifies the measurement model for COG_T1 
COG_T2=~.8*T2X1+.9*T2X2+.7*T2X3   # This specifies the measurement model for COG_T2 


#####     The following lines specify the core assumptions of the LCS 
#####     and should not generally be modified

COG_T2 ~ 1*COG_T1           # Fixed regression of COG_T2 on COG_T1
dCOG1 =~ 1*COG_T2           # Fixed regression of dCOG1 on COG_T2
COG_T2 ~ 0*1                # This line constrains the intercept of COG_T2 to 0
COG_T2 ~~ 0*COG_T2          # This fixes the variance of the COG_T2 to 0  


T1X1~0*1                  # This fixes the intercept of X1 to 0
T1X2~1*1                  # This fixes the intercept of X2 to 1
T1X3~.5*1                 # This fixes the intercept of X3 to 0.5 
T2X1~0*1                  # This fixes the intercept of X1 to 0
T2X2~1*1                  # This fixes the intercept of X2 to 1
T2X3~.5*1                 # This fixes the intercept of X3 to 0.5


###### The following five parameters will be estimated in the model. 
###### Values can be modified manually to examine the effect on the model

dCOG1 ~ 10*1            # This fixes the intercept of the change score to 10 
COG_T1 ~ 50*1           # This fixes the intercept of COG_T1 to 50. 
dCOG1 ~~ 5*dCOG1        # This fixes the variance of the change scores to 5. 
COG_T1 ~~ 8*COG_T1      # This fixes the variance of the COG_T1 to 8. 
dCOG1~-0.1*COG_T1       # This fixes the self-feedback parameter to -0.1. 


'

#Simulate data
set.seed(1234)
simdatMILCS<-simulateData(MILCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
colMeans(simdatMILCS) #sanity check the means
write.csv(simdatMILCS,'2_simdatMILCS.csv')

#Fit the multiple indicator Univariate Latent Change Score model to simulated data
MILCS<-'
COG_T1=~1*T1X1+T1X2+T1X3                           # This specifies the measurement model for COG_T1 
COG_T2=~1*T2X1+equal("COG_T1=~T1X2")*T2X2+equal("COG_T1=~T1X3")*T2X3   # This specifies the measurement model for COG_T2 with the equality constrained factor loadings

COG_T2 ~ 1*COG_T1     # Fixed regression of COG_T2 on COG_T1
dCOG1 =~ 1*COG_T2     # Fixed regression of dCOG1 on COG_T2
COG_T2 ~ 0*1          # This line constrains the intercept of COG_T2 to 0
COG_T2 ~~ 0*COG_T2    # This fixes the variance of the COG_T2 to 0 

dCOG1 ~ 1             # This estimates the intercept of the change score 
COG_T1 ~  1           # This estimates the intercept of COG_T1 
dCOG1 ~~  dCOG1       # This estimates the variance of the change scores 
COG_T1 ~~   COG_T1    # This estimates the variance of the COG_T1 
dCOG1~COG_T1          # This estimates the self-feedback parameter


T1X1~~T2X1   # This allows residual covariance on indicator X1 across T1 and T2
T1X2~~T2X2   # This allows residual covariance on indicator X2 across T1 and T2
T1X3~~T2X3   # This allows residual covariance on indicator X3 across T1 and T2

T1X1~~T1X1   # This allows residual variance on indicator X1 
T1X2~~T1X2   # This allows residual variance on indicator X2
T1X3~~T1X3   # This allows residual variance on indicator X3

T2X1~~equal("T1X1~~T1X1")*T2X1  # This allows residual variance on indicator X1 at T2 
T2X2~~equal("T1X2~~T1X2")*T2X2  # This allows residual variance on indicator X2 at T2 
T2X3~~equal("T1X3~~T1X3")*T2X3  # This allows residual variance on indicator X3 at T2

T1X1~0*1                 # This constrains the intercept of X1 to 0 at T1
T1X2~1                   # This estimates the intercept of X2 at T1
T1X3~1                   # This estimates the intercept of X3 at T1
T2X1~0*1                 # This constrains the intercept of X1 to 0 at T2
T2X2~equal("T1X2~1")*1   # This estimates the intercept of X2 at T2
T2X3~equal("T1X3~1")*1   # This estimates the intercept of X3 at T2


'

fitMILCS <- lavaan(MILCS, data=simdatMILCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitMILCS, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Visualize the raw data if so inclined
theme_set(theme_grey(base_size = 18)) #increase text size
id=factor(1:samplesize)
plotdattemp=data.frame(c(simdatMILCS$T1X1,simdatMILCS$T1X2,simdatMILCS$T1X3),
                       c(simdatMILCS$T2X1,simdatMILCS$T2X2,simdatMILCS$T2X3),as.factor(c(id,id,id)),
                       c(rep('X1',times=samplesize),rep('X2',times=samplesize),rep('X3',times=samplesize)))
colnames(plotdattemp)<-c('COG_T1', 'COG_T2','id','Indicator')
plotdat<-melt(plotdattemp,by='id')
ggplot(plotdat,aes(variable,value,group=id,col=Indicator))+geom_point(size=3,alpha=.7)+geom_line(alpha=.7)+ggtitle('Multiple indicator Latent Change Score model')+ylab('Cognitive scores')+xlab('Time points')+facet_grid(~Indicator)





