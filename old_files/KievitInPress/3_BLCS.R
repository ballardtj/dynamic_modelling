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

#Uncomment the following lines to download and install the relevant packages

# install.packages('lavaan') 
# install.packages('ggplot2')
# install.packages('reshape2')
library(lavaan)
library(ggplot2)
library(reshape2)

#This script simulates, then saves, then fits, data for a bivariate Latent Change Score model.

#####  Simulate data for a bivariate Latent Change Score model ####


#Fix sample size
samplesize<-500

#Simulate data for a bivariate Latent Change Score model. 
BLCS_simulate<-'

##### The following four parameters capture the core assumptions of the LCS and should not generally be modified
COG_T2 ~ 1*COG_T1     # This parameter regresses COG_T2 perfectly on COG_T1
dCOG1 =~ 1*COG_T2     # This defines the latent change score factor as measured perfectly by scores on COG_T2
COG_T2 ~ 0*1          # This line constrains the intercept of COG_T2 to 0
COG_T2 ~~ 0*COG_T2    # This fixes the variance of the COG_T2 to 0  

###### The following five parameters will be estimated in the model. Their values can be modified manually to examine the effect on the raw data and model fit

dCOG1 ~ 10*1         # This fixes the intercept of the change score to 10 (i.e. people gain 10 points on average).
COG_T1 ~ 50*1        # This fixes the intercept of COG_T1 to 50. 
dCOG1 ~~ 5*dCOG1     # This fixes the variance of the change scores to 5. This parameter can be changed manually to examine the effects.
COG_T1 ~~ 8*COG_T1   # This fixes the variance of the COG_T1 to 8. This parameter can be changed manually to examine the effects.
dCOG1~-0.1*COG_T1    # This fixes the self-feedback parameter to -0.2. This parameter can be changed manually to examine the effects.

############
NEU_T2 ~ 1*NEU_T1    # This parameter regresses NEU_T2 perfectly on NEU_T1
dNEU1 =~ 1*NEU_T2    # This defines the latent change score factor as measured perfectly by scores on NEU_T2
NEU_T2 ~ 0*1         # This line constrains the intercept of NEU_T2 to 0
NEU_T2 ~~ 0*NEU_T2   # This fixes the variance of the NEU_T1 to 0  

###### The following five parameters will be estimated in the model. Their values can be modified manually to examine the effect on the raw data and model fit

dNEU1 ~ 10*1         # This fixes the intercept of the change score to 10 (i.e. people gain 10 points on average).
NEU_T1 ~ 50*1        # This fixes the intercept of NEU_T1 to 50. 
dNEU1 ~~ 5*dNEU1     # This fixes the variance of the change scores to 5. This parameter can be changed manually to examine the effects.
NEU_T1 ~~ 8*NEU_T1   # This fixes the variance of the NEU_T1 to 8. This parameter can be changed manually to examine the effects.
dNEU1~-0.1*NEU_T1    # This fixes the self-feedback parameter to -0.1. This parameter can be changed manually to examine the effects.

dNEU1~0.4*COG_T1     # This fixes the coupling (.2) parameter for dNEU. These parameter can be changed manually to examine the effects.
dCOG1~0.3*NEU_T1     # This fixes the coupling (.3) parameter for dCOG. These parameter can be changed manually to examine the effects.

NEU_T1~~5*COG_T1     # This fixes the covariance in scores at T1 to 5
dCOG1~~.2*dNEU1      # This fixes the covariance in change scores to .2
'

#Simulate data
set.seed(1234)
simdatBLCS<-simulateData(BLCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
colMeans(simdatBLCS) #sanity check the means
write.csv(simdatBLCS,'3_simdatBLCS.csv')


#Fit the Bivariate Latent Change Score model to simulated data
BLCS<-'

COG_T2 ~ 1*COG_T1     # This parameter regresses COG_T2 perfectly on COG_T1
dCOG1 =~ 1*COG_T2     # This defines the latent change score factor as measured perfectly by scores on COG_T2
dCOG1 ~ 1             # This estimates the intercept of the change score 
COG_T1 ~  1           # This estimates the intercept of COG_T1 
COG_T2 ~ 0*1          # This constrains the intercept of COG_T2 to 0

NEU_T2 ~ 1*NEU_T1     # This parameter regresses NEU_T2 perfectly on NEU_T1
dNEU1 =~ 1*NEU_T2     # This defines the latent change score factor as measured perfectly by scores on NEU_T2
NEU_T2 ~ 0*1          # This line constrains the intercept of NEU_T2 to 0
NEU_T2 ~~ 0*NEU_T2    # This fixes the variance of the NEU_T1 to 0  

dCOG1 ~~  dCOG1       # This estimates the variance of the change scores
COG_T1 ~~   COG_T1    # This estimates the variance of the COG_T1 
COG_T2 ~~ 0*COG_T2    # This fixes the variance of the COG_T2 to 0  

dNEU1 ~ 1             # This estimates the intercept of the change score 
NEU_T1 ~ 1            # This estimates the intercept of NEU_T1 
dNEU1 ~~ dNEU1        # This estimates the variance of the change scores 
NEU_T1 ~~ NEU_T1      # This estimates the variance of NEU_T1 

dNEU1~COG_T1+NEU_T1   # This estimates the COG to NEU coupling parameter and the COG to COG self-feedback
dCOG1~NEU_T1+COG_T1   # This estimates the NEU to COG coupling parameter and the NEU to NEU self-feedback

COG_T1 ~~  NEU_T1     # This estimates the COG_T1 NEU_T1 covariance
dCOG1~~dNEU1          # This estimates the dCOG and dNEU covariance
'

fitBLCS <- lavaan(BLCS, data=simdatBLCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitBLCS, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Visualize the raw data

theme_set(theme_grey(base_size = 18)) #increase text size
id=factor(1:samplesize)
plotdattemp=data.frame(c(simdatBLCS$COG_T1,simdatBLCS$NEU_T1),c(simdatBLCS$COG_T2,simdatBLCS$NEU_T2),as.factor(c(id,id)),c(rep('COG',times=samplesize),rep('NEU',times=samplesize)))
colnames(plotdattemp)<-c('T1', 'T2','id','Domain')
plotdat<-melt(plotdattemp,by='id')
ggplot(plotdat,aes(variable,value,group=id,col=Domain))+geom_point(size=3,alpha=.7)+geom_line(alpha=.7)+ggtitle('Bivariate Latent Change Score model')+ylab('Scores')+xlab('Time points')+facet_grid(~Domain)

