#This script simulates, then fits, data for a univariate Latent Change Score model.
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

#This script simulates, then saves, then fits, data for a univariate Latent Change Score model.

#####  Simulate data for a Univariate Latent Change Score model ####

#Fix sample size
samplesize<-10

#Simulation specification
ULCS_simulate<-'

#####     The following lines specify the core assumptions of the LCS 
#####     and should not generally be modified

COG_T2 ~ 1*COG_T1     # Fixed regression of COG_T2 on COG_T1
dCOG1 =~ 1*COG_T2     # Fixed regression of dCOG1 on COG_T2
COG_T2 ~ 0*1          # This line constrains the intercept of COG_T2 to 0
COG_T2 ~~ 0*COG_T2    # This fixes the variance of the COG_T2 to 0  


###### The following five parameters will be estimated in the model. 
###### Values can be modified manually to examine the effect on the model

dCOG1 ~ 10*1        # This fixes the intercept of the change score to 10 
COG_T1 ~ 50*1       # This fixes the intercept of COG_T1 to 50 

dCOG1 ~~ 5*dCOG1    # This fixes the variance of the change scores to 5. 
COG_T1 ~~ 8*COG_T1  # This fixes the variance of the COG_T1 to 8. 

dCOG1~-0.1*COG_T1   # This fixes the self-feedback parameter to -0.1. 

'

#Simulate data
set.seed(1234)
simdatULCS<-simulateData(ULCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
colMeans(simdatULCS) #sanity check the means

write.csv(simdatULCS,'1_simdatULCS.csv') #save data to be used by other programs

#Fit the Univariate Latent Change Score model to simulated data
ULCS<-'

COG_T2 ~ 1*COG_T1     # Fixed regression of COG_T2 on COG_T1
dCOG1 =~ 1*COG_T2     # Fixed regression of dCOG1 on COG_T2
COG_T2 ~ 0*1          # This line constrains the intercept of COG_T2 to 0
COG_T2 ~~ 0*COG_T2    # This fixes the variance of the COG_T2 to 0 

dCOG1 ~ 1             # This estimates the intercept of the change scores 
COG_T1 ~  1           # This estimates the intercept of COG_T1 
dCOG1 ~~  dCOG1       # This estimates the variance of the change scores 
COG_T1 ~~   COG_T1    # This estimates the variance of COG_T1 
dCOG1~COG_T1          # This estimates the self-feedback parameter

'

fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitULCS, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#Visualize the raw data if so inclined
theme_set(theme_grey(base_size = 18)) #increase text size
id=factor(1:samplesize) #create ids
plotdattemp=data.frame(simdatULCS$COG_T1,simdatULCS$COG_T2,id) #create dataframe with raw scores
colnames(plotdattemp)<-c('COG_T1', 'COG_T2','id') #specify names
plotdat<-melt(plotdattemp,by='id') #melt to long form
ggplot(plotdat,aes(variable,value,group=id))+geom_point(col='dodgerblue',size=3,alpha=.7)+geom_line(col='dodgerblue',alpha=.7)+ggtitle('Univariate Latent Change Score model')+ylab('COG scores')+xlab('Time points')











