#This script simulates, then fits, data for a Bivariate Dual Change Score model.
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


#Fix sample size
samplesize<-500

#####  Simulate data for a bivariate Latent Change Score model ####


BDCS_simulate<-'

COGlv_T1=~1*COG_T1        # Defining the COG latent variables
COGlv_T2=~1*COG_T2        # Defining the COG latent variables
COGlv_T3=~1*COG_T3        # Defining the COG latent variables
COGlv_T4=~1*COG_T4        # Defining the COG latent variables

NEUlv_T1=~1*NEU_T1        # Defining the NEU latent variables
NEUlv_T2=~1*NEU_T2        # Defining the NEU latent variables
NEUlv_T3=~1*NEU_T3        # Defining the NEU latent variables
NEUlv_T4=~1*NEU_T4        # Defining the NEU latent variables

##### The following parameters capture the core assumptions of the LCS and should not generally be modified

COGlv_T2 ~ 1*COGlv_T1     # This parameter regresses COG_T2 perfectly on COG_T1
COGlv_T3 ~ 1*COGlv_T2     # This parameter regresses COG_T3 perfectly on COG_T2
COGlv_T4 ~ 1*COGlv_T3     # This parameter regresses COG_T4 perfectly on COG_T3

NEUlv_T2 ~ 1*NEUlv_T1     # This parameter regresses NEU_T2 perfectly on NEU_T1
NEUlv_T3 ~ 1*NEUlv_T2     # This parameter regresses NEU_T3 perfectly on NEU_T2
NEUlv_T4 ~ 1*NEUlv_T3     # This parameter regresses NEU_T4 perfectly on NEU_T3

dCOG1 =~ 1*COGlv_T2       # This defines the change score as measured perfectly by scores on COG_T2
dCOG2 =~ 1*COGlv_T3       # This defines the change score as measured perfectly by scores on COG_T3
dCOG3 =~ 1*COGlv_T4       # This defines the change score as measured perfectly by scores on COG_T4

dNEU1 =~ 1*NEUlv_T2       # This defines the change score as measured perfectly by scores on NEU_T2
dNEU2 =~ 1*NEUlv_T3       # This defines the change score as measured perfectly by scores on NEU_T3
dNEU3 =~ 1*NEUlv_T4       # This defines the change score as measured perfectly by scores on NEU_T4

COG_T1~~5*COG_T1          # This specifies the COG residual variances 
COG_T2~~5*COG_T2          # This specifies the COG residual variances 
COG_T3~~5*COG_T3          # This specifies the COG residual variances 
COG_T4~~5*COG_T4          # This specifies the COG residual variances 

NEU_T1~~5*NEU_T1          # This specifies the NEU residual variances 
NEU_T2~~5*NEU_T2          # This specifies the NEU residual variances 
NEU_T3~~5*NEU_T3          # This specifies the NEU residual variances 
NEU_T4~~5*NEU_T4          # This specifies the NEU residual variances 

#Dynamics

dNEU1~0.05*NEUlv_T1       # This specifies the NEU self-feedback parameter (equality constrained across timepoints)
dNEU2~0.05*NEUlv_T2       # This specifies the NEU self-feedback parameter (equality constrained across timepoints) 
dNEU3~0.05*NEUlv_T3       # This specifies the NEU self-feedback parameter (equality constrained across timepoints)

dCOG1~0.05*COGlv_T1       # This specifies the COG self-feedback parameter (equality constrained across timepoints)
dCOG2~0.05*COGlv_T2       # This specifies the COG self-feedback parameter (equality constrained across timepoints) 
dCOG3~0.05*COGlv_T3       # This specifies the COG self-feedback parameter (equality constrained across timepoints)


dNEU1~.3*COGlv_T1         # This specifies the COG to NEU coupling parameter 
dNEU2~.3*COGlv_T2         # This specifies the COG to NEU coupling parameter 
dNEU3~.3*COGlv_T3         # This specifies the COG to NEU coupling parameter 

dCOG1~0.4*NEUlv_T1        # This specifies the NEU to COG coupling parameter 
dCOG2~0.4*NEUlv_T2        # This specifies the NEU to COG coupling parameter 
dCOG3~0.4*NEUlv_T3        # This specifies the NEU to COG coupling parameter 


iCOG=~1*COGlv_T1                   # This defines the COG intercept measurement model
sCOG=~1*dCOG1+1*dCOG2+1*dCOG3      # This defines the COG slope measurement model
iCOG~2*1                           # This specifies the COG intercept intercept (mean)
iCOG~~2*iCOG                       # This specifies the COG intercept variance
sCOG~2*1                           # This specifies the COG slope intercept
sCOG~~3*sCOG                       # This specifies the COG slope variance

iNEU=~1*NEUlv_T1                   # This defines the NEU slope measurement model
sNEU=~1*dNEU1+1*dNEU2+1*dNEU3      # This defines the NEU slope measurement model
iNEU~2*1                           # This specifies the NEU intercept intercept (mean)
iNEU~~2*iNEU                       # This specifies the NEU intercept variance
sNEU~2*1                           # This specifies the NEU slope intercept
sNEU~~3*sNEU                       # This specifies the NEU slope variance

iNEU~~.8*sNEU                      # This specifies the iNEU sNEU covariance
iNEU~~.8*sCOG                      # This specifies the iNEU sCOG covariance
iNEU~~.8*iCOG                      # This specifies the iNEU iCOG covariance
iCOG~~.8*sCOG                      # This specifies the iCOG sCOG covariance        
iCOG~~.8*sNEU                      # This specifies the iCOG sNEU covariance
sCOG~~.8*sNEU                      # This specifies the sCOG sNEU covariance  

'

set.seed(1234)
simdatBDCS<-simulateData(BDCS_simulate,sample.nobs = samplesize,meanstructure = T) #Simulate data
colMeans(simdatBDCS) 

write.csv(simdatBDCS,'4_simdatBDCS.csv')

BDCS<-'

COGlv_T1=~1*COG_T1        # Defining the COG latent variables
COGlv_T2=~1*COG_T2        # Defining the COG latent variables
COGlv_T3=~1*COG_T3        # Defining the COG latent variables
COGlv_T4=~1*COG_T4        # Defining the COG latent variables

NEUlv_T1=~1*NEU_T1        # Defining the NEU latent variables
NEUlv_T2=~1*NEU_T2        # Defining the NEU latent variables
NEUlv_T3=~1*NEU_T3        # Defining the NEU latent variables
NEUlv_T4=~1*NEU_T4        # Defining the NEU latent variables

##### The following parameters capture the core assumptions of the LCS and should not generally be modified

COGlv_T2 ~ 1*COGlv_T1     # This parameter regresses COG_T2 perfectly on COG_T1
COGlv_T3 ~ 1*COGlv_T2     # This parameter regresses COG_T3 perfectly on COG_T2
COGlv_T4 ~ 1*COGlv_T3     # This parameter regresses COG_T4 perfectly on COG_T3

NEUlv_T2 ~ 1*NEUlv_T1     # This parameter regresses NEU_T2 perfectly on NEU_T1
NEUlv_T3 ~ 1*NEUlv_T2     # This parameter regresses NEU_T3 perfectly on NEU_T2
NEUlv_T4 ~ 1*NEUlv_T3     # This parameter regresses NEU_T4 perfectly on NEU_T3

dCOG1 =~ 1*COGlv_T2       # This defines the change score as measured perfectly by scores on COG_T2
dCOG2 =~ 1*COGlv_T3       # This defines the change score as measured perfectly by scores on COG_T3
dCOG3 =~ 1*COGlv_T4       # This defines the change score as measured perfectly by scores on COG_T4

dNEU1 =~ 1*NEUlv_T2       # This defines the change score as measured perfectly by scores on NEU_T2
dNEU2 =~ 1*NEUlv_T3       # This defines the change score as measured perfectly by scores on NEU_T3
dNEU3 =~ 1*NEUlv_T4       # This defines the change score as measured perfectly by scores on NEU_T4

COG_T1~~resCOGvar*COG_T1          # This estimates the COG residual variances 
COG_T2~~resCOGvar*COG_T2          # This estimates the COG residual variances 
COG_T3~~resCOGvar*COG_T3          # This estimates the COG residual variances 
COG_T4~~resCOGvar*COG_T4          # This estimates the COG residual variances 

NEU_T1~~resNEUvar*NEU_T1          # This estimates the NEU residual variances 
NEU_T2~~resNEUvar*NEU_T2          # This estimates the NEU residual variances 
NEU_T3~~resNEUvar*NEU_T3          # This estimates the NEU residual variances 
NEU_T4~~resNEUvar*NEU_T4          # This estimates the NEU residual variances 

#Dynamics

dNEU1~NEUselfFB*NEUlv_T1       # This estimates the NEU self-feedback parameter (equality constrained across timepoints)
dNEU2~NEUselfFB*NEUlv_T2       # This estimates the NEU self-feedback parameter (equality constrained across timepoints) 
dNEU3~NEUselfFB*NEUlv_T3       # This estimates the NEU self-feedback parameter (equality constrained across timepoints)

dCOG1~COGselfFB*COGlv_T1       # This estimates the COG self-feedback parameter (equality constrained across timepoints)
dCOG2~COGselfFB*COGlv_T2       # This estimates the COG self-feedback parameter (equality constrained across timepoints) 
dCOG3~COGselfFB*COGlv_T3       # This estimates the COG self-feedback parameter (equality constrained across timepoints)

dNEU1~COG_to_NEU*COGlv_T1         # This estimates the COG to NEU coupling parameter 
dNEU2~COG_to_NEU*COGlv_T2         # This estimates the COG to NEU coupling parameter 
dNEU3~COG_to_NEU*COGlv_T3         # This estimates the COG to NEU coupling parameter 

dCOG1~NEU_to_COG*NEUlv_T1        # This estimates the NEU to COG coupling parameter 
dCOG2~NEU_to_COG*NEUlv_T2        # This estimates the NEU to COG coupling parameter 
dCOG3~NEU_to_COG*NEUlv_T3        # This estimates the NEU to COG coupling parameter 


iCOG=~1*COGlv_T1                   # This defines the COG intercept measurement model
sCOG=~1*dCOG1+1*dCOG2+1*dCOG3      # This defines the COG slope measurement model
iCOG~1                             # This estimates the COG intercept intercept (mean)
iCOG~~iCOG                         # This estimates the COG intercept variance
sCOG~1                             # This estimates the COG slope intercept
sCOG~~sCOG                         # This estimates the COG slope variance

iNEU=~1*NEUlv_T1                   # This defines the NEU slope measurement model
sNEU=~1*dNEU1+1*dNEU2+1*dNEU3      # This defines the NEU slope measurement model
iNEU~1                             # This estimates the NEU intercept intercept (mean)
iNEU~~iNEU                         # This estimates the NEU intercept variance
sNEU~1                             # This estimates the NEU slope intercept
sNEU~~sNEU                         # This estimates the NEU slope variance

iNEU~~sNEU                      # This estimates the iNEU sNEU covariance
iNEU~~sCOG                      # This estimates the iNEU sCOG covariance
iNEU~~iCOG                      # This estimates the iNEU iCOG covariance
iCOG~~sCOG                      # This estimates the iCOG sCOG covariance        
iCOG~~sNEU                      # This estimates the iCOG sNEU covariance
sCOG~~sNEU                      # This estimates the sCOG sNEU covariance  

'

fitBDCS <- lavaan(BDCS, data=simdatBDCS, estimator='mlr',missing='fiml')
summary(fitBDCS, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE) 


#Visualize the raw data
theme_set(theme_grey(base_size = 18)) #increase text size
id=factor(1:samplesize)
plotdattemp=data.frame(c(simdatBDCS$COG_T1,simdatBDCS$NEU_T1),
                       c(simdatBDCS$COG_T2,simdatBDCS$NEU_T2),
                       c(simdatBDCS$COG_T3,simdatBDCS$NEU_T3),
                       c(simdatBDCS$COG_T4,simdatBDCS$NEU_T4),
                       as.factor(c(id,id,id,id)),c(rep('COG',times=samplesize),rep('NEU',times=samplesize)))
colnames(plotdattemp)<-c('T1', 'T2','T3','T4','id','Domain')
plotdat<-melt(plotdattemp,by='id')
ggplot(plotdat,aes(variable,value,group=id,col=Domain))+geom_point(size=3,alpha=.7)+geom_line(alpha=.7)+ggtitle('Bivariate Dual Change Score model')+ylab('Scores')+xlab('Time points')+facet_grid(~Domain)
