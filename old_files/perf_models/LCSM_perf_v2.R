#Univariate latent change score model of performance 
#16 April 2018 - updated to be more in line with Lio et al 2015
#   - create latent x variable for each time step
#   - realigned latent change variables to reflect current timestep

model <-'

#Fixed Parameters

#First, the latent score of each X variable is created by loading each X variable on its respective latent variable (i.e. x1–x6) with a fixed factor loading of 1.0.
latent1 =~ 1*perf1
latent2 =~ 1*perf2
latent3 =~ 1*perf3
latent4 =~ 1*perf4
latent5 =~ 1*perf5
latent6 =~ 1*perf6
latent7 =~ 1*perf7
latent8 =~ 1*perf8
latent9 =~ 1*perf9
latent10 =~ 1*perf10

#Second, the autoregressive effects are specified with a fixed value of 1.0 between x1 and x2, x2 and x3, and so on
latent2 ~ 1*latent1       # Fixed regression of X2 on X1
latent3 ~ 1*latent2       # Fixed regression of X3 on X2
latent4 ~ 1*latent3       # Fixed regression 
latent5 ~ 1*latent4       # Fixed regression 
latent6 ~ 1*latent5       # Fixed regression
latent7 ~ 1*latent6       # Fixed regression 
latent8 ~ 1*latent7       # Fixed regression 
latent9 ~ 1*latent8       # Fixed regression 
latent10 ~ 1*latent9       # Fixed regression 

#Third, the latent change score (i.e. Dx2–Dx6) at each time point (excluding T1) is specified as a latent cause 
#of the latent x variable at that time point with a fixed factor loading of 1.

dperf2 =~ 1*latent2     # Fixed regression 
dperf3 =~ 1*latent3     # Fixed regression 
dperf4 =~ 1*latent4     # Fixed regression 
dperf5 =~ 1*latent5     # Fixed regression 
dperf6 =~ 1*latent6     # Fixed regression 
dperf7 =~ 1*latent7     # Fixed regression 
dperf8 =~ 1*latent8     # Fixed regression 
dperf9 =~ 1*latent9     # Fixed regression 
dperf10 =~ 1*latent10   # Fixed regression

#Fourth, the path from each latent x variable on the latent change score at a subsequent timepoint is specified and freely estimated (i.e. b parame- ters). 
#Equality constraints can be imposed on b parameters depending on the specific research question.

dperf2~label("sf")*latent1              # This estimates the self-feedback parameter
dperf3~equal("sf")*latent2              # This estimates the self-feedback parameter
dperf4~equal("sf")*latent3              # This estimates the self-feedback parameter
dperf5~equal("sf")*latent4              # This estimates the self-feedback parameter
dperf6~equal("sf")*latent5              # This estimates the self-feedback parameter
dperf7~equal("sf")*latent6              # This estimates the self-feedback parameter
dperf8~equal("sf")*latent7              # This estimates the self-feedback parameter
dperf9~equal("sf")*latent8              # This estimates the self-feedback parameter
dperf10~equal("sf")*latent9             # This estimates the self-feedback parameter

#Fifth, a latent intercept factor is created as indicated by x1 with a fixed factor loading at 1.0
l_int =~ 1*latent1
l_int ~ 1            #estimate mean
l_int ~~ l_int       #estimate variance

#Sixth, a latent slope factor is specified as a latent cause of all latent change scores with factor loadings
#representing a parameters. Equality constraints can be imposed on a parameters, which means all a parameters 
#are fixed to 1.0 and renders thetaS to represent a constant change component in delta x over time.

l_slope =~ 1*dperf2
l_slope =~ 1*dperf3
l_slope =~ 1*dperf4
l_slope =~ 1*dperf5
l_slope =~ 1*dperf6
l_slope =~ 1*dperf7
l_slope =~ 1*dperf8
l_slope =~ 1*dperf9
l_slope =~ 1*dperf10
l_slope ~ 1
l_slope ~~ l_slope

#Further, residuals of X1–X6 are set to be equal, indicating stable measurement error variance over time

perf1 ~~ label("perf_var")*perf1
perf2 ~~ label("perf_var")*perf2
perf3 ~~ label("perf_var")*perf3        # This line constrains the intercept of X3 to 0
perf4 ~~ label("perf_var")*perf4        # This line constrains the intercept
perf5 ~~ label("perf_var")*perf5         # This line constrains the intercept
perf6 ~~ label("perf_var")*perf6         # This line constrains the intercept
perf7 ~~ label("perf_var")*perf7         # This line constrains the intercept
perf8 ~~ label("perf_var")*perf8         # This line constrains the intercept
perf9 ~~ label("perf_var")*perf9         # This line constrains the intercept
perf10 ~~ label("perf_var")*perf10         # This line constrains the intercept

# perf1 ~~ 0.001*perf1
# perf2 ~~ 0.001*perf2
# perf3 ~~ 0.001*perf3        # This line constrains the intercept of X3 to 0
# perf4 ~~ 0.001*perf4        # This line constrains the intercept
# perf5 ~~ 0.001*perf5         # This line constrains the intercept
# perf6 ~~ 0.001*perf6         # This line constrains the intercept
# perf7 ~~ 0.001*perf7         # This line constrains the intercept
# perf8 ~~ 0.001*perf8         # This line constrains the intercept
# perf9 ~~ 0.001*perf9         # This line constrains the intercept
# perf10 ~~ 0.001*perf10         # This line constrains the intercept

' #end model specification

#The steps below are automatically implemented by lavaan

# #As a final step, the means of X1–X6 (observed), x1– x6 (latent), and Dx2–Dx6
# #are all fixed to zero.
# 
# perf1 ~ 0*1        # This line constrains the mean of X to 0
# perf2 ~ 0*1
# perf3 ~ 0*1
# perf4 ~ 0*1
# perf5 ~ 0*1
# perf6 ~ 0*1
# perf7 ~ 0*1
# perf8 ~ 0*1
# perf9 ~ 0*1
# perf10 ~ 0*1
# 
# latent1 ~ 0*1        # This line constrains the mean of X to 0
# latent2 ~ 0*1
# latent3 ~ 0*1
# latent4 ~ 0*1
# latent5 ~ 0*1
# latent6 ~ 0*1
# latent7 ~ 0*1
# latent8 ~ 0*1
# latent9 ~ 0*1
# latent10 ~ 0*1
# 
# dperf2 ~ 0*1            # This line constrains the mean of X to 0
# dperf3 ~ 0*1
# dperf4 ~ 0*1
# dperf5 ~ 0*1
# dperf6 ~ 0*1
# dperf7 ~ 0*1
# dperf8 ~ 0*1
# dperf9 ~ 0*1
# dperf10 ~ 0*1
# 
# #... as well as the residual variances of x1–x6 and Dx2–Dx6, 
# 
# latent1 ~~ 0*latent1      # This fixes the variance of the X2 to 0
# latent2 ~~ 0*latent2      # This fixes the variance of the X2 to 0  
# latent3 ~~ 0*latent3      # This fixes the variance of the X3 to 0 
# latent4 ~~ 0*latent4      # This fixes the variance
# latent5 ~~ 0*latent5      # This fixes the variance
# latent6 ~~ 0*latent6      # This fixes the variance
# latent7 ~~ 0*latent7      # This fixes the variance
# latent8 ~~ 0*latent8      # This fixes the variance
# latent9 ~~ 0*latent9      # This fixes the variance
# latent10 ~~ 0*latent10      # This fixes the variance
# 
# dperf2 ~~  0*dperf2         
# dperf3 ~~  0*dperf3         
# dperf4 ~~  0*dperf4        
# dperf5 ~~  0*dperf5        
# dperf6 ~~  0*dperf6         
# dperf7 ~~  0*dperf7         
# dperf8 ~~  0*dperf8 
# dperf9 ~~  0*dperf9      
# dperf10 ~~  0*dperf10 