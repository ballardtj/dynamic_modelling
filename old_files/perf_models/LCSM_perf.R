#Univariate latent change score model of performance 

model <-'

#Fixed Parameters
#Autoregressive effects
perf2 ~ 1*perf1       # Fixed regression of X2 on X1
perf3 ~ 1*perf2       # Fixed regression of X3 on X2
perf4 ~ 1*perf3       # Fixed regression 
perf5 ~ 1*perf4       # Fixed regression 
perf6 ~ 1*perf5       # Fixed regression
perf7 ~ 1*perf6       # Fixed regression 
perf8 ~ 1*perf7       # Fixed regression 
perf9 ~ 1*perf8       # Fixed regression 
perf10 ~ 1*perf9       # Fixed regression 

dperf1 =~ 1*perf2     # Fixed regression of dX1 on X2
dperf2 =~ 1*perf3     # Fixed regression of dX2 on X3
dperf3 =~ 1*perf4     # Fixed regression 
dperf4 =~ 1*perf5     # Fixed regression 
dperf5 =~ 1*perf6     # Fixed regression 
dperf6 =~ 1*perf7     # Fixed regression 
dperf7 =~ 1*perf8     # Fixed regression 
dperf8 =~ 1*perf9     # Fixed regression 
dperf9 =~ 1*perf10     # Fixed regression 

perf2 ~ 0*1        # This line constrains the intercept of X2 to 0
perf3 ~ 0*1        # This line constrains the intercept of X3 to 0
perf4 ~ 0*1        # This line constrains the intercept 
perf5 ~ 0*1        # This line constrains the intercept 
perf6 ~ 0*1        # This line constrains the intercept 
perf7 ~ 0*1        # This line constrains the intercept 
perf8 ~ 0*1        # This line constrains the intercept 
perf9 ~ 0*1        # This line constrains the intercept 
perf10 ~ 0*1        # This line constrains the intercept 

perf2 ~~ 0*perf2      # This fixes the variance of the X2 to 0  
perf3 ~~ 0*perf3      # This fixes the variance of the X3 to 0 
perf4 ~~ 0*perf4      # This fixes the variance
perf5 ~~ 0*perf5      # This fixes the variance
perf6 ~~ 0*perf6      # This fixes the variance
perf7 ~~ 0*perf7      # This fixes the variance
perf8 ~~ 0*perf8      # This fixes the variance
perf9 ~~ 0*perf9      # This fixes the variance
perf10 ~~ 0*perf10      # This fixes the variance

#Estimated Parameters
perf1 ~ 1             # This estimates the intercept of X1 
perf1 ~~ perf1        # This estimates the variance of X1 

dperf1 ~ label("dperf_int")*1             # This estimates the intercept of the change scores 
dperf2 ~ equal("dperf_int")*1             # This estimates the intercept of the change scores 
dperf3 ~ equal("dperf_int")*1             # This estimates the intercept of the change scores 
dperf4 ~ equal("dperf_int")*1             # This estimates the intercept of the change scores 
dperf5 ~ equal("dperf_int")*1             # This estimates the intercept of the change scores 
dperf6 ~ equal("dperf_int")*1             # This estimates the intercept of the change scores 
dperf7 ~ equal("dperf_int")*1             # This estimates the intercept of the change scores 
dperf8 ~ equal("dperf_int")*1             # This estimates the intercept of the change scores 
dperf9 ~ equal("dperf_int")*1             # This estimates the intercept of the change scores 

dperf1 ~~  label("dperf_var")*dperf1         # This estimates the variance of the change scores 
dperf2 ~~  equal("dperf_var")*dperf2         # This estimates the variance of the change scores 
dperf3 ~~  equal("dperf_var")*dperf3         # This estimates the variance of the change scores 
dperf4 ~~  equal("dperf_var")*dperf4         # This estimates the variance of the change scores 
dperf5 ~~  equal("dperf_var")*dperf5         # This estimates the variance of the change scores 
dperf6 ~~  equal("dperf_var")*dperf6         # This estimates the variance of the change scores 
dperf7 ~~  equal("dperf_var")*dperf7         # This estimates the variance of the change scores 
dperf8 ~~  equal("dperf_var")*dperf8         # This estimates the variance of the change scores 
dperf9 ~~  equal("dperf_var")*dperf9         # This estimates the variance of the change scores 

#Self-feedback parameter
dperf1~label("sf")*perf1              # This estimates the self-feedback parameter
dperf2~equal("sf")*perf2              # This estimates the self-feedback parameter
dperf3~equal("sf")*perf3              # This estimates the self-feedback parameter
dperf4~equal("sf")*perf4              # This estimates the self-feedback parameter
dperf5~equal("sf")*perf5              # This estimates the self-feedback parameter
dperf6~equal("sf")*perf6              # This estimates the self-feedback parameter
dperf7~equal("sf")*perf7              # This estimates the self-feedback parameter
dperf8~equal("sf")*perf8              # This estimates the self-feedback parameter
dperf9~equal("sf")*perf9              # This estimates the self-feedback parameter

' #end model specification