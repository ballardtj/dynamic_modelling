#Univariate latent change score model of performance 

model <-'
####################
# Fixed Parameters #
####################

#Performance
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

#goal

goal2 ~ 1*goal1       # Fixed regression of X2 on X1
goal3 ~ 1*goal2       # Fixed regression of X3 on X2
goal4 ~ 1*goal3       # Fixed regression 
goal5 ~ 1*goal4       # Fixed regression 
goal6 ~ 1*goal5       # Fixed regression
goal7 ~ 1*goal6       # Fixed regression 
goal8 ~ 1*goal7       # Fixed regression 
goal9 ~ 1*goal8       # Fixed regression 
goal10 ~ 1*goal9       # Fixed regression 

dgoal1 =~ 1*goal2     # Fixed regression of dX1 on X2
dgoal2 =~ 1*goal3     # Fixed regression of dX2 on X3
dgoal3 =~ 1*goal4     # Fixed regression 
dgoal4 =~ 1*goal5     # Fixed regression 
dgoal5 =~ 1*goal6     # Fixed regression 
dgoal6 =~ 1*goal7     # Fixed regression 
dgoal7 =~ 1*goal8     # Fixed regression 
dgoal8 =~ 1*goal9     # Fixed regression 
dgoal9 =~ 1*goal10     # Fixed regression 

goal2 ~ 0*1        # This line constrains the intercept of X2 to 0
goal3 ~ 0*1        # This line constrains the intercept of X3 to 0
goal4 ~ 0*1        # This line constrains the intercept 
goal5 ~ 0*1        # This line constrains the intercept 
goal6 ~ 0*1        # This line constrains the intercept 
goal7 ~ 0*1        # This line constrains the intercept 
goal8 ~ 0*1        # This line constrains the intercept 
goal9 ~ 0*1        # This line constrains the intercept 
goal10 ~ 0*1        # This line constrains the intercept 

goal2 ~~ 0*goal2      # This fixes the variance of the X2 to 0  
goal3 ~~ 0*goal3      # This fixes the variance of the X3 to 0 
goal4 ~~ 0*goal4      # This fixes the variance
goal5 ~~ 0*goal5      # This fixes the variance
goal6 ~~ 0*goal6      # This fixes the variance
goal7 ~~ 0*goal7      # This fixes the variance
goal8 ~~ 0*goal8      # This fixes the variance
goal9 ~~ 0*goal9      # This fixes the variance
goal10 ~~ 0*goal10      # This fixes the variance

########################
# Estimated Parameters #
########################

#Performance

perf1 ~ 2*1             # This estimates the intercept of X1 
perf1 ~~ 1*perf1        # This estimates the variance of X1 

dperf1 ~ 0.3*1             # This estimates the intercept of the change scores 
dperf2 ~ 0.3*1             # This estimates the intercept of the change scores 
dperf3 ~ 0.3*1             # This estimates the intercept of the change scores 
dperf4 ~ 0.3*1             # This estimates the intercept of the change scores 
dperf5 ~ 0.3*1             # This estimates the intercept of the change scores 
dperf6 ~ 0.3*1             # This estimates the intercept of the change scores 
dperf7 ~ 0.3*1             # This estimates the intercept of the change scores 
dperf8 ~ 0.3*1             # This estimates the intercept of the change scores 
dperf9 ~ 0.3*1             # This estimates the intercept of the change scores 

dperf1 ~~  1*dperf1         # This estimates the variance of the change scores 
dperf2 ~~  1*dperf2         # This estimates the variance of the change scores 
dperf3 ~~  1*dperf3         # This estimates the variance of the change scores 
dperf4 ~~  1*dperf4         # This estimates the variance of the change scores 
dperf5 ~~  1*dperf5         # This estimates the variance of the change scores 
dperf6 ~~  1*dperf6         # This estimates the variance of the change scores 
dperf7 ~~  1*dperf7         # This estimates the variance of the change scores 
dperf8 ~~  1*dperf8         # This estimates the variance of the change scores 
dperf9 ~~  1*dperf9         # This estimates the variance of the change scores 

dperf1~0.4*perf1 + 0.2*goal1    # This estimates the self-feedback and coupling parameter
dperf2~0.4*perf2 + 0.2*goal2    # This estimates the self-feedback and coupling parameter
dperf3~0.4*perf3 + 0.2*goal3    # This estimates the self-feedback and coupling parameter
dperf4~0.4*perf4 + 0.2*goal4    # This estimates the self-feedback and coupling parameter
dperf5~0.4*perf5 + 0.2*goal5    # This estimates the self-feedback and coupling parameter
dperf6~0.4*perf6 + 0.2*goal6    # This estimates the self-feedback and coupling parameter
dperf7~0.4*perf7 + 0.2*goal7    # This estimates the self-feedback and coupling parameter
dperf8~0.4*perf8 + 0.2*goal8    # This estimates the self-feedback and coupling parameter
dperf9~0.4*perf9 + 0.2*goal9    # This estimates the self-feedback and coupling parameter

#Goal

goal1 ~ 3*1             # This estimates the intercept of X1 
goal1 ~~ 3.5*goal1        # This estimates the variance of X1 

dgoal1 ~ 0.5*1             # This estimates the intercept of the change scores 
dgoal2 ~ 0.5*1             # This estimates the intercept of the change scores 
dgoal3 ~ 0.5*1             # This estimates the intercept of the change scores 
dgoal4 ~ 0.5*1             # This estimates the intercept of the change scores 
dgoal5 ~ 0.5*1             # This estimates the intercept of the change scores 
dgoal6 ~ 0.5*1             # This estimates the intercept of the change scores 
dgoal7 ~ 0.5*1             # This estimates the intercept of the change scores 
dgoal8 ~ 0.5*1             # This estimates the intercept of the change scores 
dgoal9 ~ 0.5*1             # This estimates the intercept of the change scores 

dgoal1 ~~  0.4*dgoal1         # This estimates the variance of the change scores 
dgoal2 ~~  0.4*dgoal2         # This estimates the variance of the change scores 
dgoal3 ~~  0.4*dgoal3         # This estimates the variance of the change scores 
dgoal4 ~~  0.4*dgoal4         # This estimates the variance of the change scores 
dgoal5 ~~  0.4*dgoal5         # This estimates the variance of the change scores 
dgoal6 ~~  0.4*dgoal6         # This estimates the variance of the change scores 
dgoal7 ~~  0.4*dgoal7         # This estimates the variance of the change scores 
dgoal8 ~~  0.4*dgoal8         # This estimates the variance of the change scores 
dgoal9 ~~  0.4*dgoal9         # This estimates the variance of the change scores 

dgoal1~0.3*goal1 + 0.1*perf1           # This estimates the self-feedback and coupling parameter
dgoal2~0.3*goal2 + 0.1*perf2           # This estimates the self-feedback and coupling parameter
dgoal3~0.3*goal3 + 0.1*perf3           # This estimates the self-feedback and coupling parameter
dgoal4~0.3*goal4 + 0.1*perf4           # This estimates the self-feedback and coupling parameter
dgoal5~0.3*goal5 + 0.1*perf5           # This estimates the self-feedback and coupling parameter
dgoal6~0.3*goal6 + 0.1*perf6           # This estimates the self-feedback and coupling parameter
dgoal7~0.3*goal7 + 0.1*perf7           # This estimates the self-feedback and coupling parameter
dgoal8~0.3*goal8 + 0.1*perf8           # This estimates the self-feedback and coupling parameter
dgoal9~0.3*goal9 + 0.1*perf9           # This estimates the self-feedback and coupling parameter

# Covariances
goal1 ~~ 0.4*perf1

dgoal1 ~~ 0.2*dperf1
dgoal2 ~~ 0.2*dperf2
dgoal3 ~~ 0.2*dperf3
dgoal4 ~~ 0.2*dperf4
dgoal5 ~~ 0.2*dperf5
dgoal6 ~~ 0.2*dperf6
dgoal7 ~~ 0.2*dperf7
dgoal8 ~~ 0.2*dperf8
dgoal9 ~~ 0.2*dperf9

' #end model specification

# goal2 ~~ 0.4*perf2
# goal3 ~~ 0.4*perf3
# goal4 ~~ 0.4*perf4
# goal5 ~~ 0.4*perf5
# goal6 ~~ 0.4*perf6
# goal7 ~~ 0.4*perf7
# goal8 ~~ 0.4*perf8
# goal9 ~~ 0.4*perf9
# goal10 ~~ 0.4*perf10


