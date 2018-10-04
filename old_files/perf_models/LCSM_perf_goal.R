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

dperf1~label("perf_sf")*perf1 + label("dperf_on_goal")*goal1    # This estimates the self-feedback and coupling parameter
dperf2~equal("perf_sf")*perf2 + equal("dperf_on_goal")*goal2    # This estimates the self-feedback and coupling parameter
dperf3~equal("perf_sf")*perf3 + equal("dperf_on_goal")*goal3    # This estimates the self-feedback and coupling parameter
dperf4~equal("perf_sf")*perf4 + equal("dperf_on_goal")*goal4    # This estimates the self-feedback and coupling parameter
dperf5~equal("perf_sf")*perf5 + equal("dperf_on_goal")*goal5    # This estimates the self-feedback and coupling parameter
dperf6~equal("perf_sf")*perf6 + equal("dperf_on_goal")*goal6    # This estimates the self-feedback and coupling parameter
dperf7~equal("perf_sf")*perf7 + equal("dperf_on_goal")*goal7    # This estimates the self-feedback and coupling parameter
dperf8~equal("perf_sf")*perf8 + equal("dperf_on_goal")*goal8    # This estimates the self-feedback and coupling parameter
dperf9~equal("perf_sf")*perf9 + equal("dperf_on_goal")*goal9    # This estimates the self-feedback and coupling parameter

#Goal

goal1 ~ 1             # This estimates the intercept of X1 
goal1 ~~ goal1        # This estimates the variance of X1 

dgoal1 ~ label("dgoal_int")*1             # This estimates the intercept of the change scores 
dgoal2 ~ equal("dgoal_int")*1             # This estimates the intercept of the change scores 
dgoal3 ~ equal("dgoal_int")*1             # This estimates the intercept of the change scores 
dgoal4 ~ equal("dgoal_int")*1             # This estimates the intercept of the change scores 
dgoal5 ~ equal("dgoal_int")*1             # This estimates the intercept of the change scores 
dgoal6 ~ equal("dgoal_int")*1             # This estimates the intercept of the change scores 
dgoal7 ~ equal("dgoal_int")*1             # This estimates the intercept of the change scores 
dgoal8 ~ equal("dgoal_int")*1             # This estimates the intercept of the change scores 
dgoal9 ~ equal("dgoal_int")*1             # This estimates the intercept of the change scores 

dgoal1 ~~  label("dgoal_var")*dgoal1         # This estimates the variance of the change scores 
dgoal2 ~~  equal("dgoal_var")*dgoal2         # This estimates the variance of the change scores 
dgoal3 ~~  equal("dgoal_var")*dgoal3         # This estimates the variance of the change scores 
dgoal4 ~~  equal("dgoal_var")*dgoal4         # This estimates the variance of the change scores 
dgoal5 ~~  equal("dgoal_var")*dgoal5         # This estimates the variance of the change scores 
dgoal6 ~~  equal("dgoal_var")*dgoal6         # This estimates the variance of the change scores 
dgoal7 ~~  equal("dgoal_var")*dgoal7         # This estimates the variance of the change scores 
dgoal8 ~~  equal("dgoal_var")*dgoal8         # This estimates the variance of the change scores 
dgoal9 ~~  equal("dgoal_var")*dgoal9         # This estimates the variance of the change scores 

dgoal1~label("goal_sf")*goal1 + label("dgoal_on_perf")*perf1           # This estimates the self-feedback and coupling parameter
dgoal2~equal("goal_sf")*goal2 + equal("dgoal_on_perf")*perf2           # This estimates the self-feedback and coupling parameter
dgoal3~equal("goal_sf")*goal3 + equal("dgoal_on_perf")*perf3           # This estimates the self-feedback and coupling parameter
dgoal4~equal("goal_sf")*goal4 + equal("dgoal_on_perf")*perf4           # This estimates the self-feedback and coupling parameter
dgoal5~equal("goal_sf")*goal5 + equal("dgoal_on_perf")*perf5           # This estimates the self-feedback and coupling parameter
dgoal6~equal("goal_sf")*goal6 + equal("dgoal_on_perf")*perf6           # This estimates the self-feedback and coupling parameter
dgoal7~equal("goal_sf")*goal7 + equal("dgoal_on_perf")*perf7           # This estimates the self-feedback and coupling parameter
dgoal8~equal("goal_sf")*goal8 + equal("dgoal_on_perf")*perf8           # This estimates the self-feedback and coupling parameter
dgoal9~equal("goal_sf")*goal9 + equal("dgoal_on_perf")*perf9           # This estimates the self-feedback and coupling parameter

# Covariances
goal1 ~~ label("t1_cov")*perf1

dgoal1 ~~ label("d_cov")*dperf1
dgoal2 ~~ equal("d_cov")*dperf2
dgoal3 ~~ equal("d_cov")*dperf3
dgoal4 ~~ equal("d_cov")*dperf4
dgoal5 ~~ equal("d_cov")*dperf5
dgoal6 ~~ equal("d_cov")*dperf6
dgoal7 ~~ equal("d_cov")*dperf7
dgoal8 ~~ equal("d_cov")*dperf8
dgoal9 ~~ equal("d_cov")*dperf9

' #end model specification

# goal2 ~~ equal("t1_cov")*perf2
# goal3 ~~ equal("t1_cov")*perf3
# goal4 ~~ equal("t1_cov")*perf4
# goal5 ~~ equal("t1_cov")*perf5
# goal6 ~~ equal("t1_cov")*perf6
# goal7 ~~ equal("t1_cov")*perf7
# goal8 ~~ equal("t1_cov")*perf8
# goal9 ~~ equal("t1_cov")*perf9
# goal10 ~~ equal("t1_cov")*perf10

