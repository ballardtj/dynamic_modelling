#This code adapts the IMWM so that it can be run on Gee et al (2018) data

# Simulation setup

tstep = 1
tspan = c(0,100)
tvec = seq( tspan[1] , tspan[2], by = tstep )

tchange = 30
goal_effect_weight = 0.5
intrinsic_value = 0.75
feedback = 1
ability = 1
task_complexity = 0.2
k = 0.3
fade = 0.05
initial_weight = 2.5
assigned_goal_effect = 0
assigned_goal_difficulty = 0.94
personality = 0.5

# Initial value, "xyz_0" indicates the initial value of xyz

SELF_EFFICACY_0 = personality
goal_specificity_0 = 0
value = intrinsic_value + assigned_goal_difficulty*goal_specificity_0*goal_effect_weight

if (SELF_EFFICACY_0*value*goal_specificity_0 >= assigned_goal_difficulty) {
  personal_goal_0 = assigned_goal_difficulty
} else {
  personal_goal_0 = SELF_EFFICACY_0*value
}

EFFORT_0 = personal_goal_0
performance_0 = min(EFFORT_0*(1-task_complexity)*ability, ability)
EFFORT_vec = rep(0,length(tvec))
SELF_EFFICACY_vec = rep(0,length(tvec))
performance_vec = rep(0,length(tvec))
EFFORT_vec[1] = EFFORT_0
SELF_EFFICACY_vec[1] = SELF_EFFICACY_0
performance_vec[1] = performance_0


# Run simulation % “_minus” indicates the variable value at the previous time step
#“_vec” stores the simulation results (time-series) of the variable

for (i in 1:(length(tvec)-1)) {

  #EFFORT_minus = EFFORT_vec[i]
  #SELF_EFFICACY_minus = SELF_EFFICACY_vec[i]
  #Weight = initial_weight/(1+i*tstep*fade)

  value = intrinsic_value + assigned_goal_difficulty*goal_effect_weight

  if (SELF_EFFICACY_vec[i]*value >= assigned_goal_difficulty ){
    personal_goal = assigned_goal_difficulty
  } else {
    personal_goal = SELF_EFFICACY_vec[i]*value
  }

  #performance = min(EFFORT_vec[i]*(1-task_complexity)*ability, ability)
  #performance = performance_vec[i]
  capacity = performance_vec[i]/EFFORT_vec[i]

  Rate_EFFORT = personal_goal - performance_vec[i]
  Rate_SELF_EFFICACY = k*(capacity-SELF_EFFICACY_vec[i])

  EFFORT_vec[i+1] = EFFORT_vec[i] + Rate_EFFORT*tstep
  SELF_EFFICACY_vec[i+1] = SELF_EFFICACY_vec[i] + Rate_SELF_EFFICACY*tstep


  performance_vec[i+1] = min( EFFORT_vec[i+1]*(1-task_complexity)*ability , ability) #ability is the ceiling on performance

}

plot(tvec,EFFORT_vec)
plot(tvec,SELF_EFFICACY_vec)
plot(tvec,performance_vec)
