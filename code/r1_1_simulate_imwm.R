#This code runs the Integrated Model of Work Motivation as it is specified in Vancouver, Wang, & Li (2018)

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

  EFFORT_minus = EFFORT_vec[i]
  SELF_EFFICACY_minus = SELF_EFFICACY_vec[i]
  Weight = initial_weight/(1+i*tstep*fade)

  if (i >= tchange){
    goal_specificity = 1;
  } else {
    goal_specificity = 1
  }

  value = intrinsic_value + assigned_goal_difficulty*goal_specificity*goal_effect_weight

  if (SELF_EFFICACY_minus*value*goal_specificity >= assigned_goal_difficulty ){
    personal_goal = assigned_goal_difficulty
  } else {
    personal_goal = SELF_EFFICACY_minus*value
  }

  performance = min(EFFORT_minus*(1-task_complexity)*ability, ability)
  capacity = performance/EFFORT_minus

  Rate_EFFORT = personal_goal - (performance - (1-feedback)*SELF_EFFICACY_minus)
  Rate_SELF_EFFICACY = k*(capacity-SELF_EFFICACY_minus) + Weight*assigned_goal_difficulty*goal_specificity*assigned_goal_effect

  EFFORT = EFFORT_minus + Rate_EFFORT*tstep
  SELF_EFFICACY = SELF_EFFICACY_minus + Rate_SELF_EFFICACY*tstep

  EFFORT_vec[i+1] = EFFORT
  SELF_EFFICACY_vec[i+1] = SELF_EFFICACY
  performance_vec[i+1] = min(EFFORT*(1-task_complexity)*ability, ability)

}

plot(tvec,EFFORT_vec)
plot(tvec,SELF_EFFICACY_vec)
plot(tvec,performance_vec)
