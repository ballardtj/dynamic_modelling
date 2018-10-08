#NOTE: This script is used to implement the Bayesian change models presented
#in Ballard, Palada, Griffin, and Neal (2018). The files for the models themselves
#are located in the 'models' folder.


#clear workspace
rm(list=ls())

#load rtan
library(rstan) #for rstan installation instructions, see https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

#make sure working directory is set to the "analysis" folder

#load data
load("data/clean/goal_data.RData")

data = filter(data,condition==1) #only approach for now
stan_list = as.list(data)
stan_list$Ntotal = nrow(data)
stan_list$Nsubj = length(unique(data$subject))
stan_list$Ntrial = length(unique(data$trial))
stan_list$time = stan_list$time/60
stan_list$goal = data %>% group_by(subject,trial) %>% summarise(goal = mean(goal)) %>% pull(goal)


##---------------------------------------------------------------------------
# Model 1: Bottom-up sample-level Model

#implement model
fit_bu_sample = stan(file="models/r1_bottom_up_sample_level_model.stan")

#view summary of results
fit_sample

#save results
save(fit_sample,file="../output/1_sample_fit.RData")

#get percentage of alpha distribution > 0
mean((rstan::extract(fit_sample,'alpha')[[1]]>0))

#get percentage of beta distribution > 0
mean((rstan::extract(fit_sample,'beta')[[1]]>0))

##---------------------------------------------------------------------------
# Model 2: Person-level Model

#implement model
fit_person = stan(file="../models/2_person_level_model.stan")

#view summary of results
fit_person

#save results
save(fit_person,file="../output/2_person_fit.RData")

##---------------------------------------------------------------------------
# Model 3: Hierarchical Model

#implement model
fit_hier = stan(file="../models/3_hierarchical_model.stan")

#view summary of results
fit_hier

#save results
save(fit_hier,file="../output/3_hierarchical_fit.RData")

##---------------------------------------------------------------------------
# Model 4: Multiple-group Model

#implement model
fit_multigroup = stan(file="../models/4_multiple_group_model.stan")

#view summary of results
fit_multigroup

#save results
save(fit_multigroup,file="../output/4_multigroup_fit.RData")

#get alpha and beta parameters associated with each mixture
alpha1=extract(fit_multigroup,'alpha')[[1]][,1] #alpha for mixture 1
alpha2=extract(fit_multigroup,'alpha')[[1]][,2] #alpha for mixture 2
beta1=extract(fit_multigroup,'beta')[[1]][,1] #beta for mixture 1
beta2=extract(fit_multigroup,'beta')[[1]][,2] #beta for mixture 2

#get upper and lower bounds on 95% CI on alpha difference and beta difference between conditions
quantile(alpha1-alpha2,0.025) #lower bound of 95% CI on alpha difference
quantile(alpha1-alpha2,0.975) #upper bound of 95% CI on alpha difference
quantile(beta1-beta2,0.025) #lower bound of 95% CI on beta difference
quantile(beta1-beta2,0.975) #lower bound of 95% CI on beta difference

#get percentage of alpha difference distribution and beta difference distribution greater than 0
#(or equivilantly, the percentage of the samples in which alpha for mixture 1 is greater than alpha for mixture 2, and likewise for beta)
mean(alpha1>alpha2) #alpha difference
mean(beta1>beta2) #beta difference

##---------------------------------------------------------------------------
# Model 5: Two Mixture Model

#implement model
fit_two_mixture = stan(file="../models/5_two_mixture_model.stan")

#view summary of results
fit_two_mixture

#view summary of results
save(fit_two_mixture,file="../output/5_two_mixture_fit.RData")


##---------------------------------------------------------------------------
# Model 6: Multiple Group Hierarchical Model (not presented in paper)

#Create new object that contains a single element for each subject, indicating the condition of each subject
subj_cond = condition[trial==1]

#Implement model
fit_multihiergroup = stan(file="../models/6_multiple_group_hierarchical_model.stan")

#View model summary
fit_multihiergroup


##---------------------------------------------------------------------------
# Model 7: N-Mixture Model (not presented in paper)

#Create new object containing a single element that sets the number of mixtures to model
Nmix = 3

#Implement model
fit_n_mixture= stan(file="../models/7_n_mixture_model.stan")

#View model summary
fit_n_mixture

#Note: The 3 mixture model doesn't actually converge well, which is often an indication that there are more mixtures specified by the model than in the data.
traceplot(fit_n_mixture,c('alpha','beta'))


