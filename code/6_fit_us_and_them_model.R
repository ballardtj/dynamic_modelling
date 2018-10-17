#NOTE: This script is used to implement the Bayesian change models presented
#in Ballard, Palada, Griffin, and Neal (2018). The files for the models themselves
#are located in the 'models' folder.


#clear workspace
rm(list=ls())

#load rtan
library(rstan) #for rstan installation instructions, see https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(tidyverse)
#make sure working directory is set to the "analysis" folder

#load data
#load(file="data/raw/us_and_them.RData")
load(file="us_and_them.RData")

setpoint = 0.5
Ntotal = 1000
data_tmp = filter(data,iter<=Ntotal)
stan_list = as.list(data_tmp)
stan_list$Ntotal = Ntotal
stan_list$Nsims = 1000
#pre-generate random numbers
stan_list$runif1 = matrix( runif(n=stan_list$Ntotal * stan_list$Nsims ),nrow=stan_list$Nsims )
stan_list$runif2 = matrix( runif(n=stan_list$Ntotal * stan_list$Nsims ),nrow=stan_list$Nsims )
stan_list$runif3 = matrix( runif(n=stan_list$Ntotal * stan_list$Nsims ),nrow=stan_list$Nsims )
stan_list$Nplayers = 12
stan_list$Nplayers_sq = stan_list$Nplayers^2
stan_list$Probx_0 = (1-diag(1,stan_list$Nplayers))*setpoint
stan_list$Payx_0 = rep(0,stan_list$Nplayers)
stan_list$player_n = ceiling( matrix( runif(n=stan_list$Ntotal * stan_list$Nsims ),nrow=stan_list$Nsims )*stan_list$Nplayers )
stan_list$player_m = ceiling( matrix( runif(n=stan_list$Ntotal * stan_list$Nsims ),nrow=stan_list$Nsims )*(stan_list$Nplayers-1) )
stan_list$player_m[stan_list$player_m>=stan_list$player_n] = stan_list$player_m[stan_list$player_m>=stan_list$player_n]+1
stan_list$rnorm1 = matrix(rnorm(n=stan_list$Nsims * stan_list$Nplayers^2,mean=0,sd=0.0001),nrow=stan_list$Nsims)

fit = stan(#file="models/r3_us_and_them_model.stan",
           file = "r3_us_and_them_model.stan",
           data = stan_list,
           chains=4,
           seed=12345)

save(fit,"fit_us_and_them.RData")

#   x = matrix(c(0,0.916667,0.125,0.0833333,0.875,0.5,0.916667,0.25,0.916667,0.861111,0.5,0.25,
#   0.916667,0,0.125,0.0555556,0.96875,0.5,0.9375,0.25,0.979167,0.96875,0.5,0.25,
#   0.125,0.125,0,0.833333,0.125,0.25,0.125,0.125,0.125,0.166667,0.166667,0.0555556,
#   0.0833333,0.0555556,0.833333,0,0.125,0.5,0.125,0.25,0.0625,0.125,0.5,0.25,
#   0.875,0.96875,0.125,0.125,0,0.5,0.916667,0.25,0.972222,0.958333,0.5,0.25,
#   0.5,0.5,0.25,0.5,0.5,0,0.5,0.25,0.5,0.5,0.833333,0.5,
#   0.916667,0.9375,0.125,0.125,0.916667,0.5,0,0.0833333,0.9375,0.875,0.5,0.166667,
#   0.25,0.25,0.125,0.25,0.25,0.25,0.0833333,0,0.25,0.5,0.166667,0.944444,
#   0.916667,0.979167,0.125,0.0625,0.972222,0.5,0.9375,0.25,0,0.96875,0.5,0.0833333,
#   0.861111,0.96875,0.166667,0.125,0.958333,0.5,0.875,0.5,0.96875,0,0.5,0.5,
#   0.5,0.5,0.166667,0.5,0.5,0.833333,0.5,0.166667,0.5,0.5,0,0.5,
#   0.25,0.25,0.0555556,0.25,0.25,0.5,0.166667,0.944444,0.0833333,0.5,0.5,0),nrow=12)
#
#   matrix.power(x, 3)
#
#   original_Q <- x;
#   for(i in 1:2){
#     Q <- Q %*% original_Q;
#   }
#
#   Q = x %*% x %*% x
#
# samples = rstan::extract(fit)
#
# names(samples)
#
# samples$predicted_mean
#
# plot(1:1000,samples$predicted_mean)
#
# expose_stan_functions(stanmodel="models/r3_us_and_them_model.stan")
#
# res = simulate_abm(stan_list$Ntotal,
#              stan_list$Nsims,
#              stan_list$Nplayers,
#              stan_list$runif1,
#              stan_list$runif2,
#              stan_list$runif3,
#              stan_list$rnorm1,
#              stan_list$Probx_0,
#              stan_list$Payx_0,
#              stan_list$player_n,
#              stan_list$player_m )
#
#
# load("data/clean/goal_data.RData")
#
# data = filter(data,condition==1) #only approach for now
# stan_list = as.list(data)
# #stan_list$subject = ceiling (stan_list$subject / 2)
# stan_list$Ntotal = nrow(data)
# #stan_list$Nsubj = length(unique(data$subject))
# #stan_list$Ntrial = length(unique(data$trial))
# #stan_list$time = stan_list$time/60
# #stan_list$goal = data %>% group_by(subject,trial) %>% summarise(goal = mean(goal)) %>% pull(goal)
# #stan_list$trial_index = (stan_list$subject-1)*5 + stan_list$trial
# #stan_list$goal = stan_list$goal / 20
# #stan_list$score = stan_list$score / 20
#
# ##---------------------------------------------------------------------------
# # Model 1: Bottom-up sample-level Model
#
# #implement model
# fit_fb_sample = stan(file="models/r2_feedback_model.stan",data=stan_list,cores=4)
#
# #view summary of results
# fit_fb_sample
#
# # data = dim(samples$sampled_goal)
#
# samples =rstan::extract(fit_fb_sample)
#
# #Get 100 samples for posterior predictives
# samples_used = sample(1:4000,size=100)
# pp_list=list()
#
# for(i in 1:100){
#   pp_list[[i]]=data %>%
#     mutate(#predicted_goal = samples$sampled_goal[samples_used[i],],
#            predicted_effort = samples$sampled_effort[samples_used[i],],
#            predicted_score = samples$sampled_score[samples_used[i],]) %>%
#     group_by(trial,time) %>%
#     summarise(iter = samples_used[i],
#               #predicted_goal = mean(predicted_goal),
#               predicted_effort = mean(predicted_effort),
#               predicted_score = mean(predicted_score),
#               #observed_goal = mean(goal),
#               observed_effort = mean(effort),
#               observed_score = mean(score))
# }
#
# bind_rows(pp_list) %>%
#   gather(key=key,value=value,predicted_effort:observed_score) %>%
#   separate(col=key,into=c('source','variable')) %>%
#   #mutate(condition = factor(condition,levels=c('approach','avoidance'),labels=c('Approach','Avoidance'))) %>%
#   group_by(trial,time,source,variable) %>%
#   summarise(mean = mean(value),
#             upper = quantile(value,0.975),
#             lower = quantile(value,0.025)) %>%
#   ggplot() +
#   geom_ribbon(aes(ymin=lower,ymax=upper,x=time,group=source),fill="skyblue") +
#   geom_line(aes(y=mean,x=time,group=source,colour=source)) +
#   facet_grid(variable ~ trial,scale="free")
#
#
# bind_rows(pp_list) %>%
#   gather(key=key,value=value,predicted_effort:observed_score) %>%
#   separate(col=key,into=c('source','variable')) %>%
#   spread(key=variable,value=value) %>%
#   #mutate(condition = factor(condition,levels=c('approach','avoidance'),labels=c('Approach','Avoidance'))) %>%
#   group_by(trial,time,source) %>%
#   summarise(mean_effort = mean(effort),
#             mean_score = mean(score)) %>%
#       #      upper = quantile(value,0.975),
#       #      lower = quantile(value,0.025)) %>%
#   ggplot() +
#   #geom_ribbon(aes(ymin=lower,ymax=upper,x=time,group=source),fill="skyblue") +
#   geom_path(aes(y=mean_score,x=mean_effort,group=source,colour=source)) +
#   facet_grid(.~ trial,scale="free")
#
#
#
#
#
# %>%
#
#
#   summarise( = mean(predicted_goal_1),
#             Predicted_2 = mean(predicted_goal_2),
#             Observed = mean(observed_goal)) %>%
#   gather(key=Source,value=goal_mean,Predicted_1:Observed)
#
# #save results
# save(fit_sample,file="../output/1_sample_fit.RData")
#
# #get percentage of alpha distribution > 0
# mean((rstan::extract(fit_sample,'alpha')[[1]]>0))
#
# #get percentage of beta distribution > 0
# mean((rstan::extract(fit_sample,'beta')[[1]]>0))
#
# ##---------------------------------------------------------------------------
# # Model 2: Person-level Model
#
# #implement model
# fit_person = stan(file="../models/2_person_level_model.stan")
#
# #view summary of results
# fit_person
#
# #save results
# save(fit_person,file="../output/2_person_fit.RData")
#
# ##---------------------------------------------------------------------------
# # Model 3: Hierarchical Model
#
# #implement model
# fit_hier = stan(file="../models/3_hierarchical_model.stan")
#
# #view summary of results
# fit_hier
#
# #save results
# save(fit_hier,file="../output/3_hierarchical_fit.RData")
#
# ##---------------------------------------------------------------------------
# # Model 4: Multiple-group Model
#
# #implement model
# fit_multigroup = stan(file="../models/4_multiple_group_model.stan")
#
# #view summary of results
# fit_multigroup
#
# #save results
# save(fit_multigroup,file="../output/4_multigroup_fit.RData")
#
# #get alpha and beta parameters associated with each mixture
# alpha1=extract(fit_multigroup,'alpha')[[1]][,1] #alpha for mixture 1
# alpha2=extract(fit_multigroup,'alpha')[[1]][,2] #alpha for mixture 2
# beta1=extract(fit_multigroup,'beta')[[1]][,1] #beta for mixture 1
# beta2=extract(fit_multigroup,'beta')[[1]][,2] #beta for mixture 2
#
# #get upper and lower bounds on 95% CI on alpha difference and beta difference between conditions
# quantile(alpha1-alpha2,0.025) #lower bound of 95% CI on alpha difference
# quantile(alpha1-alpha2,0.975) #upper bound of 95% CI on alpha difference
# quantile(beta1-beta2,0.025) #lower bound of 95% CI on beta difference
# quantile(beta1-beta2,0.975) #lower bound of 95% CI on beta difference
#
# #get percentage of alpha difference distribution and beta difference distribution greater than 0
# #(or equivilantly, the percentage of the samples in which alpha for mixture 1 is greater than alpha for mixture 2, and likewise for beta)
# mean(alpha1>alpha2) #alpha difference
# mean(beta1>beta2) #beta difference
#
# ##---------------------------------------------------------------------------
# # Model 5: Two Mixture Model
#
# #implement model
# fit_two_mixture = stan(file="../models/5_two_mixture_model.stan")
#
# #view summary of results
# fit_two_mixture
#
# #view summary of results
# save(fit_two_mixture,file="../output/5_two_mixture_fit.RData")
#
#
# ##---------------------------------------------------------------------------
# # Model 6: Multiple Group Hierarchical Model (not presented in paper)
#
# #Create new object that contains a single element for each subject, indicating the condition of each subject
# subj_cond = condition[trial==1]
#
# #Implement model
# fit_multihiergroup = stan(file="../models/6_multiple_group_hierarchical_model.stan")
#
# #View model summary
# fit_multihiergroup
#
#
# ##---------------------------------------------------------------------------
# # Model 7: N-Mixture Model (not presented in paper)
#
# #Create new object containing a single element that sets the number of mixtures to model
# Nmix = 3
#
# #Implement model
# fit_n_mixture= stan(file="../models/7_n_mixture_model.stan")
#
# #View model summary
# fit_n_mixture
#
# #Note: The 3 mixture model doesn't actually converge well, which is often an indication that there are more mixtures specified by the model than in the data.
# traceplot(fit_n_mixture,c('alpha','beta'))
#
#
