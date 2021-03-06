
#NOTE: This script is used to implement the Bayesian change models presented
#in Ballard, Palada, Griffin, and Neal (2018). The files for the models themselves
#are located in the 'models' folder.

#start a fresh R session (note this command only works in Rstudio)
rm(list=ls())

#load rtan
library(rstan) #for rstan installation instructions, see https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(tidyverse)
library(tidybayes)
library(shinystan)
#make sure working directory is set to the "analysis" folder

#load data
load("data/clean/goal_data.RData")

data =  mutate(data,obs = 1:n())#only approach for now
stan_list = as.list(data)
#stan_list$subject = ceiling (stan_list$subject / 2)
stan_list$Ntotal = nrow(data)
stan_list$practice = (stan_list$trial-1)*5 + stan_list$time

stan_list$Nglobal_trial = length(unique(data$subject))*length(unique(data$trial))
stan_list$goal = data %>% group_by(subject,trial) %>% summarise(goal = mean(goal)) %>% pull(goal)
stan_list$global_trial_number = data %>%
  group_by(time) %>%
  mutate(global_trial_number = 1:n()) %>%
  ungroup() %>%
  arrange(subject,trial) %>% pull(global_trial_number)


# practice = 1:50
# delta = 0.07
# skill_change = 13.5 #8.58 #4.92, 13.5
# skill_min = 11.02 #7.09 #3.98, 11.02
# skill_max = skill_min + skill_change
#
# skill = skill_max - (skill_max - skill_min)*(exp(-delta*practice))
# plot(practice,skill,col="blue")
#
# effort = seq(0,10,0.1)
# skill = 2.23 + 2.47
# kappa = 14.80  #3.29, 6.68, 14.80
# gamma = 6.52  #6.30, 6.52, 6.76
# performance = skill / (1 + exp(-kappa*(effort-gamma)))
# plot(effort,performance,ylim=c(0,10))


#predicted_performance = (gain23 + gain22*predicted_ability[i]) /
#(1 + exp(-(gain20 + gain24*predicted_ability[i] + gain21*predicted_effort[i]  ) )); //


# stan_list$Nsubj = length(unique(data$subject))
# stan_list$Ntrial = length(unique(data$trial))



#stan_list$time = stan_list$time/60
#stan_list$goal = data %>% group_by(subject,trial) %>% summarise(goal = mean(goal)) %>% pull(goal)
#stan_list$trial_index = (stan_list$subject-1)*5 + stan_list$trial
#stan_list$goal = stan_list$goal / 20
#stan_list$score = stan_list$score / 20

data = data %>%
  mutate(change_in_score = score-lag(score),
         change_in_score = if_else(time==1,score,change_in_score),
         change_in_effort = effort-lag(effort),
         change_in_effort = if_else(time==1,effort,change_in_effort))


##---------------
# Multivariate regression
# library(rstanarm)
#
# f1 <- stan_mvmer(
#   formula = list(
#     score ~ effort + (1 | subject),
#     effort ~ goal + (1 | subject)),
#   data = data,
#   # this next line is only to keep the example small in size!
#   chains = 1, cores = 1, seed = 12345, iter = 1000)
# summary(f1)
#
# names(f1)
#
# stancode <- rstan::get_stancode(f1$stanfit)
# cat(stancode)

##---------------------------------------------------------------------------
# Model 1: Bottom-up sample-level Model

for(m in 28:29){

#25 hypothesised model
#26 linear effort
#27 linear skill
#28 linear performance
#29 linear goal

#m = 29

#implement model
fit_fb_sample = stan(file=paste0("models/r2_2_feedback_model_change_v",m,".stan"),
                     #file="models/r2_1_feedback_model_same_variance.stan",
                     data=stan_list,
                     cores=4,
                     chains=4,
                     init_r = 1,
                     #iter=1,
                     #refresh=10,
                     control=list(adapt_delta=0.99,max_treedepth=20))

#view summary of results
fit_fb_sample

save(fit_fb_sample,file=paste0("data/derived/fit_fb_sample_v",m,".RData"))

}

for(m in 28:29){

load(file=paste0("data/derived/fit_fb_sample_v",m,".RData"))

pars = names(fit_fb_sample)[!(str_detect(names(fit_fb_sample),'sampled')|str_detect(names(fit_fb_sample),'predicted')|str_detect(names(fit_fb_sample),'outcome')|str_detect(names(fit_fb_sample),'gpd'))]

smry = summary(fit_fb_sample)[[1]]
smry = smry[rownames(smry) %in% pars,]

round(smry[c('effort_baseline','alpha','beta','effort_0','skill_min',
             'skill_max','delta','kappa','gamma','theta','lambda','goal_0',
             'sigma_effort_0','sigma_effort_change','sigma_performance_change',
             'sigma_goal_0','sigma_goal_change'),],3)

write.csv(smry,file=paste0("results_v",m,".csv"))


traceplot(fit_fb_sample,pars=pars)
ggsave(file=paste0("figures/trace_v",m,".pdf"),height=10,width=12)

pdf(file=paste0("figures/pairs_v",m,".pdf"),height=10,width=12)
pairs(fit_fb_sample,pars=pars)
dev.off()



# mean  se_mean       sd       2.5%       25%      50%
# alpha_int                  -0.56     0.42     0.91      -1.67     -1.24    -0.95
# alpha_slope                 0.91     0.47     0.76       0.06      0.21     0.61
# beta_int                    1.88     1.47     2.12       0.04      0.54     0.63
# beta_slope                  0.51     0.21     0.35       0.04      0.17     0.45
# eff_0                       4.11     2.10     2.97       0.30      1.68     4.31
# eff_int                     0.12     0.74     1.05      -1.01     -0.50    -0.17
# gain1                       0.36     0.30     0.42       0.00      0.01     0.20




# data = dim(samples$sampled_goal)

samples =rstan::extract(fit_fb_sample,permuted=T)

#Get 100 samples for posterior predictives
samples_used = sample(1:1000,size=100)
pp_list=list()

# variable='goal'
# obs=1
# chain=2
# iter=5

# get_sample=function(samples,obs,variable,chain,iter){
#   variable_name = paste0('sampled_',variable,'[',obs,']')
#   val = samples[,,variable_name][iter,chain]
#   #print(obs)
#   return(val)
# }
ctr=0
for(i in 1:100){
  #print(i)
 # for(c in 1:4){
    ctr=ctr+1
    pp_list[[ctr]]= data %>%
     # rowwise() %>%
      mutate(predicted_goal = samples$sampled_goal[samples_used[i],], #get_sample(samples,obs,variable='goal',chain=c,iter=samples_used[i]),
             predicted_effort = samples$sampled_effort[samples_used[i],],
             predicted_performance = samples$sampled_performance[samples_used[i],],
            # predicted_change_in_effort = samples$effort_outcome[samples_used[i],],
            # predicted_change_in_score = samples$score_outcome[samples_used[i],],
             predicted_skill = samples$predicted_skill[samples_used[i],]
           #predicted_alpha = samples$predicted_alpha[samples_used[i],],
           #predicted_beta = samples$predicted_beta[samples_used[i],],
          # predicted_effort = get_sample(samples,obs,variable='effort',chain=c,iter=samples_used[i]),
           #predicted_score = get_sample(samples,obs,variable='score',chain=c,iter=samples_used[i])
          ) %>%
      group_by(trial,time) %>%
      summarise(iter = samples_used[i],
              predicted_goal = mean(predicted_goal),
              predicted_skill = mean(predicted_skill),
              #predicted_alpha = mean(predicted_alpha),
              #predicted_beta = mean(predicted_beta),
              predicted_effort = mean(predicted_effort),
              predicted_performance = mean(predicted_performance),
              #predicted_changeineffort  = mean(predicted_change_in_effort),
              #predicted_changeinscore = mean(predicted_change_in_score),
              observed_goal = mean(goal),
              observed_effort = mean(effort),
              observed_performance = mean(performance)
              #observed_changeineffort = mean(change_in_effort),
              #observed_changeinscore = mean(change_in_score)
              )

}




pd = bind_rows(pp_list) %>%
  mutate(predicted_skill = predicted_skill) %>%
  gather(key=key,value=value,predicted_goal:observed_performance) %>%
  separate(col=key,into=c('source','variable')) %>%
  #mutate(condition = factor(condition,levels=c('approach','avoidance'),labels=c('Approach','Avoidance'))) %>%
  group_by(trial,time,source,variable) %>%
  summarise(mean = mean(value),
            upper = quantile(value,0.975),
            lower = quantile(value,0.025)) %>%
  ungroup() %>%
  filter(variable!="skill") %>%
  mutate(variable = factor(variable,levels=c('effort','performance','goal'),labels=c('Effort','Performance','Goal')),
         source = factor(source,level=c('observed','predicted'),labels=c('Observed','Predicted')))

ggplot(pd) +
  geom_ribbon(aes(ymin=lower,ymax=upper,x=time,group=source),fill="skyblue") +
  geom_line(aes(y=mean,x=time,group=factor(source),colour=factor(source))) +
  #geom_line(data=subset(pd,source=="predicted"),aes(y=mean,x=time,group=factor(chain),colour=factor(chain))) +
  #geom_line(data=subset(pd,source=="observed"),aes(y=mean,x=time,group=1)) +
  facet_grid(variable ~ trial,scale="free") +
  coord_cartesian(ylim=c(0,10)) +
  labs(x="Time",y="Level",color="Source")

#ggsave(file=paste0("figures/pp_v",m,".pdf"),height=6,width=8)

ggsave(file=paste0("figures/posterior_predictives_v",m,".pdf"),height=6,width=8)

}


smry = summary(fit_fb_sample)
smry$summary[pars,]
smry$c_summary[pars,,]
#1 and 3 good, #2 and 4 bad

launch_shinystan(fit_fb_sample)

###tidy bayes
tidyfit = tidy_draws(fit_fb_sample) %>%
  select(-contains('sampled')) %>%
  gather_draws()




tidyfit = fit_fb_sample %>%
  gather_draws(predicted_goal[obs],predicted_effort[obs],predicted_score[obs])







bind_rows(pp_list) %>%
  gather(key=key,value=value,predicted_effort:observed_score) %>%
  separate(col=key,into=c('source','variable')) %>%
  spread(key=variable,value=value) %>%
  #mutate(condition = factor(condition,levels=c('approach','avoidance'),labels=c('Approach','Avoidance'))) %>%
  group_by(trial,time,source) %>%
  summarise(mean_effort = mean(effort),
            mean_score = mean(score)) %>%
      #      upper = quantile(value,0.975),
      #      lower = quantile(value,0.025)) %>%
  ggplot() +
  #geom_ribbon(aes(ymin=lower,ymax=upper,x=time,group=source),fill="skyblue") +
  geom_path(aes(y=mean_score,x=mean_effort,group=source,colour=source)) +
  facet_grid(.~ trial,scale="free")





%>%


  summarise( = mean(predicted_goal_1),
            Predicted_2 = mean(predicted_goal_2),
            Observed = mean(observed_goal)) %>%
  gather(key=Source,value=goal_mean,Predicted_1:Observed)

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


