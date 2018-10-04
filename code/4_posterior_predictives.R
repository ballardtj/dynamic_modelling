#NOTE: This script is used to create the figure that displays the posterior predictives
#for the two mixture model and the sample level model (which is equivilant to a one mixture model).

#clear workspace
rm(list=ls())

#load packages 
library(tidyverse)

#load data
dat=read.csv("../raw_data/goal_data.csv")

#prepare data for stan
load(file="../output/1_sample_fit.RData")
load(file="../output/5_two_mixture_fit.RData")

#extract parameters from fit object
parms_two_mixture =rstan::extract(fit_two_mixture)
parms_one_mixture =rstan::extract(fit_sample)

#Get mean mixture weight for each subject
mean_mix_weights = data.frame(subject=1:60,
                          mean_weight=apply(parms_two_mixture$mix_weight,2,mean))

#Join mean mix weights into data frame
dat2= left_join(dat,mean_mix_weights) %>%
  mutate(class = factor((mean_weight>0.5)*1,levels=c(1,0),labels=c('Mixture 1','Mixture 2')))

#Get 100 samples for posterior predictives
samples_used = sample(1:4000,size=100)
pp_list=list()

for(i in 1:100){
  pp_list[[i]]=dat2 %>% 
    mutate(predicted_goal_2 = parms_two_mixture$sampled_goal[samples_used[i],],
           predicted_goal_1 = parms_one_mixture$sampled_goal[samples_used[i],]) %>%
    group_by(class,condition,trial) %>%
    summarise(predicted_goal_1 = mean(predicted_goal_1),
              predicted_goal_2 = mean(predicted_goal_2),
              observed_goal = mean(goal),
              observed_goal_hi = observed_goal + sd(goal)/sqrt(length(goal)),
              observed_goal_lo = observed_goal - sd(goal)/sqrt(length(goal)))
}

#Get mean of goal posterior predictive distribution for each model
pp_means = bind_rows(pp_list) %>%
  ungroup() %>%
  mutate(condition = factor(condition,levels=c('approach','avoidance'),labels=c('Approach','Avoidance'))) %>%
  group_by(class,condition,trial) %>%
  summarise(Predicted_1 = mean(predicted_goal_1),
            Predicted_2 = mean(predicted_goal_2),
            Observed = mean(observed_goal)) %>%
  gather(key=Source,value=goal_mean,Predicted_1:Observed)

#Get upper and lower bound bound of 95% CI on posterior predictive distribution for each model
pp_CIs = bind_rows(pp_list) %>%
  ungroup() %>%
  mutate(condition = factor(condition,levels=c('approach','avoidance'),labels=c('Approach','Avoidance'))) %>%
  group_by(class,condition,trial) %>%
  summarise(predicted_goal_hi_1 = quantile(predicted_goal_1,0.975),
            predicted_goal_lo_1 = quantile(predicted_goal_1,0.025),
            predicted_goal_hi_2 = quantile(predicted_goal_2,0.975),
            predicted_goal_lo_2 = quantile(predicted_goal_2,0.025))

#Get standard error of observed mean goal for each trial
pp_SEs = bind_rows(pp_list) %>%
  ungroup() %>%
  mutate(condition = factor(condition,levels=c('approach','avoidance'),labels=c('Approach','Avoidance'))) %>%
  group_by(class,condition,trial) %>%
  summarise(observed_goal_hi = mean(observed_goal_hi),
            observed_goal_lo = mean(observed_goal_lo))

#Function to mimic ggplot default colour specification
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#Generate plot
pp_fig = ggplot(data=pp_CIs,aes(x=factor(trial))) +
  facet_grid(condition~class) +
  geom_ribbon(aes(ymin=predicted_goal_lo_1,ymax=predicted_goal_hi_1),alpha=0.1,fill=gg_color_hue(3)[[2]]) +
  geom_ribbon(aes(ymin=predicted_goal_lo_2,ymax=predicted_goal_hi_2),alpha=0.1,fill=gg_color_hue(3)[[3]]) +
  geom_point(data=subset(pp_means,Source=="Observed"),aes(y=goal_mean,group=Source,colour=Source)) +
  geom_line(data=subset(pp_means,Source!="Observed"),aes(y=goal_mean,group=Source,colour=Source)) +
  geom_errorbar(data=pp_SEs,aes(ymin=observed_goal_lo,ymax=observed_goal_hi,group=1),width=0.2,col=gg_color_hue(3)[1]) +
  theme_minimal() +
  scale_color_manual(labels=c('Observed','One-mixture Model','Two-mixture Model'),values=gg_color_hue(3)) +
  labs(x="Trial",y="Goal Level")

#save figure
ggsave(file="../figures/posterior_predictive_panel.pdf",plot=pp_fig,height=5,width=7)





