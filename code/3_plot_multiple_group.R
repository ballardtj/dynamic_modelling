#NOTE: This script is used to create the figure that displays the alpha and beta
#parameters from the multiple-group models

#clear workspace
rm(list=ls())

library(tidyverse)
library(rstan)
library(patchwork)

#------------------------------------------------------
#MULTIPLE GROUP MODEL
load("../output/4_multigroup_fit.RData")

#load model output
parms=rstan::extract(fit_multigroup)

#get posteriors samples for alpha and beta parameters for each condition
posteriors_mg_raw = data.frame(
  Condition = factor(c(rep(1,4000),rep(2,4000)),levels=1:2,labels=c('Approach','Avoidance')),
  alpha = c(parms$alpha[,1],parms$alpha[,2]),
  beta = c(parms$beta[,1], parms$beta[,2])
)

#get posterior samples for the difference between the two conditions in alpha and beta
posteriors_mg_diff = data.frame(
  Parameter = factor(c(rep(1,4000),rep(2,4000)),levels=1:2,labels=c(expression(alpha),expression(beta))),
  Diff = c(parms$alpha[,1]-parms$alpha[,2],parms$beta[,1]- parms$beta[,2] )
)

#generate plots
plot_mg_alpha=ggplot(data=posteriors_mg_raw) +
  geom_density(aes(x=alpha,group=Condition,fill=Condition),alpha=0.15) +
  #labs(x=expression(paste("Constant Change (",alpha,")")),y="Posterior Density",legend='Condition') +
  labs(x=expression(alpha),y="Posterior Density",legend='Condition') +
  coord_cartesian(ylim=c(0,12)) +
  theme_minimal() + theme(legend.position = c(0.85, 0.85))

plot_mg_beta=ggplot(data=posteriors_mg_raw) +
  geom_density(aes(x=beta,group=Condition,fill=Condition),alpha=0.15) +
  #labs(x=expression(paste("Proportional Change (",beta,")")),y="Posterior Density",legend='Condition') +
  labs(x=expression(beta),y="Posterior Density",legend='Condition') +
  coord_cartesian(ylim=c(0,12)) +
  theme_minimal() + theme(legend.position = c(0.85, 0.85))

plot_mg_diff=ggplot(data=posteriors_mg_diff) +
  geom_density(aes(x=Diff,group=Parameter,fill=Parameter),alpha=0.15) +
  labs(x="Group Difference",y="Posterior Density",legend='Condition') +
  scale_fill_manual(values=c("black", "white"),labels=c(expression(alpha),expression(beta))) +
  theme_minimal() + theme(legend.position = c(0.78, 0.85))


#------------------------------------------------------
#MIXTURE MODEL

#load model output
load("../output/5_two_mixture_fit.RData")

#extract posterior samples
parms=rstan::extract(fit_two_mixture)

#get posteriors samples for alpha and beta parameters for each mixture
posteriors_mix_raw = data.frame(
  Mixture = factor(c(rep(1,4000),rep(2,4000)),levels=1:2),
  alpha = c(parms$alpha[,1],parms$alpha[,2]),
  beta = c(parms$beta[,1], parms$beta[,2])
)

#get the mean mixture weight and the upper and lower bounds on 95% CI for each participant
CI_mix_props = data.frame(
  subject = rep(1:60,each=4000),
  mix_weight = as.vector(parms$mix_weight)
) %>% group_by(subject) %>%
  summarise(lower_mix = quantile(mix_weight,0.025),
            mean_mix = mean(mix_weight),
            upper_mix = quantile(mix_weight,0.975)) %>%
  mutate(subject_mix = factor(subject,levels=subject[order(mean_mix)]))

#generate plots
plot_mix_alpha=ggplot(data=posteriors_mix_raw) +
  geom_density(aes(x=alpha,group=Mixture,fill=Mixture),alpha=0.15) +
  labs(x=expression(alpha),y="Posterior Density") +
  scale_fill_manual(values=c("green", "orange")) +
  theme_minimal() + theme(legend.position = c(0.9, 0.85))

plot_mix_beta=ggplot(data=posteriors_mix_raw) +
  geom_density(aes(x=beta,group=Mixture,fill=Mixture),alpha=0.15) +
  labs(x=expression(beta),y="Posterior Density") +
  scale_fill_manual(values=c("green", "orange")) +
  theme_minimal() + theme(legend.position = c(0.9, 0.85))

plot_mix_CIs=ggplot(data=CI_mix_props) +
  geom_point(aes(y=subject_mix,x=mean_mix)) +
  geom_errorbarh(aes(y=subject_mix,xmin=lower_mix,xmax=upper_mix)) +
  labs(x="Mixture Weight",y="Subject") +
  theme_minimal() + theme(axis.text.y=element_blank())

#------------------------------------------------------
#COMBINE INDIVIDUAL PLOTS TO CREATE PANEL FIGURE

row1=plot_mg_alpha + (plot_mg_beta+ labs(title = "Known Group Model")) + plot_mg_diff
row2=plot_mix_alpha + (plot_mix_beta+ labs(title = "Unknown Group Model")) + plot_mix_CIs

mg_panel=row1/row2

#save figure
ggsave(file="../figures/multiple_group_panel.pdf",plot=mg_panel,height=8,width=11)

