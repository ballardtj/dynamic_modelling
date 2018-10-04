#NOTE: This script is used to create the figure that displays the alpha and beta
#parameters from the sample-level, person-level, and hierarchical models.

#clear workspace
rm(list=ls())

#load packages (need tidyverse, grid, and gridExtra installed)
library(tidyverse)
library(rstan)
library(cowplot)
library(gridExtra)
library(patchwork)

#------------------------------------------------------
#SAMPLE LEVEL MODEL

#load model output
load("../output/1_sample_fit.RData")

#extract posterior samples
posteriors_sample=bind_rows(rstan::extract(fit_sample,pars=c('alpha','beta')))

#generate plots
plot_sample_alpha=ggplot(data=posteriors_sample) +
  geom_density(aes(x=alpha),fill="blue",alpha=0.15) +
  labs(x=expression(alpha),y="Posterior Density") +
  theme_minimal()

plot_sample_beta=ggplot(data=posteriors_sample) +
  geom_density(aes(x=beta),fill="blue",alpha=0.15) +
  labs(x=expression(beta) ,y="Posterior Density") +
  theme_minimal()

plot_sample_alphabeta=ggplot(data=posteriors_sample) +
  geom_density2d(aes(x=alpha,y=beta),colour="blue")+
  labs(x=expression(alpha) ,y=expression(beta)) +
  theme_minimal()

#------------------------------------------------------
#PERSON LEVEL MODEL

#load model output
load(file="../output/2_person_fit.RData")

#extract posterior samples
posteriors_person = bind_rows(rstan::extract(fit_person,pars=c('alpha','beta'))) %>%
  mutate(subject = rep(1:60,each=4000),
         sample = rep(1:4000,times=60))

#get bounds of 95% CI for each person
CIs_person = posteriors_person %>%
  group_by(subject) %>%
  summarise(lower_alpha = quantile(alpha,0.025),
            mean_alpha = mean(alpha),
            upper_alpha = quantile(alpha,0.975),
            lower_beta = quantile(beta,0.025),
            mean_beta = mean(beta),
            upper_beta = quantile(beta,0.975)) %>%
  mutate(subject_alpha = factor(subject,levels=subject[order(mean_alpha)]),
         subject_beta = factor(subject,levels=subject[order(mean_beta)]))
            
#generate plots
plot_person_alpha=ggplot(data=CIs_person) +
  geom_point(aes(y=subject_alpha,x=mean_alpha)) +
  geom_errorbarh(aes(y=subject_alpha,xmin=lower_alpha,xmax=upper_alpha),colour="red") +
  labs(x=expression(alpha) ,y='Subject') +
  theme_minimal() + theme(axis.text.y=element_blank())
 
plot_person_beta=ggplot(data=CIs_person) +
  geom_point(aes(y=subject_beta,x=mean_beta)) +
  geom_errorbarh(aes(y=subject_beta,xmin=lower_beta,xmax=upper_beta),colour="red") +
  labs(x=expression(beta),y="Subject") +
  theme_minimal() + theme(axis.text.y=element_blank())

plot_person_alphabeta=ggplot(data=CIs_person) +
  geom_point(aes(x=mean_alpha,y=mean_beta)) +
  geom_errorbarh(aes(y=mean_beta,xmin=lower_alpha,xmax=upper_alpha),color="red",alpha=0.2) +
  geom_errorbar(aes(x=mean_alpha,ymin=lower_beta,ymax=upper_beta),color="red",alpha=0.2) +
  labs(x=expression(alpha) ,y=expression(beta)) +
  theme_minimal()

#------------------------------------------------------
#HIERARCHICAL MODEL

#load data
load(file="../output/3_hierarchical_fit.RData")

#extract posterior samples for person-level parameters
posteriors_hier_person = bind_rows(rstan::extract(fit_hier,pars=c('alpha','beta'))) %>%
  mutate(subject = rep(1:60,each=4000),
         sample = rep(1:4000,times=60))

#extract posterior samples for population parameters
posteriors_hier_pop = bind_rows(rstan::extract(fit_hier,pars=c('alpha_mean','beta_mean'))) %>%
  mutate(sample = 1:4000)

#get bounds of 95% CI for each person
CIs_hier = posteriors_hier_person %>%
  group_by(subject) %>%
  summarise(lower_alpha = quantile(alpha,0.025),
            mean_alpha = mean(alpha),
            upper_alpha = quantile(alpha,0.975),
            lower_beta = quantile(beta,0.025),
            mean_beta = mean(beta),
            upper_beta = quantile(beta,0.975)) %>%
  mutate(subject_alpha = factor(subject,levels=subject[order(mean_alpha)]),
         subject_beta = factor(subject,levels=subject[order(mean_beta)]))

#generate plots
plot_hier_alpha=ggplot(data=CIs_hier) +
  geom_point(aes(y=subject_alpha,x=mean_alpha)) +
  geom_errorbarh(aes(y=subject_alpha,xmin=lower_alpha,xmax=upper_alpha),color="red") +
  geom_density(data=posteriors_hier_pop,aes(x=alpha_mean,y=..density..*4.5),fill="blue",alpha=0.15) +
  labs(x= expression(alpha) ,y="Subject") +
  theme_minimal() + theme(axis.text.y=element_blank())

plot_hier_beta=ggplot(data=CIs_hier) +
  geom_point(aes(y=subject_beta,x=mean_beta)) +
  geom_errorbarh(aes(y=subject_beta,xmin=lower_beta,xmax=upper_beta),color="red") +
  geom_density(data=posteriors_hier_pop,aes(x=beta_mean,y=..density..*10),fill="blue",alpha=0.15) +
  labs(x=expression(beta),y="Subject") +
  theme_minimal() + theme(axis.text.y=element_blank())

plot_hier_alphabeta = ggplot(data=CIs_hier) +
    geom_density2d(data=posteriors_hier_pop,aes(x=alpha_mean,y=beta_mean),colour='blue') +
    geom_point(aes(x=mean_alpha,y=mean_beta)) +
    geom_errorbarh(aes(y=mean_beta,xmin=lower_alpha,xmax=upper_alpha),color="red",alpha=0.15) +
    geom_errorbar(aes(x=mean_alpha,ymin=lower_beta,ymax=upper_beta),color="red",alpha=0.15) +
    labs(x=expression(alpha),y=expression(beta)) +
    theme_minimal()


#------------------------------------------------------
#COMBINE INDIVIDUAL PLOTS TO CREATE PANEL FIGURE

row1 = plot_sample_alpha + (plot_sample_beta+ labs(title = "Sample-level Model")) + plot_sample_alphabeta
row2 = (plot_person_alpha + (plot_person_beta+ labs(title = "Person-level Model")) + plot_person_alphabeta)
row3 = (plot_hier_alpha + (plot_hier_beta+ labs(title = "Hierarchical Model")) + plot_hier_alphabeta)

sg_panel=(row1/row2)/row3

#save figure
ggsave(file="../figures/single_group_panel.pdf",plot=sg_panel,height=12,width=8)