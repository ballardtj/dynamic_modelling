#clear workspace
rm(list=ls())

#fit models
#source("./experimental/3b_fit_univariate_stan.R")
#source("./experimental/3c_fit_univariate_subject_stan.R")
#source("./experimental/3d_fit_univariate_hierarchical_stan.R")

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)
library(grid)
library(gridExtra)

#GROUP LEVEL MODEL
load("../model_output/stan_univariate_group_samples.RData")

posteriors_group=bind_rows(extract(fit_group))

plot_group_alpha=ggplot(data=posteriors_group) +
  geom_density(aes(x=alpha),fill="blue",alpha=0.15) +
  labs(x=expression(paste("Constant Change (",alpha,")")) ,y="Posterior Density") +
  theme_minimal()

plot_group_beta=ggplot(data=posteriors_group) +
  geom_density(aes(x=beta),fill="blue",alpha=0.15) +
  labs(x=expression(paste("Proportional Change (",beta,")")) ,y="Posterior Density") +
  theme_minimal()

plot_group_corr=ggplot(data=posteriors_group) +
  geom_density2d(aes(x=alpha,y=beta),colour="blue")+
  labs(x=expression(paste("Constant Change (",alpha,")")) ,y=expression(paste("Proportional Change (",beta,")"))) +
  theme_minimal()

#PERSON LEVEL MODEL
load(file="../model_output/stan_univariate_subject_samples.RData")

posteriors_subject = bind_rows(extract(fit_subj,pars=c('alpha','beta'))) %>%
  mutate(subject = rep(1:60,each=4000),
         sample = rep(1:4000,times=60))

CIs_subject = posteriors_subject %>%
  group_by(subject) %>%
  summarise(lower_alpha = quantile(alpha,0.025),
            mean_alpha = mean(alpha),
            upper_alpha = quantile(alpha,0.975),
            lower_beta = quantile(beta,0.025),
            mean_beta = mean(beta),
            upper_beta = quantile(beta,0.975)) %>%
  mutate(subject_alpha = factor(subject,levels=subject[order(mean_alpha)]),
         subject_beta = factor(subject,levels=subject[order(mean_beta)]))
            

plot_subject_alpha=ggplot(data=CIs_subject) +
  geom_point(aes(y=subject_alpha,x=mean_alpha)) +
  geom_errorbarh(aes(y=subject_alpha,xmin=lower_alpha,xmax=upper_alpha),colour="red") +
  labs(x=expression(paste("Constant Change (",alpha,")")) ,y='Subject') +
  theme_minimal() + theme(axis.text.y=element_blank())
 
plot_subject_beta=ggplot(data=CIs_subject) +
  geom_point(aes(y=subject_beta,x=mean_beta)) +
  geom_errorbarh(aes(y=subject_beta,xmin=lower_beta,xmax=upper_beta),colour="red") +
  labs(x=expression(paste("Proportional Change (",beta,")")),y="Subject") +
  theme_minimal() + theme(axis.text.y=element_blank())

plot_subject_corr=ggplot(data=CIs_subject) +
  geom_point(aes(x=mean_alpha,y=mean_beta)) +
  geom_errorbarh(aes(y=mean_beta,xmin=lower_alpha,xmax=upper_alpha),color="red",alpha=0.2) +
  geom_errorbar(aes(x=mean_alpha,ymin=lower_beta,ymax=upper_beta),color="red",alpha=0.2) +
  labs(x=expression(paste("Constant Change (",alpha,")")) ,y=expression(paste("Proportional Change (",beta,")"))) +
  theme_minimal()

#HIERARCHICAL MODEL
load(file="../model_output/stan_univariate_hierarchical_samples.RData")

posteriors_hier_subject = bind_rows(extract(fit_hier,pars=c('alpha','beta'))) %>%
  mutate(subject = rep(1:60,each=4000),
         sample = rep(1:4000,times=60))

posteriors_hier_group = bind_rows(extract(fit_hier,pars=c('alpha_mean','beta_mean'))) %>%
  mutate(sample = 1:4000)

CIs_hier = posteriors_hier_subject %>%
  group_by(subject) %>%
  summarise(lower_alpha = quantile(alpha,0.025),
            mean_alpha = mean(alpha),
            upper_alpha = quantile(alpha,0.975),
            lower_beta = quantile(beta,0.025),
            mean_beta = mean(beta),
            upper_beta = quantile(beta,0.975)) %>%
  mutate(subject_alpha = factor(subject,levels=subject[order(mean_alpha)]),
         subject_beta = factor(subject,levels=subject[order(mean_beta)]))


plot_hier_alpha=ggplot(data=CIs_hier) +
  geom_point(aes(y=subject_alpha,x=mean_alpha)) +
  geom_errorbarh(aes(y=subject_alpha,xmin=lower_alpha,xmax=upper_alpha),color="red") +
  geom_density(data=posteriors_hier_group,aes(x=alpha_mean,y=..density..*5),fill="blue",alpha=0.15) +
  labs(x= expression(paste("Constant Change (",alpha,")")) ,y="Subject") +
  theme_minimal() + theme(axis.text.y=element_blank())

plot_hier_beta=ggplot(data=CIs_hier) +
  geom_point(aes(y=subject_beta,x=mean_beta)) +
  geom_errorbarh(aes(y=subject_beta,xmin=lower_beta,xmax=upper_beta),color="red") +
  geom_density(data=posteriors_hier_group,aes(x=beta_mean,y=..density..*2),fill="blue",alpha=0.15) +
  labs(x=expression(paste("Proportional Change (",beta,")")),y="Subject") +
  theme_minimal() + theme(axis.text.y=element_blank())

plot_hier_corr = ggplot(data=CIs_hier) +
    geom_density2d(data=posteriors_hier_group,aes(x=alpha_mean,y=beta_mean),colour='blue') +
    geom_point(aes(x=mean_alpha,y=mean_beta)) +
    geom_errorbarh(aes(y=mean_beta,xmin=lower_alpha,xmax=upper_alpha),color="red",alpha=0.15) +
    geom_errorbar(aes(x=mean_alpha,ymin=lower_beta,ymax=upper_beta),color="red",alpha=0.15) +
    labs(x=expression(paste("Constant Change (",alpha,")")),y=expression(paste("Proportional Change (",beta,")"))) +
    theme_minimal()


# sg_panel=((plot_group_alpha + labs(title='Group Level Model')) + plot_group_beta -  plot_group_corr) / "TITLE" /
# ((plot_subject_alpha + labs(title='Person Level Model')) + plot_subject_beta -  plot_subject_corr)  /
# ((plot_hier_alpha + labs(title='Hierarchical Model')) + plot_hier_beta - plot_hier_corr) 
# 


sg_panel=grid.arrange(
  arrangeGrob(   
    arrangeGrob(
      arrangeGrob(plot_group_alpha,plot_group_beta,nrow=1),
      plot_group_corr,
      nrow=1,
      top = textGrob("Group Level Model", gp=gpar(fontsize=15))
    ),
    arrangeGrob(
      arrangeGrob(plot_subject_alpha,plot_subject_beta,nrow=1),
      plot_subject_corr,
      nrow=1,
      top = textGrob("Person Level Model", gp=gpar(fontsize=15))
    ),
    arrangeGrob(
      arrangeGrob(plot_hier_alpha,plot_hier_beta,nrow=1),
      plot_hier_corr,
      nrow=1,
      top=textGrob("Hierarchical Model", gp=gpar(fontsize=15))
    )
  )
)

ggsave(file="../figures/single_group_panel.pdf",plot=sg_panel,height=12,width=8)


