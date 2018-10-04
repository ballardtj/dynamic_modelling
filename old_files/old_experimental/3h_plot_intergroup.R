#clear workspace
rm(list=ls())

#fit models
#source("./experimental/3e_fit_univariate_multiple_group_stan.R")
#source("./experimental/3e_fit_univariate_mixture_stan.R")

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)
#library(patchwork)

#MULTIPLE GROUP MODEL
load("../model_output/stan_univariate_multiple_group_samples.RData")

parms=extract(fit_mg)

posteriors_mg_raw = data.frame(
  Condition = factor(c(rep(1,4000),rep(2,4000)),levels=1:2,labels=c('Approach','Avoidance')),
  alpha = c(parms$alpha[,1],parms$alpha[,2]),
  beta = c(parms$beta[,1], parms$beta[,2])
)

posteriors_mg_diff = data.frame(
  Parameter = factor(c(rep(1,4000),rep(2,4000)),levels=1:2,labels=c('Constant Change','Proportional Change')),
  Diff = c(parms$alpha[,1]-parms$alpha[,2],parms$beta[,1]- parms$beta[,2] )
)

posteriors_mg_diff %>% 
  group_by(Parameter) %>% 
  summarise(lower=quantile(Diff,0.025),
            upper=quantile(Diff,0.975))

plot_mg_alpha=ggplot(data=posteriors_mg_raw) +
  geom_density(aes(x=alpha,group=Condition,fill=Condition),alpha=0.15) +
  labs(x=expression(paste("Constant Change (",alpha,")")),y="Posterior Density",legend='Condition') +
  theme_minimal() + theme(legend.position = c(0.85, 0.8))

plot_mg_beta=ggplot(data=posteriors_mg_raw) +
  geom_density(aes(x=beta,group=Condition,fill=Condition),alpha=0.15) +
  labs(x=expression(paste("Proportional Change (",beta,")")),y="Posterior Density",legend='Condition') +
  theme_minimal() + theme(legend.position = c(0.85, 0.8))

plot_mg_diff=ggplot(data=posteriors_mg_diff) +
  geom_density(aes(x=Diff,group=Parameter,fill=Parameter),alpha=0.15) +
  labs(x="Group Difference",y="Posterior Density",legend='Condition') +
  scale_fill_manual(values=c("black", "white")) +
  theme_minimal() + theme(legend.position = c(0.78, 0.8))


#MIXTURE MODEL
load("../model_output/stan_univariate_mixture_samples.RData")

parms=extract(fit_mix)

posteriors_mix_raw = data.frame(
  Mixture = factor(c(rep(1,4000),rep(2,4000)),levels=1:2),
  alpha = c(parms$alpha[,1],parms$alpha[,2]),
  beta = c(parms$beta[,1], parms$beta[,2])
)

CI_mix_props = data.frame(
  subject = rep(1:60,each=4000),
  mix_weight = as.vector(parms$mix_weight)
) %>% group_by(subject) %>%
  summarise(lower_mix = quantile(mix_weight,0.025),
            mean_mix = mean(mix_weight),
            upper_mix = quantile(mix_weight,0.975)) %>%
  mutate(subject_mix = factor(subject,levels=subject[order(mean_mix)]))

plot_mix_alpha=ggplot(data=posteriors_mix_raw) +
  geom_density(aes(x=alpha,group=Mixture,fill=Mixture),alpha=0.15) +
  labs(x=expression(paste("Constant Change (",alpha,")")),y="Posterior Density") +
  scale_fill_manual(values=c("green", "orange")) +
  theme_minimal() + theme(legend.position = c(0.95, 0.8))

plot_mix_beta=ggplot(data=posteriors_mix_raw) +
  geom_density(aes(x=beta,group=Mixture,fill=Mixture),alpha=0.15) +
  labs(x=expression(paste("Proportional Change (",beta,")")),y="Posterior Density") +
  scale_fill_manual(values=c("green", "orange")) +
  theme_minimal() + theme(legend.position = c(0.95, 0.8))

plot_mix_CIs=ggplot(data=CI_mix_props) +
  geom_point(aes(y=subject_mix,x=mean_mix)) +
  geom_errorbarh(aes(y=subject_mix,xmin=lower_mix,xmax=upper_mix)) +
  labs(x="Mixture Weight",y="Subject") +
  theme_minimal() + theme(axis.text.y=element_blank())


mg_panel=grid.arrange(
  arrangeGrob(   
    arrangeGrob(
      plot_mg_alpha,
      plot_mg_beta,
      plot_mg_diff,
      nrow=1,
      top = textGrob("Known Group Model", gp=gpar(fontsize=15))
    ),
    arrangeGrob(
      plot_mix_alpha,
      plot_mix_beta,
      plot_mix_CIs,
      nrow=1,
      top = textGrob("Unknown Group Model", gp=gpar(fontsize=15))
    )
  )
)

# mg_panel=((plot_mg_alpha + labs(title='Multiple Group Model')) + plot_mg_beta +  plot_mg_diff) /
# ((plot_mix_alpha + labs(title='Mixture Model')) + plot_mix_beta +  plot_mix_CIs)  

ggsave(file="../figures/multiple_group_panel.pdf",plot=mg_panel,height=8,width=11)

