#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(reshape2)

load(file="../clean_data/wide_data_v4.RData")

#prepare data for stan
perf_vars = names(wide_data)[grep('perf',names(wide_data))]
diff_vars = names(wide_data)[grep('diff',names(wide_data))]

Nsubj = dim(wide_data)[1]
Nobs = length(perf_vars)
perf = as.matrix( wide_data[,perf_vars])
diff = as.matrix( wide_data[,diff_vars])

dimnames(perf)<-list(subj=1:Nsubj,obs=1:Nobs)
perf_frame = melt(perf) %>% mutate(perf=value) %>% select(-value)

dimnames(diff)<-list(subj=1:Nsubj,obs=1:Nobs)
diff_frame = melt(diff) %>% mutate(diff=value) %>% select(-value)

dat = left_join(perf_frame,diff_frame)

#posterior predictives - bivariate group level model

#get parameters for bivariate level model
#load("../model_output/stan_bivariate_group_samples.RData")
load("../model_output/stan_exp_learning_group_samples.RData")


#pp_biv_tmp = extract(fit_biv,"predicted_perf")$predicted_perf
pp_biv_tmp = extract(fit_expl,"pred_perf")$pred_perf

dimnames(pp_biv_tmp)<-list(sample=1:4000,subj=1:Nsubj,obs=1:Nobs)
pp_biv=melt(pp_biv_tmp)

bin_size = 5 #minutes

pp_pred=pp_biv %>%
  mutate(obs_bin = ceiling(obs/bin_size)*bin_size) %>%
  group_by(sample,obs_bin) %>%
  summarise(perf_pred_sample_mean=mean(value)) %>%
  group_by(obs_bin) %>%
  summarise(perf_pred_mean = mean(perf_pred_sample_mean),
            perf_pred_lower = quantile(perf_pred_sample_mean,0.025),
            perf_pred_upper = quantile(perf_pred_sample_mean,0.975))

pp_obs=perf_frame %>%
  mutate(obs_bin = ceiling(obs/bin_size)*bin_size) %>%
  group_by(obs_bin) %>%
  summarise(perf_obs_mean = mean(perf),
            perf_obs_lower = perf_obs_mean - sd(perf)/sqrt(length(perf)),
            perf_obs_upper = perf_obs_mean + sd(perf)/sqrt(length(perf)))

perf_pp_plot = ggplot(data=pp_pred,aes(x=obs_bin)) +
  geom_ribbon(aes(ymin=perf_pred_lower,ymax=perf_pred_upper),fill="gray") +
  geom_line(aes(y=perf_pred_mean),colour='blue') +
  geom_point(data=pp_obs,aes(x=obs_bin,y=perf_obs_mean)) +
  geom_errorbar(data=pp_obs,aes(obs_bin,ymin=perf_obs_lower,ymax=perf_obs_upper),width=0.1) +
  theme_minimal() +
  labs(y="Performance",x="Time")



#pp_biv_tmp = extract(fit_biv,"predicted_diff")$predicted_diff

pp_biv_tmp = extract(fit_expl,"pred_diff")$pred_diff

dimnames(pp_biv_tmp)<-list(sample=1:4000,subj=1:Nsubj,obs=1:Nobs)
pp_biv=melt(pp_biv_tmp)

pp_pred=pp_biv %>%
  mutate(obs_bin = ceiling(obs/bin_size)*bin_size) %>%
  group_by(sample,obs_bin) %>%
  summarise(perf_pred_sample_mean=mean(value)) %>%
  group_by(obs_bin) %>%
  summarise(perf_pred_mean = mean(perf_pred_sample_mean),
            perf_pred_lower = quantile(perf_pred_sample_mean,0.025),
            perf_pred_upper = quantile(perf_pred_sample_mean,0.975))

pp_obs=diff_frame %>%
  mutate(obs_bin = ceiling(obs/bin_size)*bin_size) %>%
  group_by(obs_bin) %>%
  summarise(perf_obs_mean = mean(diff),
            perf_obs_lower = perf_obs_mean - sd(diff)/sqrt(length(diff)),
            perf_obs_upper = perf_obs_mean + sd(diff)/sqrt(length(diff)))

diff_pp_plot = ggplot(data=pp_pred,aes(x=obs_bin)) +
  geom_ribbon(aes(ymin=perf_pred_lower,ymax=perf_pred_upper),fill="gray") +
  geom_line(aes(y=perf_pred_mean),colour='blue') +
  geom_point(data=pp_obs,aes(x=obs_bin,y=perf_obs_mean)) +
  geom_errorbar(data=pp_obs,aes(obs_bin,ymin=perf_obs_lower,ymax=perf_obs_upper),width=0.1) +
  theme_minimal() +
  labs(y="Percieved Difficulty",x="Time")


pp_panel=grid.arrange(
  arrangeGrob(
    perf_pp_plot,
    diff_pp_plot,
    nrow=1)
)

ggsave(file="../figures/posterior_predictive_panel.pdf",plot=pp_panel,height=7,width=8)


 