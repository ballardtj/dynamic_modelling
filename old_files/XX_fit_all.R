#clear workspace
rm(list=ls())

#load packages
library(rstan)
library(reshape)
library(tidyverse)
#load data
load(file="../clean_data/example_data.RData")

#create variables required for model in R environment
effort=dat$eff
discrepancy=dat$disc
demands=dat$demand
time=dat$time
init_discrepancy=dat$init_disc
subject=dat$subject
condition=as.numeric(dat$condition)
Nsubj=length(unique(dat$subject))
Ntotal=nrow(dat)

#Fit discrepancy model
fit_disc = stan(file="./models/1_disc_model.stan",cores=4)

#Fit task demands model
fit_demand = stan(file="./models/2_demand_model.stan",cores=4)

#Fit mixture model model
fit_mixture = stan(file="./models/3_mixture_model.stan",refresh=10,cores=4)

#Fit three mixture model
fit_3_mixture = stan(file="./models/4_three_mixture_model.stan",refresh=10,cores=4)

save(fit_3_mixture,file="~/Desktop/fit_3_mixture.RData")


#Fit two mixture model with temporal motivation and worklakd
fit_2_mixture_tm_wl = stan(file="./models/5_two_mixture_tm_wl.stan",refresh=10,cores=4)
save(fit_2_mixture_tm_wl,file="~/Desktop/fit_2_mixture_tm_wl.RData")

#Fit two mixture model with temporal motivation and worklakd (fully crossed)
fit_2_mixture_tm_wl_full = stan(file="./models/5_two_mixture_tm_wl_full.stan",refresh=10,cores=4)
save(fit_2_mixture_tm_wl_full,file="~/Desktop/fit_2_mixture_tm_wl_full.RData")

parms=rstan::extract(fit_2_mixture_tm_wl_full)

#Get mixture weights
mix_weights=melt(parms$mix_weight)
names(mix_weights) <- c('sample','subject','weight')

mean_weights  = mix_weights %>%
  group_by(subject) %>%
  summarise(mean_weight = mean(weight))

dat = left_join(dat,mean_weights) %>%
  mutate(class = factor((mean_weight>0.5)*1,levels=c(0,1),labels=c('Task Demands','Temporal Motivation'))) #1 if task demands, 2 if discrepancy

samples_used = sample(1:4000,size=100)
pp_list=list()


for(i in 1:100){
  
  dat$predicted_effort1 = parms$predicted_effort[samples_used[i],,1]
  dat$predicted_effort2 = parms$predicted_effort[samples_used[i],,2]
  #dat$predicted_effort3 = parms$predicted_effort[samples_used[i],,3]
  
  weights = data.frame(subject=1:60,
                       weight=parms$mix_weight[samples_used[i],])
  
  #,
                       #weight3=parms$mix_weights[samples_used[i],,3])
  
  dat_tmp = left_join(dat,weights) %>%
    mutate(sigma = parms$sigma[samples_used[i]],
           predicted_effort = weight*rnorm(nrow(dat),predicted_effort1,sigma) +
                              (1-weight)*rnorm(nrow(dat),predicted_effort2,sigma) #+
                             # weight3*rnorm(nrow(dat),predicted_effort3,sigma)
           ) %>%
    group_by(class,condition,time) %>%
    summarise(predicted_effort = mean(predicted_effort),
              observed_effort = mean(eff))
  pp_list[[i]] = dat_tmp
}

pp = bind_rows(pp_list) %>%
  group_by(class,condition,time) %>%
  summarise(predicted_effort_mean = mean(predicted_effort),
            predicted_effort_hi = quantile(predicted_effort,0.975),
            predicted_effort_lo = quantile(predicted_effort,0.025),
            observed_effort = mean(observed_effort))

ggplot(data=pp,aes(x=time,group=factor(class),colour=factor(class))) +
  geom_point(aes(y=observed_effort)) +
  geom_line(aes(y=predicted_effort_mean)) +
  facet_grid(.~condition)


# 
# 
# 
# rm(fit_3_mixture)
# 
# samples_used = sample(1:4000,size=100)
# 


%>%
  summarise(weight_m_1 = mean(`1`),
            weight_m_2 = mean(`2`),
            weight_m_3 = mean(`3`)) %>%
  mutate(class = (weight_m_1 > weight_m_2)*(weight_m_1 > weight_m_3)*1 +
                 (weight_m_2 > weight_m_1)*(weight_m_2 > weight_m_3)*2 +
                 (weight_m_3 > weight_m_2)*(weight_m_3 > weight_m_1)*3) %>%
  group_by(condition) %>%
  summarise(class1 = sum(class==1),
            class2 = sum(class==2),
            class3 = sum(class==3))
#            
# #Get predicted effort
# pp = melt(parms$predicted_effort)
# names(pp)<-c('sample','obs','mixture','predicted_effort')
# pp = pp %>% filter(sample %in% samples_used)
# 
# pp1 = left_join(pp,mix_weights)
# rm(pp)
# rm(mix_weights)
# 
# #get sigma
# sigma=melt(parms$sigma)
# names(sigma)<-c('sample','sigma')
# sigma = sigma %>% filter(sample %in% samples_used)
# pp2=left_join(pp1,sigma)
# rm(pp1)
# rm(sigma)
# 
# pp2 = pp2 %>% 
#   mutate(weighted_effort = weight*rnorm(nrow(pp2),predicted_effort,sigma) )
# 
# preds = pp2 %>%
#   select(sample,obs,mixture,subject,condition,weighted_effort) %>%
#   mutate(mixture=factor(mixture,levels=1:3,labels=c('mix1','mix2','mix3'))) %>%
#   spread(key=mixture,value=weighted_effort)
# 
# weights = pp2 %>%
#   select(sample,obs,mixture,subject,condition,weight) %>%
#   mutate(mixture=factor(mixture,levels=1:3,labels=c('weight1','weight2','weight3'))) %>%
#   spread(key=mixture,value=weight)
# 
# preds$weight1=weights$weight1
# preds$weight2=weights$weight2
# preds$weight3=weights$weight3
# rm(weights)
# rm(pp2)
# 
# x=preds %>%
#   mutate(class = (weight1 > weight2)*(weight1 > weight3)*1 +
#            (weight2 > weight1)*(weight_m_2 > weight3)*2 +
#            (weight3 > weight2)*(weight_m_3 > weight1)*3,
#          sub_obs = obs %% 70) %>%
#   
# 
# 
# pp1 = pp %>%
#   spread(key=mixture,value=predicted_effort)
# 
# 
# 
# 
#  %>%
#   mutate(predicted_effort_comb = rnorm(nrow(pp1),'1',sigma)     ))
# 
# 
# 
# head(pp2)
# 
# 
# 
# weights_CIs = mix_weights %>%
#   spread(key=mixture,value=weight) %>%
#   group_by(condition,subject) %>%
#   summarise(weight_m_1 = mean(`1`),
#             weight_hi_1 = quantile(`1`,0.975),
#             weight_lo_1 = quantile(`1`,0.025),
#             weight_m_2 = mean(`2`),
#             weight_hi_2 = quantile(`2`,0.975),
#             weight_lo_2 = quantile(`2`,0.025))
# 
# ggplot(data=weights_CIs) +
#   geom_point(aes(x=weight_m_1,y=weight_m_2)) +
#   facet_grid(. ~ condition)
# 
# 
# 
library(bridgesampling)
ml_3_mixture = bridge_sampler(fit_3_mixture,maxiter=10000)
ml_mixture = bridge_sampler(fit_mixture)
ml_disc = bridge_sampler(fit_disc)
ml_demand = bridge_sampler(fit_demand)

bf(ml_disc,ml_demand)

bf(ml_3_mixture,ml_demand)

bf(ml_disc,ml_3_mixture)

bf(ml_disc,ml_mixture)

bf(ml_demand,ml_mixture)
# 
# save(fit_group,file="../model_output/stan_univariate_group_samples.RData")
# 
# 
# #Fit person level model
# fit_subj = stan(file="./models/stan_univariate_subject.stan",
#                  data=model_data,cores=4)
# 
# save(fit_subj,file="../model_output/stan_univariate_subject_samples.RData")
# 
# 
# #Fit hierarchical model
# fit_hier = stan(file="./models/stan_univariate_hierarchical.stan",
#                 data=model_data,cores=4,control=list(adapt_delta=0.99,max_treedepth=20))
# 
# save(fit_hier,file="../model_output/stan_univariate_hierarchical_samples.RData")
# 
# 
# #Fit multiple group model
# fit_mg = stan(file="./models/stan_univariate_multiple_group.stan",
#               data=model_data,cores=4)
# 
# save(fit_mg,file="../model_output/stan_univariate_multiple_group_samples.RData")
# 
# #Fit mixture model
# fit_mix = stan(file="./models/stan_univariate_mixture.stan",
#               data=model_data,cores=4)
# 
# save(fit_mix,file="../model_output/stan_univariate_mixture_samples.RData")
# 
# 
# #Fit bivariate group level model
# fit_biv = stan(file="./models/stan_bivariate_group.stan",
#                  data=model_data,cores=4)
# 
# save(fit_biv,file="../model_output/stan_bivariate_group_samples.RData")
# 
# #Fit closed loop model
# fit_cl = stan(file="./models/stan_univariate_group_closed_loop.stan",
#                  data=model_data,cores=4)
# 
# save(fit_cl,file="../model_output/stan_univariate_group_closed_loop_samples.RData")
# 
# #Fit exponential learning model
# fit_expl = stan(file="./models/stan_exp_learning_group.stan",
#               data=model_data,cores=4)
# 
# save(fit_expl,file="../model_output/stan_exp_learning_group_samples.RData")
# 
# 
# 
# 
# 
# 
# hdi(extract(fit_group,pars="alpha"))
# hdi(extract(fit_group,pars="beta"))
# hdi(extract(fit_group,pars="y0"))
# hdi(extract(fit_group,pars="sigma0"))
# hdi(extract(fit_group,pars="sigma_change"))
# 
# mean(extract(fit_group,pars="alpha")$alpha>0)
# mean(extract(fit_group,pars="beta")$beta>0)
# 
# 
# alpha)
# 
# beta=extract(fit_group,pars="beta")
# hdi(beta)
# 
# y0=extract(fit_group,pars="y0")
# hdi(alpha)
# 
# beta=extract(fit_group,pars="beta")
# hdi(beta)
# 
# 
# # posteriors_group=bind_rows(extract(fit))
# # 
# # plot_group=ggplot(data=posteriors_group) +
# #     geom_density2d(aes(x=alpha,y=beta))
# 
# 
# save(fit_group,file="../model_output/stan_univariate_group_samples.RData")
# 
# 
# 
# 
# 
# smry=summary(fit)
# smry$summary
# 
# write.csv(smry$summary,file="../model_output/univariate_stan_smry.csv")
