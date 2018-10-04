#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)

#load data
load(file="../clean_data/wide_data_v4.RData")

#prepare data for stan
perf_vars = names(wide_data)[grep('perf',names(wide_data))]
diff_vars = names(wide_data)[grep('diff',names(wide_data))]

model_data = list(
  Nsubj = dim(wide_data)[1],
  Nobs = length(perf_vars),
  perf = as.matrix( wide_data[,perf_vars]),
  diff = as.matrix( wide_data[,diff_vars]),
  condition = as.numeric(wide_data$condition), #1=Approach, 2=Avoidance
  Ncond = 2
)

#Fit group level model
fit_group = stan(file="./models/stan_univariate_group.stan",
           data=model_data,cores=4)

save(fit_group,file="../model_output/stan_univariate_group_samples.RData")


#Fit person level model
fit_subj = stan(file="./models/stan_univariate_subject.stan",
                 data=model_data,cores=4)

save(fit_subj,file="../model_output/stan_univariate_subject_samples.RData")


#Fit hierarchical model
fit_hier = stan(file="./models/stan_univariate_hierarchical.stan",
                data=model_data,cores=4,control=list(adapt_delta=0.99,max_treedepth=20))

save(fit_hier,file="../model_output/stan_univariate_hierarchical_samples.RData")


#Fit multiple group model
fit_mg = stan(file="./models/stan_univariate_multiple_group.stan",
              data=model_data,cores=4)

save(fit_mg,file="../model_output/stan_univariate_multiple_group_samples.RData")

#Fit mixture model
fit_mix = stan(file="./models/stan_univariate_mixture.stan",
              data=model_data,cores=4)

save(fit_mix,file="../model_output/stan_univariate_mixture_samples.RData")


#Fit bivariate group level model
fit_biv = stan(file="./models/stan_bivariate_group.stan",
                 data=model_data,cores=4)

save(fit_biv,file="../model_output/stan_bivariate_group_samples.RData")

#Fit closed loop model
fit_cl = stan(file="./models/stan_univariate_group_closed_loop.stan",
                 data=model_data,cores=4)

save(fit_cl,file="../model_output/stan_univariate_group_closed_loop_samples.RData")

#Fit exponential learning model
fit_expl = stan(file="./models/stan_exp_learning_group.stan",
              data=model_data,cores=4)

save(fit_expl,file="../model_output/stan_exp_learning_group_samples.RData")






hdi(extract(fit_group,pars="alpha"))
hdi(extract(fit_group,pars="beta"))
hdi(extract(fit_group,pars="y0"))
hdi(extract(fit_group,pars="sigma0"))
hdi(extract(fit_group,pars="sigma_change"))

mean(extract(fit_group,pars="alpha")$alpha>0)
mean(extract(fit_group,pars="beta")$beta>0)


alpha)

beta=extract(fit_group,pars="beta")
hdi(beta)

y0=extract(fit_group,pars="y0")
hdi(alpha)

beta=extract(fit_group,pars="beta")
hdi(beta)


# posteriors_group=bind_rows(extract(fit))
# 
# plot_group=ggplot(data=posteriors_group) +
#     geom_density2d(aes(x=alpha,y=beta))


save(fit_group,file="../model_output/stan_univariate_group_samples.RData")





smry=summary(fit)
smry$summary

write.csv(smry$summary,file="../model_output/univariate_stan_smry.csv")
