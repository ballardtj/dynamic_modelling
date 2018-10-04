#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(rstan)
library(HDInterval)

#load data
load(file="../clean_data/wide_data.RData")

#prepare data for stan
vars = names(wide_data)[grep('perf',names(wide_data))]

model_data = list(
  Nsubj = dim(wide_data)[1],
  Nobs = length(vars),
  perf = as.matrix( wide_data[,vars])
)


fit_group = stan(file="./models/stan_univariate_group_pp.stan",
           data=model_data,cores=4)

save(fit_group,file="../model_output/stan_univariate_group_pp_samples.RData")





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
