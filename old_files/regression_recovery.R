

nsim = 100
nobs = 1000
B0 = 1
B1 = -0.2
sigma = 0.01



sim_dat = data.frame(x = runif(n=n,-10,10))
recovery=data.frame(sim=1:nsim,B0=NA,B1=NA,sigma=NA)

for(i in 1:nsim){
  sim_dat$y = B0 + B1*sim_dat$x + rnorm(n,0,sigma)

fit=lm(y~x,data=sim_dat)

recovery$B0[i]=summary(fit)$coefficients[,'Estimate'][1]
recovery$B1[i]=summary(fit)$coefficients[,'Estimate'][2]
recovery$sigma[i]=summary(fit)$sigma

}

recovery = recovery %>% gather(key=parameter,value=value,B0:sigma)

ggplot(data=recovery) +
  geom_line(aes(x=sim,y=value)) +
  facet_grid(.~parameter)
