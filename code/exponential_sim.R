library(tidyverse)

expand.grid(effort = seq(0,10,0.1),
            alpha = -1,#seq(-4,4,1),
            beta = 0.5,#seq(0.05,0.5,0.05),
            assymptote = 2) %>% #seq(05,10,01)) %>%
  mutate(d_score = assymptote /(1+exp(-(alpha+effort*beta))),
         required_effort = (-log((assymptote-d_score)/d_score) - alpha)/beta,
         required_effort_2 = 10 - 10*exp(-required_effort*0.1)) %>%
  ggplot() +
  geom_line(aes(x=d_score,y=required_effort_2,colour=factor(alpha))) +
  facet_grid(beta~assymptote)


expand.grid(effort = seq(0,10,0.1),
            alpha = -1,#seq(-4,4,1),
            beta = 0.5,#seq(0.05,0.5,0.05),
            assymptote = 2) %>% #seq(05,10,01)) %>%
  mutate(d_score = assymptote /(1+exp(-(alpha+effort*beta))),
         required_effort = (-log((assymptote-d_score)/d_score) - alpha)/beta),


         )



alpha = 8.35  + 13.99*50
beta = 2.41 + 1.63*50
effort = seq(0,10,0.01)
#P = exp(alpha+beta*effort)/(1+exp(alpha+beta*effort))
P = 1/(1+exp(-1*(alpha+beta*effort)));
plot(effort,P,ylim=c(0,1))

#
dS = b1/(1+exp(-1*(b2+b3*effort)))

ds*(1+exp(-1*(b2+b3*effort))) = b1

