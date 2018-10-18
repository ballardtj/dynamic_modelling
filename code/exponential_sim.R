library(tidyverse)

expand.grid(effort = seq(0,10,0.1),
            alpha = seq(-4,4,1),
            beta = seq(0.05,0.5,0.05),
            assymptote = seq(05,10,01)) %>%
  mutate(value = assymptote /(1+exp(-(alpha+effort*beta)))) %>%
  ggplot() +
  geom_line(aes(x=effort,y=value,colour=factor(beta))) +
  facet_grid(alpha~assymptote)



alpha = 0    #2.56 # + 0.91*50
beta = 1.16 #+ 0.51*50
effort = seq(0,10,0.01)
#P = exp(alpha+beta*effort)/(1+exp(alpha+beta*effort))
P = 1/(1+exp(-1*(alpha+beta*effort)));
plot(effort,P)
