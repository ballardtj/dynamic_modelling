#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(foreign)

dataset = read.spss("../raw_data/HLM WitTrial.sav", use.value.labels=F,to.data.frame=TRUE)

decisions_pending = as.vector(t(read.table('Decisions_Pending_Data.csv',sep=',',header=T)[,4:16])))

long_data = dataset %>%
  mutate(subject=subno-100,
         trial=torder-1,
         time = time/60,
         diff = wdiff,
         eff=weff,
         disc = (condition==1)*(goal-correct) + (condition==-1)*((20-goal)-correct),
         condition = factor(condition,levels=c(1,-1),labels=c('Approach','Avoidance'))) %>%
  select(subject,condition,trial,battain,time,diff,eff,disc)
  
  # group_by(subject,trial,time,condition) %>%
  # summarise(perf = sum(correctnc)-sum(incorrectnc),#sum(correctnc)/(sum(correctnc)+sum(incorrectnc)),
  #           diff = mean(diff),
  #           eff=mean(eff)) %>%
  # ungroup() %>%
  # 

ggplot(data=long_data,aes(x=time,y=eff))+
  stat_summary(geom='line') +
  geom_smooth(method="lm") +
  facet_wrap(~condition)

ggplot(data=long_data,aes(x=disc,y=eff))+
  stat_summary(geom='line') +
  geom_smooth(method="lm") +
  facet_wrap(~condition) + coord_cartesian(xlim=c(0,20))

ggplot(data=long_data,aes(x=disc/time,y=eff))+
  stat_summary(geom='line') +
  geom_smooth(method="lm") +
  facet_wrap(~condition) + coord_cartesian(xlim=c(0,20))

ggplot(data=long_data,aes(x=disc/time,y=eff))+
  stat_summary(geom='smooth',method='lm',formula="y~x") +
  facet_wrap(~condition)

#Convert to wide data frame, which will be needed for latent change score modeling
wide_data_perf = long_data %>%
  select(-diff,-eff) %>%
  mutate(perftime = factor(paste0('perf',time),levels=paste0('perf',1:(70/bin_size)))) %>%
  select(-time) %>%
  spread(key=perftime,value=perf) 

wide_data_diff = long_data %>%
  select(-perf,-eff) %>%
  mutate(difftime = factor(paste0('diff',time),levels=paste0('diff',1:(70/bin_size)))) %>%
  select(-time) %>%
  spread(key=difftime,value=diff) 

wide_data_eff = long_data %>%
  select(-perf,-diff) %>%
  mutate(difftime = factor(paste0('eff',time),levels=paste0('eff',1:(70/bin_size)))) %>%
  select(-time) %>%
  spread(key=difftime,value=eff) 

wide_data = left_join(wide_data_perf,wide_data_diff)
  
save(wide_data,file="../clean_data/wide_data_v4.RData")
 