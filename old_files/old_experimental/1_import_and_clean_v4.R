#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)
library(foreign)

dataset = read.spss("../raw_data/HLM WitTrial.sav", use.value.labels=F,to.data.frame=TRUE)

# bin_size = 5
# 
# long_data = dataset %>%
#   mutate(subject=subno-100,
#          time = ceiling(((torder-2)*14 + time/60)/bin_size),
#          perf = correctnc - incorrectnc,
#          diff=wdiff,
#          eff=weff,
#          condition = factor(condition,levels=c(1,-1),labels=c('Approach','Avoidance'))) %>%
#  select(subject,condition,time,diff,eff,conhitnc:incorrectnc) %>%
#   group_by(subject,condition,time) %>%
#   summarise(perf = sum(correctnc)-sum(incorrectnc),
#             diff = mean(diff),
#             eff = mean(eff),
#             hit = sum(conhitnc),
#             miss = sum(confanc),
#             fa = sum(nconfanc),
#             cr = sum(nconhitnc)) %>%
#      mutate(zhr = qnorm(hit / (hit+miss)),
#             zfa = qnorm(fa / (fa+cr)),
#             dprime = zhr-zfa,
#             crit = -zfa)



#select(subject,condition,time,perf,diff)

bin_size = 1

long_data = dataset %>%
  mutate(subject=subno-100,
         time = ceiling(((torder-2)*14 + time/60)/bin_size),
         diff = wdiff,
         eff=weff,
         condition = factor(condition,levels=c(1,-1),labels=c('Approach','Avoidance'))) %>%
  group_by(subject,time,condition) %>%
  summarise(perf = sum(correctnc)-sum(incorrectnc),#sum(correctnc)/(sum(correctnc)+sum(incorrectnc)),
            diff = mean(diff),
            eff=mean(eff)) %>%
  ungroup() %>%
  select(subject,condition,time,perf,diff,eff)


ggplot(data=long_data,aes(x=time,y=eff))+
  stat_summary(geom='smooth') +
  stat_summary(geom='smooth',aes(y=perf),colour='red')



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
 