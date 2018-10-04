#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)

#Load data, recode variable names, and save.
long_data = read.csv(file="../raw_data/Affect_Data.csv") %>%
  mutate(subject=subno-100,
         time = (torder-2)*14 + time/60,
         perf = correctnc - incorrectnc,
         condition = factor(condition,levels=c(1,-1),labels=c('Approach','Avoidance'))) %>%
  select(subject,condition,time,perf)

ggplot(data=long_data,aes(x=time,y=perf))+
  stat_summary(geom='smooth')



#Convert to wide data frame, which will be needed for latent change score modeling
wide_data = long_data %>%
  mutate(perftime = factor(paste0('perf',time),levels=paste0('perf',1:70))) %>%
  select(-time) %>%
  spread(key=perftime,value=perf) 
  
save(wide_data,file="../clean_data/wide_data.RData")
 