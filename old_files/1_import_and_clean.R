#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)

#Load data, recode variable names, and save.
long_data = read.csv(file="../raw_data/Goaldata_long.csv") %>%
  mutate(subject = subno,
         trial = trialno,
         perf = correct,
         condition = factor(condition,levels=c(1,-1),labels=c('Approach','Avoidance'))) %>%
  select(subject,condition,trial,goal,perf)
  
save(long_data,file="../clean_data/long_data.RData")

#Convert to wide data frame, which will be needed for latent change score modeling
wide_data = long_data %>%
  gather(key=variable,value=value,goal:perf) %>%
  unite(col=variable,c(variable,trial),sep="") %>%
  spread(key=variable,value=value) %>%
  select(subject,condition,goal1,goal2,goal3,goal4,goal5,goal6,goal7,
         goal8,goal9,goal10,perf1,perf2,perf3,
         perf4,perf5,perf6,perf7,
         perf8,perf9,perf10)

save(wide_data,file="../clean_data/wide_data.RData")
  
  
 