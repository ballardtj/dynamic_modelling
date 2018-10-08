#NOTE: This script is used to implement the Bayesian change models presented
#in Ballard, Palada, Griffin, and Neal (2018). The files for the models themselves
#are located in the 'models' folder.


#clear workspace
rm(list=ls())

#load rtan
library(foreign)
library(tidyverse)

#make sure working directory is set to the "analysis" folder

#load data
data = read.spss("data/raw/HLM WitTrial.sav", use.value.labels=F,to.data.frame=TRUE) %>%
  mutate(subject = subno - 100,
         trial = torder - 1,
         goal = wgoal,
         diff = wdiff,
         eff = weff,
         score = (condition==1)*correct + (condition==2)*incorrect) %>%
  select(subject,condition,trial,time,goal,diff,eff,score)

save(data,file="data/clean/goal_data.RData")
