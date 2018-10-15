#NOTE: This script is used to implement the Bayesian change models presented
#in Ballard, Palada, Griffin, and Neal (2018). The files for the models themselves
#are located in the 'models' folder.


#clear workspace
rm(list=ls())

#load rtan
#library(foreign)
library(tidyverse)

#make sure working directory is set to the "analysis" folder
data = read_csv("data/raw/HLM_WitTrial_P1.csv") %>%
  mutate(subject = subno - 100,
         condition = (condition==1) + 2*(condition==-1),
         trial = torder - 1,
         time = time/60,
         effort = eff,
         difficulty = diff,
         score = (condition==1)*correct + (condition==2)*incorrect) %>%
  select(subject,condition,trial,goal,time,difficulty,effort,score)


save(data,file="data/clean/goal_data.RData")
