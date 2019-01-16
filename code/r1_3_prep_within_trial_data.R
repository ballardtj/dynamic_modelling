#NOTE: This script is used to implement the Bayesian change models presented
#in Ballard, Palada, Griffin, and Neal (2018). The files for the models themselves
#are located in the 'models' folder.


#clear workspace
rm(list=ls())

#load rtan
library(foreign)
library(tidyverse)

#sdata = read.spss("data/raw/HLM WitTrial.sav")

#make sure working directory is set to the "analysis" folder
data = read_csv("data/raw/HLM_WitTrial_P1.csv") %>%
  mutate(subject = as.numeric(as.factor(subno)),
         condition = (condition==-1) + 2*(condition==1), #1=approach, 2=avoidance
         trial = torder - 1,
         time = time/60,
         effort = eff,
         difficulty = diff,
         performance = correct,
         goal = (condition==1)*goal + (condition==2)*(10-goal),
         score = (condition==1)*correct + (condition==2)*incorrect,
         obs=1:n()) %>%
  select(obs,subject,condition,trial,goal,time,difficulty,effort,performance,score)


save(data,file="data/clean/goal_data.RData")



# data %>%
#   group_by(condition,trial) %>%
#   summarise(goal = mean(goal)) %>%
#   ggplot() +
#   geom_line(aes(x=trial,y=goal)) +
#   facet_wrap(~condition)


