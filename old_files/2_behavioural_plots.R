#clear workspace
rm(list=ls())

#load packages (need tidyverse installed)
library(tidyverse)

load(file="../clean_data/clean.RData")

#Plot average performance over trials
ggplot(data=imported_data,aes(x=trial,y=performance)) +
  stat_summary(fun.y="mean",geom="line")
 
#Plot average performance over trials by condition
ggplot(data=imported_data,aes(x=trial,y=performance,colour=condition)) +
  stat_summary(fun.y="mean",geom="line")
 
#Plot individual subject performance trials by subject
ggplot(data=imported_data,aes(x=trial,y=performance,colour=condition)) +
   geom_line() + facet_wrap(~subject)
 