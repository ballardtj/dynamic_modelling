

#Step 1 is to simulate some data and make sure I understand how the LCM works and how it can be estimated in R






#------------------------
# remove all previous R objects
rm(list=ls())


#------------------------
# call lavaan library
library(RAMpath)


#------------------------
# load data
data(ex1)


m1<-'
manifest=3
label=age,hvlt,ept
arrow(2,1)=?
arrow(3,1)=?
arrow(3,2)=?
sling(1,1)=?
sling(2,2)=?
sling(3,3)=?
'

## Fit the model
res1<-ramFit(m1, ex1)


