# ---------------------------------------------------------------------
# Program: LCSM_sem_v0.9-21.R
#  Author: Paolo Ghisletta and John J. McArdle
#    Date: mercredi, mars 23, 2011  15:47
# Comment: sem script to run LCSM
# ---------------------------------------------------------------------


#------------------------
# remove all previous R objects
rm(list=ls())

#------------------------
# set working directory
setwd("F:/")

#------------------------
# read in WISC data
wisc <- read.table("wisc.dat", sep="\t", na.strings = "NA", header=TRUE)


#------------------------
# call sem library
library(sem)

#select indicators of interest
wisc.verbal <- wisc[,c("verbal0","verbal1","verbal3","verbal5")]

#compute augmented SSCP matrix
wisc.verbal.mom <- raw.moments(cbind(wisc.verbal, 1))

# ----------------------------------
# LCSM on WISC verbal

LCSM.verbal.model <- specify.model()
  l0 -> verbal0, NA, 1
  l1 -> verbal1, NA, 1
  l2 -> verbal1, NA, 0
  l3 -> verbal3, NA, 1
  l4 -> verbal3, NA, 0
  l5 -> verbal5, NA, 1
  l0 -> l1, NA, 1
  l1 -> l2, NA, 1
  l2 -> l3, NA, 1
  l3 -> l4, NA, 1
  l4 -> l5, NA, 1
  d1 -> l1, NA, 1
  d2 -> l2, NA, 1
  d3 -> l3, NA, 1
  d4 -> l4, NA, 1
  d5 -> l5, NA, 1
  l0 -> d1, B, .1
  l1 -> d2, B, .1
  l2 -> d3, B, .1
  l3 -> d4, B, .1
  l4 -> d5, B, .1
  B0 -> l0, NA, 1
  B1 -> d1, NA, 1
  B1 -> d2, NA, 1
  B1 -> d3, NA, 1
  B1 -> d4, NA, 1
  B1 -> d5, NA, 1
  1 -> B0, MB0, 20
  1 -> B1, MB1, NA
  B0 <-> B0, VB0, 20
  B1 <-> B1, VB1, 2
  B0 <-> B1, CB0B1, 1
  verbal0 <-> verbal0, Ve, 5
  verbal1 <-> verbal1, Ve, 5
  verbal3 <-> verbal3, Ve, 5
  verbal5 <-> verbal5, Ve, 5


LCSM.verbal.fit <- sem(LCSM.verbal.model, wisc.verbal.mom, nrow(wisc), fixed.x="1", raw=TRUE, debug=TRUE)
summary(LCSM.verbal.fit)




# ----------------------------------
# LCSM on WISC verbal with B=0

LCSM.verbalB0.model <- specify.model()
  l0 -> verbal0, NA, 1
  l1 -> verbal1, NA, 1
  l2 -> verbal1, NA, 0
  l3 -> verbal3, NA, 1
  l4 -> verbal3, NA, 0
  l5 -> verbal5, NA, 1
  l0 -> l1, NA, 1
  l1 -> l2, NA, 1
  l2 -> l3, NA, 1
  l3 -> l4, NA, 1
  l4 -> l5, NA, 1
  d1 -> l1, NA, 1
  d2 -> l2, NA, 1
  d3 -> l3, NA, 1
  d4 -> l4, NA, 1
  d5 -> l5, NA, 1
  B0 -> l0, NA, 1
  B1 -> d1, NA, 1
  B1 -> d2, NA, 1
  B1 -> d3, NA, 1
  B1 -> d4, NA, 1
  B1 -> d5, NA, 1
  1 -> B0, MB0, 20
  1 -> B1, MB1, NA
  B0 <-> B0, VB0, 20
  B1 <-> B1, VB1, 2
  B0 <-> B1, CB0B1, 1
  verbal0 <-> verbal0, Ve, 5
  verbal1 <-> verbal1, Ve, 5
  verbal3 <-> verbal3, Ve, 5
  verbal5 <-> verbal5, Ve, 5


LCSM.verbalB0.fit <- sem(LCSM.verbalB0.model, wisc.verbal.mom, nrow(wisc), fixed.x="1", raw=TRUE, debug=TRUE)
summary(LCSM.verbalB0.fit)
anova(LCSM.verbal.fit, LCSM.verbalB0.fit)

# ----------------------------------
# LCSM on WISC verbal with free Ve

LCSM.verbal.freeVe.model <- specify.model()
  l0 -> verbal0, NA, 1
  l1 -> verbal1, NA, 1
  l2 -> verbal1, NA, 0
  l3 -> verbal3, NA, 1
  l4 -> verbal3, NA, 0
  l5 -> verbal5, NA, 1
  l0 -> l1, NA, 1
  l1 -> l2, NA, 1
  l2 -> l3, NA, 1
  l3 -> l4, NA, 1
  l4 -> l5, NA, 1
  d1 -> l1, NA, 1
  d2 -> l2, NA, 1
  d3 -> l3, NA, 1
  d4 -> l4, NA, 1
  d5 -> l5, NA, 1
  l0 -> d1, B, .1
  l1 -> d2, B, .1
  l2 -> d3, B, .1
  l3 -> d4, B, .1
  l4 -> d5, B, .1
  B0 -> l0, NA, 1
  B1 -> d1, NA, 1
  B1 -> d2, NA, 1
  B1 -> d3, NA, 1
  B1 -> d4, NA, 1
  B1 -> d5, NA, 1
  1 -> B0, MB0, 20
  1 -> B1, MB1, NA
  B0 <-> B0, VB0, 20
  B1 <-> B1, VB1, 2
  B0 <-> B1, CB0B1, 1
  verbal0 <-> verbal0, Ve0, 5
  verbal1 <-> verbal1, Ve1, 5
  verbal3 <-> verbal3, Ve3, 5
  verbal5 <-> verbal5, Ve5, 5


LCSM.verbal.freeVe.fit <- sem(LCSM.verbal.freeVe.model, wisc.verbal.mom, nrow(wisc), fixed.x="1", raw=TRUE, debug=TRUE)
summary(LCSM.verbal.freeVe.fit)
anova(LCSM.verbal.freeVe.fit, LCSM.verbal.fit)
