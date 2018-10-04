# ---------------------------------------------------------------------
# Program: LCSM_lavaan_v0.4-7.R
#  Author: Paolo Ghisletta and John J. McArdle
#    Date: mercredi, mars 23, 2011  16:02
# Comment: lavaan script to run LCSM
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


########################
# call lavaan library
library(lavaan)



# ----------------------------------
# LCSM on WISC verbal

LCSM.model <- ' l0 =~ 1*verbal0 #Loading each variable on its respective factor
                l1 =~ 1*verbal1
                l2 =~ 0*verbal1
                l3 =~ 1*verbal3
                l4 =~ 0*verbal3
                l5 =~ 1*verbal5
                l1 ~ 1*l0 #Autoregressive effects
                l2 ~ 1*l1
                l3 ~ 1*l2
                l4 ~ 1*l3
                l5 ~ 1*l4
                d1 =~ 1*l1
                d2 =~ 1*l2
                d3 =~ 1*l3
                d4 =~ 1*l4
                d5 =~ 1*l5
                d1 ~ label("B")*l0
                d2 ~ equal("B")*l1
                d3 ~ equal("B")*l2
                d4 ~ equal("B")*l3
                d5 ~ equal("B")*l4
                B0 =~ 1*l0
                B1 =~ 1*d1 + 1*d2 + 1*d3 + 1*d4 + 1*d5
                B0 ~ 1
                B1 ~ 1
                B0 ~~ start(20)*B0
                B1 ~~ B1
                B0 ~~ B1
                verbal0 ~~ label("Ve")*verbal0
                verbal1 ~~ equal("Ve")*verbal1
                verbal3 ~~ equal("Ve")*verbal3
                verbal5 ~~ equal("Ve")*verbal5 '
			
LCSM.fit <- lavaan(LCSM.model, data=wisc)
summary(LCSM.fit, fit.measure=TRUE)



LCSM.freeVe.model <- ' l0 =~ 1*verbal0
                l1 =~ 1*verbal1
                l2 =~ 0*verbal1
                l3 =~ 1*verbal3
                l4 =~ 0*verbal3
                l5 =~ 1*verbal5
                l1 ~ 1*l0
                l2 ~ 1*l1
                l3 ~ 1*l2
                l4 ~ 1*l3
                l5 ~ 1*l4
                d1 =~ 1*l1
                d2 =~ 1*l2
                d3 =~ 1*l3
                d4 =~ 1*l4
                d5 =~ 1*l5
                d1 ~ label("B")*l0
                d2 ~ equal("B")*l1
                d3 ~ equal("B")*l2
                d4 ~ equal("B")*l3
                d5 ~ equal("B")*l4
                B0 =~ 1*l0
                B1 =~ 1*d1 + 1*d2 + 1*d3 + 1*d4 + 1*d5
                B0 ~ 1
                B1 ~ 1
                B0 ~~ start(20)*B0
                B1 ~~ B1
                B0 ~~ B1
                verbal0 ~~ label("Ve0")*verbal0
                verbal1 ~~ label("Ve1")*verbal1
                verbal3 ~~ label("Ve3")*verbal3
                verbal5 ~~ label("Ve5")*verbal5 '
			
LCSM.freeVe.fit <- lavaan(LCSM.freeVe.model, data=wisc)
summary(LCSM.freeVe.fit, fit.measure=TRUE)
