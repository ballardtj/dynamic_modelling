# ---------------------------------------------------------------------
# Program: LCSM_OpenMx_v1.0.6-1581.R
#  Author: Paolo Ghisletta and John J. McArdle
#    Date: mercredi, mars 23, 2011  16:21
# Comment: OpenMx script to run LCSM
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
#attach OpenMx library
library(OpenMx)


# ----------------------------------
# create labels for ensembles of names
indic <- c("verbal0","verbal1","verbal3","verbal5")
lat <- c("l0","l1","l2","l3","l4","l5")
diff <- c("d1","d2","d3","d4","d5")


# ----------------------------------
# Saturated Model.

Sat.model <- mxModel("Sat", type="RAM",
    manifestVars=indic,
    mxPath(from="one", to=indic, arrows=1, free=TRUE),
    mxPath(from=indic, to=indic, all=TRUE, arrows=2, free=TRUE),
    mxPath(from=indic, arrows=2, free=TRUE, values=c(30,35,50,115)),
    mxData(observed=wisc, type="raw")
)

Sat.fit <- mxRun(Sat.model)

# ----------------------------------
# Code GREEN, should be OK but to be sure run again starting at previous solution.
Sat.model.2 <- Sat.fit
Sat.fit.2 <- mxRun(Sat.model.2)
summary(Sat.fit.2)


# ----------------------------------
# LCSM on WISC verbal

LCSM.model <- mxModel("LCSM", type="RAM",
    manifestVars=indic,
    latentVars=c("B0","B1",lat,diff),
    mxPath(from=lat[c(1,2,4,6)], to=indic, arrows=1, free=FALSE, values=1),
    mxPath(from=lat[1:5], to=lat[2:6], arrows=1, free=FALSE, values=1),
    mxPath(from=diff, to=lat[2:6], arrows=1, free=FALSE, values=1),
    mxPath(from=lat[1:5], to=diff, arrows=1, free=TRUE, values=.1, labels="B"),
    mxPath(from="B0", to="l0", arrows=1, free=FALSE, values=1),
    mxPath(from="B1", to=diff, arrows=1, free=FALSE, values=1),
    mxPath(from="one", to=c("B0","B1"), free=TRUE, values=c(20,5), labels=c("MB0","MB1")),
    mxPath(from=c("B0","B1",indic), arrows=2, free=TRUE, values=c(20,2,13,13,13,13),
           labels=c("VB0","VB1","Ve","Ve","Ve","Ve")),
    mxPath(from="B0", to="B1", arrows=2, free=TRUE, values=3, labels="CB0B1"),
    mxData(observed=wisc, type="raw")
)

LCSM.fit <- mxRun(LCSM.model)
summary(LCSM.fit)
mxCompare(Sat.fit.2, LCSM.fit)


# ----------------------------------
# LCSM on WISC verbal with free Ve

LCSM.freeVe.model <- mxModel(LCSM.model,(
  mxPath(from=c("B0", "B1", indic), arrows=2, free=TRUE, values=c(20,2,13,13,13,13),
                labels=c("VB0", "VB1", "Ve0", "Ve1", "Ve3", "Ve5"))))

LCSM.freeVe.fit <- mxRun(LCSM.freeVe.model)
summary(LCSM.freeVe.fit)
mxCompare(Sat.fit, LCSM.freeVe.fit)
mxCompare(LCSM.freeVe.fit, LCSM.fit)
