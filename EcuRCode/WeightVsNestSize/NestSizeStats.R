# TODO: Add comment
# Statistica analysis for the nest size vs spider size 
# Author: Ruth
###############################################################################

library (lme4)
library(lmerTest) # not sure what excatly this does
library(glmmADMB)
library(ICC)
library(reshape)
source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/WeightVsNestSize/NestSizeData.R")
source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/OverDispersionFunction.R")


############ Single vs multiple nests #####################################

spidersSglMt <- subset(spiders, Instar == "Adult")

###Leg Length
SglMtLegMod1 <- lmer(LegLen.mm ~ type + (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

qqnorm(resid(SglMtLegMod1), main = "SglMtMod1"); abline(0, 1) # not good, how would I fix this I don't know.
overdisp_fun(SglMtLegMod1)# really really over dispersed
summary(SglMtLegMod1)
anova(SglMtLegMod1)

### Testing against reduced model


SglMtLegRedMod <- lmer(LegLen.mm ~ (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

qqnorm(resid(SglMtLegRedMod), main = "SglMtLegRedMod") # not good
overdisp_fun(SglMtLegRedMod)# really really over dispersed
summary(SglMtLegRedMod)
anova(SglMtLegRedMod, SglMtLegMod1 )



