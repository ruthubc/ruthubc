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

#### All nests ###############
spidersSglMt <- subset(spiders, Instar == "Adult")

spidersSglMt <- subset(spidersSglMt , FemalesHaveEggsOrJuvs != "n")

###Leg Length ##########
SglMtLegMod1 <- lmer(lnLegLen ~ type + (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

qqnorm(resid(SglMtLegMod1), main = "SglMtMod1"); abline(0, 1) # not good, how would I fix this I don't know.
overdisp_fun(SglMtLegMod1)# underdispered
summary(SglMtLegMod1)
anova(SglMtLegMod1)

### Testing against reduced model


SglMtLegRedMod <- lmer(lnLegLen ~ (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

qqnorm(resid(SglMtLegRedMod), main = "SglMtLegRedMod") # fine
overdisp_fun(SglMtLegRedMod)# underdispersed
summary(SglMtLegRedMod)
anova(SglMtLegRedMod, SglMtLegMod1 )


###  Weight ##########
SglMtWeiMod1 <- lmer(logWeight ~ type + (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

qqnorm(resid(SglMtWeiMod1), main = "SglMtWeiMod1"); # very good fit
overdisp_fun(SglMtWeiMod1)# very over dispersed
summary(SglMtWeiMod1)
anova(SglMtWeiMod1)

### Testing against reduced model

SglMtWeiRedMod <- lmer(logWeight ~ (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

qqnorm(resid(SglMtWeiRedMod), main = "SglMtWeiRedMod") # fine
overdisp_fun(SglMtWeiRedMod)# underdispersed\
anova(SglMtWeiRedMod, SglMtWeiMod1 )

###################################################################################################
##### Stats of size using only 44.4 singe nests and the prob source nest 44.4EX03####################

Spis44 <- subset(spidersSglMt, km == "44.4")
Spis44 <- subset(Spis44, type == "single" | NestID == "44.4EX03")

SingMt44LegMod <- lmer(logLeg ~ type + (1|NestID), data = Spis44, REML = FALSE )

qqnorm(resid(SingMt44LegMod), main = "SglMtLegRedMod") # ok
overdisp_fun(SingMt44LegMod)# underdispersed
summary(SingMt44LegMod)

### testing against reduced model

SingMt44LegRedMod <- lmer(logLeg ~ 1 + (1|NestID), data = Spis44, REML = FALSE )

qqnorm(resid(SingMt44LegRedMod), main = "SingMt44WeiRedMod") # ok
overdisp_fun(SingMt44LegRedMod)# underdispersed
anova(SingMt44LegRedMod, SingMt44LegMod)


########## Weight #######################



SingMt44WeiMod <- lmer(logWeight ~ type + (1|NestID), data = Spis44, REML = FALSE )

qqnorm(resid(SingMt44WeiMod), main = "SingMt44WeiMod") # ok
overdisp_fun(SingMt44WeiMod)# ??? could be ok
summary(SingMt44WeiMod)

### testing against reduced model

SingMt44WeiRedMod <- lmer(logWeight ~ 1 + (1|NestID), data = Spis44, REML = FALSE )

qqnorm(resid(SingMt44WeiRedMod), main = "SingMt44LegRedMod") # ok
overdisp_fun(SingMt44WeiRedMod)# ??? could be ok
anova(SingMt44WeiRedMod, SingMt44WeiMod)


##################################################################
########## Leg Lenght vs nest size ###############################

### start by removing the single female nests

spidersMul <- subset(spiders, type == "multiple")

LegNestSzeMd1 <- lmer(logLeg ~ logCtFm*Instar + (1|NestID), data = spidersMul, REML = FALSE)

qqnorm(resid(LegNestSzeMd1), main = "LegNestSzeMd1") # mmm not so great
overdisp_fun(LegNestSzeMd1)# ??? very very underdispersed even when removing the interaction term
plot(fitted(LegNestSzeMd1), resid(LegNestSzeMd1),xlab = "fitted", ylab = "Residuals")

var(spidersMul$logLeg ~ Instar)

summary(LegNestSzeMd1)  # The interactions matter!!
coefplot2(LegNestSzeMd1) # not really sure that this shows me!
anova(LegNestSzeMd1)
plot(LegNestSzeMd1)
VarCorr(LegNestSzeMd1)

plot(spidersMul$LogCtFm, resid(LegNestSzeMd1))

### Taking out all nest size in the reduced model and testing against that

LegNestSzeRedMd <- lmer(logLeg ~ I(logCtFm^2)*Instar + (1|NestID), data = spidersMul, REML = FALSE)

qqnorm(resid(LegNestSzeRedMd), main = "LegNestSzeRedMd") # mmm not so great
overdisp_fun(LegNestSzeRedMd)# so over dispered, Not sure why or what is going on.
summary(LegNestSzeRedMd)

anova(LegNestSzeRedMd, LegNestSzeMd1)

qqnorm(ranef(LegNestSzeMd1)$NestID[[1]])
