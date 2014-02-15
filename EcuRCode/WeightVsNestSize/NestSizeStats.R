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

############################## All nests ############################3##########
spidersSglMt <- subset(spiders, Instar == "Adult")

#removing single spiders that don't have juvs or eggs to make sure that the higher weight isn't because they are gravid
spidersSglMt <- subset(spidersSglMt , FemalesHaveEggsOrJuvs != "n")

### Leg Length for all nests ##########
SglMtLegMod1 <- lmer(LnLegLen ~ type + (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

modelPlot(SglMtLegMod1) # not sure so normal but variance seems good
summary(SglMtLegMod1)
anova(SglMtLegMod1)

### Testing against reduced model
SglMtLegRedMod <- lmer(LnLegLen ~ (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

modelPlot(SglMtLegRedMod) # same as for the none reduced model
summary(SglMtLegRedMod)

anova(SglMtLegRedMod, SglMtLegMod1 )


###  Weight  for all nests ##########
SglMtWeiMod1 <- lmer(logWeight ~ type + (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)

modelPlot(SglMtWeiMod1) # very normal and good variance
summary(SglMtWeiMod1)


### Testing against reduced model

SglMtWeiRedMod <- lmer(logWeight ~ (1|km) + (1|km:NestID), spidersSglMt, REML = FALSE)
modelPlot(SglMtWeiRedMod) # same for full model

anova(SglMtWeiRedMod, SglMtWeiMod1 )


################## Only 44.4 singe nests and the prob source nest 44.4EX03####################

Spis44 <- subset(spidersSglMt, km == "44.4")
Spis44 <- subset(Spis44, type == "single" | NestID == "44.4EX03")

####### Leg length for 44.4 nests ########

SingMt44LegMod <- lmer(logLeg ~ type + (1|NestID), data = Spis44, REML = FALSE )

modelPlot(SingMt44LegMod) # not so normal and not good variances
summary(SingMt44LegMod)

### testing against reduced model

SingMt44LegRedMod <- lmer(logLeg ~ 1 + (1|NestID), data = Spis44, REML = FALSE )

modelPlot(SingMt44LegRedMod) # variance not good and not so normal
anova(SingMt44LegRedMod, SingMt44LegMod)


######################### Weight vs single/multiple #######################

SingMt44WeiMod <- lmer(logWeight ~ type + (1|NestID), data = Spis44, REML = FALSE )

modelPlot(SingMt44WeiMod) # normal, variances not great but not good
summary(SingMt44WeiMod)

### testing against reduced model

SingMt44WeiRedMod <- lmer(logWeight ~ 1 + (1|NestID), data = Spis44, REML = FALSE )

modelPlot(SingMt44WeiRedMod) # normal but variances not great
anova(SingMt44WeiRedMod, SingMt44WeiMod)


##################################################################
########## Leg Length vs nest size ###############################

spidersMul <- subset(spiders, type == "multiple") #removing single females

LegNestSzeMdNull <- lmer(logLeg ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

#getting AIC and p values and putting into list
LegNestNull <- multipleModel(LegNestSzeMdNull, LegNestSzeMdNull)

LegNestSzeMd1 <- lmer(logLeg ~ I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(LegNestSzeMd1) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test

anova(LegNestSzeMd1)  # The interactions matter!!
summary(LegNestSzeMd1)

LegNest1<- multipleModel(LegNestSzeMd1, LegNestSzeMdNull)

### Leg 2 removing squared term and interaction
LegNestSzeMd2 <- lmer(logLeg ~ logCtFm + Instar + logCtFm:Instar 
				 + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(LegNestSzeMd2) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test

anova(LegNestSzeMd2)  
summary(LegNestSzeMd2)

LegNest2<- multipleModel(LegNestSzeMd2, LegNestSzeMdNull)

### Leg 3 removing interaction
LegNestSzeMd3 <- lmer(logLeg ~ logCtFm + Instar 
				+ (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(LegNestSzeMd3) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test

anova(LegNestSzeMd3)  
summary(LegNestSzeMd3)

LegNest3<- multipleModel(LegNestSzeMd3, LegNestSzeMdNull)

LegColNames<-c("null", "Full", "No Size Sq", "No Interaction")
LegTable<- data.frame(LegColNames, LegNestNull, LegNest1, LegNest2, LegNest3)

##drop 1..not sure how useful this is
drop1(LegNestSzeMd1, scope ~ I(logCtFm^2):logCtFm:Instar, test = "Chi") #sig p = 0.00058	
drop1(LegNestSzeMd1, scope ~ I(logCtFm^2):logCtFm, test = "Chi") # NOT sig 
drop1(LegNestSzeMd1, scope ~ logCtFm:Instar, test = "Chi") # sig p = 0.0028
drop1(LegNestSzeMd1, scope ~ I(logCtFm^2):Instar, test = "Chi") # sig p = 0.001238
drop1(LegNestSzeMd1, scope ~ I(logCtFm^2), test = "Chi")# sig p = 0.0138
drop1(LegNestSzeMd1, scope ~ logCtFm, test = "Chi") # sig p = 0.02566
drop1(LegNestSzeMd1, scope ~ Instar, test = "Chi") # sig p = 0.00197


######### Testing 3-way interaction by reduced model

LegNestSzeRedMd3W <- lmer(logLeg ~ logCtFm*Instar + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(LegNestSzeRedMd3W) #same as full mod
summary(LegNestSzeRedMd3W)
anova(LegNestSzeRedMd3W)

anova(LegNestSzeRedMd3W, LegNestSzeMd1) # three way interactions are significant


########### Weight vs nest size ################################
spidersMul <- subset(spiders, type == "multiple") #removing single females

WeightNestSzMdNull <-lmer(logWeight ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

WeightNestSzeMd1 <- lmer(logWeight ~ I(logCtFm^2)*logCtFm*Instar + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(WeightNestSzeMd1) # all seems good! Yay!

anova(WeightNestSzeMd1)  # The interactions matter!!

modsum <- summary(WeightNestSzeMd1)

anova(WeightNestSzMdNull, WeightNestSzeMd1)

modsum$formula
as.character(WeightNestSzeMd1@call[2])


AIC(WeightNestSzeMd1)
BIC(WeightNestSzeMd1)



drop1(WeightNestSzeMd1, scope ~ I(logCtFm^2):logCtFm:Instar, test = "Chi") #sig p = 0.0257
drop1(WeightNestSzeMd1, scope ~ I(logCtFm^2):logCtFm, test = "Chi") # NOT sig although not sure about this
drop1(WeightNestSzeMd1, scope ~ logCtFm:Instar, test = "Chi") # sig p = 0.04 but not massively so
drop1(WeightNestSzeMd1, scope ~ I(logCtFm^2):Instar, test = "Chi") # sig p = 0.03
drop1(WeightNestSzeMd1, scope ~ I(logCtFm^2), test = "Chi")# sig p = 0.03069
drop1(WeightNestSzeMd1, scope ~ logCtFm, test = "Chi") # NOT sig p = 0.2845, perhpas we only need the square term in there
drop1(WeightNestSzeMd1, scope ~ Instar, test = "Chi") # sig p = 0.03821

