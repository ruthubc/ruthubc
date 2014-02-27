# TODO: Add comment
# Statistica analysis for the nest size vs spider size 
# Author: Ruth
###############################################################################

library (lme4)
library(lmerTest) # this puts pvalue in lmer
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

HungNestSzeMdNull <- lmer(logLeg ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

#getting AIC and p values and putting into list
#HungNestNull <- multipleModel(HungNestSzeMdNull, HungNestSzeMdNull)

HungNestSzeMd1 <- lmer(logLeg ~ I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(HungNestSzeMd1) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test

anova(HungNestSzeMd1)  # The interactions matter!!
summary(HungNestSzeMd1)

HungNest1<- multipleModel(HungNestSzeMd1, HungNestSzeMdNull)

### Leg 2 removing squared term and interaction
HungNestSzeMd2 <- lmer(logLeg ~ logCtFm + Instar + logCtFm:Instar 
				 + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(HungNestSzeMd2) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test

anova(HungNestSzeMd2)  
summary(HungNestSzeMd2)

HungNest2<- multipleModel(HungNestSzeMd2, HungNestSzeMdNull)

### Leg 3 removing interaction
HungNestSzeMd3 <- lmer(logLeg ~ logCtFm + Instar 
				+ (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(HungNestSzeMd3) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test


anova(HungNestSzeMd3)  
summary(HungNestSzeMd3)

HungNest3<- multipleModel(HungNestSzeMd3, HungNestSzeMdNull)


LegTable<- as.data.frame(t(data.frame(HungNest1, HungNest2, HungNest3)))
colnames(LegTable)<-c("model", "AIC", "BIC", "pValue")

write.table(LegTable, file="RuthEcuador2013/NestSize/Graphs/HungNestSizeStats.csv", sep=",",row.names=F)

##drop 1..not sure how useful this is
drop1(HungNestSzeMd1, scope ~ I(logCtFm^2):logCtFm:Instar, test = "Chi") #sig p = 0.00058	
drop1(HungNestSzeMd1, scope ~ I(logCtFm^2):logCtFm, test = "Chi") # NOT sig 
drop1(HungNestSzeMd1, scope ~ logCtFm:Instar, test = "Chi") # sig p = 0.0028
drop1(HungNestSzeMd1, scope ~ I(logCtFm^2):Instar, test = "Chi") # sig p = 0.001238
drop1(HungNestSzeMd1, scope ~ I(logCtFm^2), test = "Chi")# sig p = 0.0138
drop1(HungNestSzeMd1, scope ~ logCtFm, test = "Chi") # sig p = 0.02566
drop1(HungNestSzeMd1, scope ~ Instar, test = "Chi") # sig p = 0.00197


############### Testing Leg Length Nest Size individual terms


########### Weight vs nest size ################################



WgtNestSzMdNull <-lmer(logWeight ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

##full model
WgtNestSzMd1 <- lmer(logWeight ~ I(logCtFm^2)+ logCtFm + Instar +logCtFm:Instar + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(WgtNestSzMd1) # all seems good!
anova(WgtNestSzMd1)  # The interactions matter!!

WgtNest1<- multipleModel(WgtNestSzMd1,WgtNestSzMdNull)


##removing fem ct squared and interaction
WgtNestSzMd2 <- lmer(logWeight ~ logCtFm + Instar +logCtFm:Instar 
				 + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(WgtNestSzeMd2) # all seems good!
anova(WgtNestSzeMd2)  # The interactions matter!!

WgtNest2<- multipleModel(WgtNestSzMd2,WgtNestSzMdNull)

##removing interaction
WgtNestSzMd3 <- lmer(logWeight ~ logCtFm + Instar 
				+ (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(WgtNestSzeMd3) # all seems good!
anova(WgtNestSzeMd3)  # The interactions matter!!

WgtNest3<- multipleModel(WgtNestSzMd3,WgtNestSzMdNull)



## making table of different model values

WgtTable<- as.data.frame(t(data.frame(WgtNest1, WgtNest2, WgtNest3)))
colnames(WgtTable)<-c("model", "AIC", "BIC", "pValue")
write.table(WgtTable, file="RuthEcuador2013/NestSize/Graphs/WeightNestSizeStats.csv", sep=",",row.names=F)


### Hunger vs nest size

HunNestSzMdNull <-lmer(logHung ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

##full model
HunNestSzMd1 <- lmer(logHung ~ I(logCtFm^2)+ logCtFm + Instar +logCtFm:Instar + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

HunNest1<-multipleModel(HunNestSzMd1, HunNestSzMdNull)

##removing logCtFm^2 and interaction
HunNestSzMd2 <- lmer(logHung ~ logCtFm + Instar +logCtFm:Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

HunNest2<-multipleModel(HunNestSzMd2, HunNestSzMdNull)

##removing interaction
HunNestSzMd3<- lmer(logHung ~ logCtFm + Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

HunNest3<-multipleModel(HunNestSzMd3, HunNestSzMdNull)

## making table of different model values
HunTable<- as.data.frame(t(data.frame(HunNest1, HunNest2, HunNest3)))
colnames(HunTable)<-c("model", "AIC", "BIC", "pValue")


#### Hunger vs nest size individual terms #######################

HungNestSzeMd1 <- lmer(logHung ~ I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

#removed squared interactin term
HungNestSzeMd2a <- lmer(logHung ~ I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

anova(HungNestSzeMd1, HungNestSzeMd2a)
# removed all squared terms
HungNestSzeMd3a <- lmer(logHung ~ logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

anova(HungNestSzeMd1, HungNestSzeMd3a)

## removing non squared interaction terms

HungNestSzeMd4a <- lmer(logHung ~ I(logCtFm^2) + logCtFm + Instar  + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

anova(HungNestSzeMd1, HungNestSzeMd4a)

## removing all non squared interaction terms

HungNestSzeMd5a <- lmer(logHung ~ I(logCtFm^2) + Instar  + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

anova(HungNestSzeMd1, HungNestSzeMd5a)

modelPlot(HungNestSzeMd5a)



############# CV of Leg by nest size####


cvLegMod1<- lmer(logcvByNLeg ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar + (1|NestID), data = SpiNestAve, REML = FALSE)

modelPlot(cvLegMod1)

anova(cvLegMod1)

##testing individual terms
### removing square interation term and testing

cvLegMod2a<- lmer(logcvByNLeg ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				 (1|NestID), data = SpiNestAve, REML = FALSE)
 
 anova(cvLegMod1, cvLegMod2a)
 
 ## removing all square terms
 cvLegMod3a<- lmer(logcvByNLeg ~  logCtFm + Instar+ Instar:logCtFm + 
				 (1|NestID), data = SpiNestAve, REML = FALSE)
 
 anova(cvLegMod1, cvLegMod3a)

#testing instar:logfme
 cvLegMod4a<- lmer(logcvByNLeg ~  logCtFm + Instar+ 
				 (1|NestID), data = SpiNestAve, REML = FALSE)
 
 anova(cvLegMod1, cvLegMod4a)
 anova(cvLegMod3a, cvLegMod4a)
 
 
 #testing count fem
 cvLegMod5a<- lmer(logcvByNLeg ~  Instar + (1|NestID), data = SpiNestAve, REML = FALSE)
 
 anova(cvLegMod1, cvLegMod5a)
 anova(cvLegMod3a, cvLegMod5a)
 
 #testing instar
 cvLegMod6a<- lmer(logcvByNLeg ~  I(logCtFm^2) + logCtFm + (1|NestID), data = SpiNestAve, REML = FALSE)
 
 anova(cvLegMod1, cvLegMod6a)

 ##testing instar against just log CtFm
 cvLegMod7a<- lmer(logcvByNLeg ~ logCtFm + (1|NestID), data = SpiNestAve, REML = FALSE)
 
 anova(cvLegMod1, cvLegMod7a)
 
 
 ##testing instar against completely null model
 cvLegMod8a<- lmer(logcvByNLeg ~ Instar+ (1|NestID), data = SpiNestAve, REML = FALSE)
 cvLegModComNull<- lmer(logcvByNLeg ~ (1|NestID), data = SpiNestAve, REML = FALSE)
 anova(cvLegModComNull, cvLegMod8a)


cvLegModNull<- lmer(logcvByNLeg ~ Instar+ (1|NestID), data = LegCV, REML = FALSE)

anova(cvLegModNull)
anova(cvLegModNull, cvLegMod1 )

cvLegMod2 <- lmer(logcvByNLeg ~ logCtFm + Instar+ Instar:logCtFm +
				+ (1|NestID), data = LegCV, REML = FALSE)

anova(cvLegMod2)
anova(cvLegModNull, cvLegMod2 )

cvLegMod3 <- lmer(logcvByNLeg ~ logCtFm + Instar
				+ (1|NestID) + (1|N), data = LegCV, REML = FALSE)

anova(cvLegMod3)

anova(cvLegMod3, cvLegMod2)

######## CV of weight by nest size #######

cvWgtModNull<- lmer(logCVByNWei ~ Instar + (1|NestID) + (1|N), data = SpiNestAve, REML = FALSE)

cvWgtMod1<- lmer(logCVByNWei ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar + (1|NestID) + (1|N), data = SpiNestAve, REML = FALSE)

drop1(cvWgtMod1, test = "Chi", scope = ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar)

modelPlot(cvWgtMod1)
summary(cvWgtMod1)
anova(cvWgtMod1)

multipleModel(cvWgtMod1, cvWgtModNull)

cvWgtMod2<- lmer(logCVByNWei ~ logCtFm + Instar+ Instar:logCtFm + (1|NestID) + (1|N), data = SpiNestAve, REML = FALSE)
anova(cvWgtMod2)
multipleModel(cvWgtMod2, cvWgtModNull)

anova(cvWgtMod2, cvWgtMod1)

cvWgtMod2_5<- lmer(logCVByNWei ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + (1|NestID) + (1|N), data = SpiNestAve, REML = FALSE)

anova(cvWgtMod2_5, cvWgtMod1)

cvWgtMod3<- lmer(logCVByNWei ~ logCtFm + Instar+ (1|NestID) + (1|N), data = SpiNestAve, REML = FALSE)
anova(cvWgtMod3)
multipleModel(cvWgtMod3, cvWgtModNull)

cvWgtMod4<- lmer(logCVByNWei ~ I(logCtFm^2) + Instar + 
				I(logCtFm^2):Instar + (1|NestID) + (1|N), data = SpiNestAve, REML = FALSE)

anova(cvWgtMod4)
multipleModel(cvWgtMod4, cvWgtModNull)

######## CV of hunger by nest size #######


cvHungMod1<- lmer(cvByNHung ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar + (1|NestID), data = SpiNestAve, REML = FALSE)

modelPlot(cvHungMod1)

anova(cvHungMod1)

##testing individual terms
### removing square interation term and testing

cvHungMod2a<- lmer(cvByNHung ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				(1|NestID), data = SpiNestAve, REML = FALSE)

anova(cvHungMod1, cvHungMod2a)

## removing all square terms
cvHungMod3a<- lmer(logcvByNHung ~  logCtFm + Instar+ Instar:logCtFm + 
				(1|NestID), data = SpiNestAve, REML = FALSE)

anova(cvHungMod1, cvHungMod3a)

#testing instar:logfme
cvHungMod4a<-  lmer(cvByNHung ~ I(logCtFm^2) + logCtFm + Instar+ 
				I(logCtFm^2):Instar + (1|NestID), data = SpiNestAve, REML = FALSE)


anova(cvHungMod1, cvHungMod4a)
anova(cvHungMod3a, cvHungMod4a)


#testing count fem and interaction effect
cvHungMod5a<- lmer(logcvByNHung ~ I(logCtFm^2) + I(logCtFm^2):Instar + Instar+ (1|NestID), data = SpiNestAve, REML = FALSE)

anova(cvHungMod1, cvHungMod5a)
anova(cvHungMod3a, cvHungMod5a)

#testing instar
cvHungMod6a<- lmer(logcvByNHung ~  I(logCtFm^2) + logCtFm + (1|NestID), data = SpiNestAve, REML = FALSE)

anova(cvHungMod1, cvHungMod6a)

##testing instar against just log CtFm
cvHungMod7a<- lmer(logcvByNHung ~ logCtFm + (1|NestID), data = SpiNestAve, REML = FALSE)

anova(cvHungMod1, cvHungMod7a)


##testing instar against completely null model
cvHungMod8a<- lmer(logcvByNHung ~ Instar+ (1|NestID), data = SpiNestAve, REML = FALSE)
cvHungModComNull<- lmer(logcvByNHung ~ (1|NestID), data = SpiNestAve, REML = FALSE)
anova(cvHungModComNull, cvHungMod8a)