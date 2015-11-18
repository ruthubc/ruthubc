# TODO: Add comment
# Statistica analysis for the nest size vs spider size 
# Author: Ruth
###############################################################################

#library (lme4)
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

anova(SglMtWeiRedMod, SglMtWeiMod1 ) # likihood ratio model


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


##########################################################################################################
########## Leg Length vs nest size ########################################################################



LegNestSzeMdNull <- lmer(logLeg ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

#getting AIC and p values and putting into list
#LegNestNull <- multipleModel(LegNestSzeMdNull, LegNestSzeMdNull)

LegNestSzeMd1 <- lmer(logLeg ~ I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(LegNestSzeMd1) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test

anova(LegNestSzeMd1)  # The interactions matter!!

LegNest1<- multipleModel(LegNestSzeMd1, LegNestSzeMdNull)

### Leg 2 removing squared term and interaction
LegNestSzeMd2 <- lmer(logLeg ~ logCtFm + Instar + logCtFm:Instar 
				 + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(LegNestSzeMd2) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test

anova(LegNestSzeMd2)  
summary(LegNestSzeMd2)

anova(LegNestSzeMd2, LegNestSzeMdNull)

anova(LegNestSzeMd2, LegNestSzeMd1) # of course will be significant beause of the interaction

LegNest2<- multipleModel(LegNestSzeMd2, LegNestSzeMdNull)

### Leg 3 removing interaction
LegNestSzeMd3 <- lmer(logLeg ~ logCtFm + Instar 
				+ (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(LegNestSzeMd3) # seems to be skwesnot sure it is so normal;not sure about the variances
# I could check the different variance with that test


anova(LegNestSzeMd3)  
summary(LegNestSzeMd3)

LegNest3<- multipleModel(LegNestSzeMd3, LegNestSzeMdNull)


LegTable<- as.data.frame(t(data.frame(LegNest1, LegNest2, LegNest3)))
colnames(LegTable)<-c("model", "AIC", "BIC", "pValue")

write.table(LegTable, file="RuthEcuador2013/NestSize/Graphs/LegNestSizeStats.csv", sep=",",row.names=F)



############### Testing  leg length vs nest size for each seperate instar

#Adult

LegNestAdultMd1 <- lmer(logLeg ~  logCtFm + (1|NestID), subset(spidersMul, Instar == "Adult"), REML = FALSE)

LegNestAdultMdRed <- lmer(logLeg ~  (1|NestID), subset(spidersMul, Instar == "Adult"), REML = FALSE)

anova(LegNestAdultMd1, LegNestAdultMdRed)

#Sub2

LegNestSub2Md1 <- lmer(logLeg ~  logCtFm + (1|NestID), subset(spidersMul, Instar == "Sub2"), REML = FALSE)

LegNestSub2MdRed <- lmer(logLeg ~  (1|NestID), subset(spidersMul, Instar == "Sub2"), REML = FALSE)

anova(LegNestSub2Md1, LegNestSub2MdRed)


#AdMale

LegNestAdMaleMd1 <- lmer(logLeg ~  logCtFm + (1|NestID), subset(spidersMul, Instar == "AdMale"), REML = FALSE)

LegNestAdMaleMdRed <- lmer(logLeg ~  (1|NestID), subset(spidersMul, Instar == "AdMale"), REML = FALSE)

anova(LegNestAdMaleMd1, LegNestAdMaleMdRed)

#Juv4

LegNestJuv4Md1 <- lmer(logLeg ~  logCtFm + (1|NestID), subset(spidersMul, Instar == "Juv4"), REML = FALSE)

LegNestJuv4MdRed <- lmer(logLeg ~  (1|NestID), subset(spidersMul, Instar == "Juv4"), REML = FALSE)

anova(LegNestJuv4Md1, LegNestJuv4MdRed)

#Sub1

LegNestSub1Md1 <- lmer(logLeg ~  logCtFm + (1|NestID), subset(spidersMul, Instar == "Sub1"), REML = FALSE)

anova(LegNestSub1Md1)

LegNestSub1MdRed <- lmer(logLeg ~  (1|NestID), subset(spidersMul, Instar == "Sub1"), REML = FALSE)

anova(LegNestSub1Md1, LegNestSub1MdRed)

#Juv4

LegNestJuv4Md1 <- lmer(logLeg ~ logCtFm + (1|NestID), subset(spidersMul, Instar == "Juv4"), REML = FALSE)

anova(LegNestJuv4Md1)

LegNestJuv4MdRed <- lmer(logLeg ~  (1|NestID), subset(spidersMul, Instar == "Juv4"), REML = FALSE)

anova(LegNestJuv4Md1, LegNestJuv4MdRed)

# SubMale

LegNestSubMaleMd1 <- lmer(logLeg ~ logCtFm + (1|NestID), subset(spidersMul, Instar == "SubMale"), REML = FALSE)


LegNestSubMaelMdRed <- lmer(logLeg ~  (1|NestID), subset(spidersMul, Instar == "SubMale"), REML = FALSE)

anova(LegNestSubMaleMd1, LegNestSubMaelMdRed)



########### Condition vs nest size ################################



WgtNestSzMdNull <-lmer(conditionSq ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

##full model
WgtNestSzMd1 <- lmer(conditionSq ~ I(logCtFm^2)+ logCtFm + Instar +logCtFm:Instar + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(WgtNestSzMd1) # all seems good!
anova(WgtNestSzMd1)  # The interactions matter!!

WgtNest1<- multipleModel(WgtNestSzMd1,WgtNestSzMdNull)


##removing fem ct squared and interaction
WgtNestSzMd2 <- lmer(conditionSq ~ logCtFm + Instar +logCtFm:Instar 
				 + (1|NestID), data = spidersMul, REML = FALSE)

modelPlot(WgtNestSzeMd2) # all seems good!
anova(WgtNestSzMd2)  # The interactions matter!!

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

############### Testing individual instars Hung length vs nest size

#Adult

HungNestAdultMd1 <- lmer(conditionSq ~   logCtFm + (1|NestID), subset(spidersMul, Instar == "Adult"), REML = FALSE)

anova(HungNestAdultMd1)
HungNestAdultMdRed <- lmer(logHung ~  (1|NestID), subset(spidersMul, Instar == "Adult"), REML = FALSE)

anova(HungNestAdultMd1, HungNestAdultMdRed)

#Sub2

HungNestSub2Md1 <- lmer(conditionSq ~  logCtFm + (1|NestID), subset(spidersMul, Instar == "Sub2"), REML = FALSE)
anova(HungNestSub2Md1)

HungNestSub2MdRed <- lmer(logHung ~  (1|NestID), subset(spidersMul, Instar == "Sub2"), REML = FALSE)

anova(HungNestSub2Md1, HungNestSub2MdRed)


#AdMale

HungNestAdMaleMd1 <- lmer(logHung ~  logCtFm + (1|NestID), subset(spidersMul, Instar == "AdMale"), REML = FALSE)

HungNestAdMaleMdRed <- lmer(logHung ~  (1|NestID), subset(spidersMul, Instar == "AdMale"), REML = FALSE)

anova(HungNestAdMaleMd1, HungNestAdMaleMdRed)

#Juv4

HungNestJuv4Md1 <- lmer(logHung ~  logCtFm + (1|NestID), subset(spidersMul, Instar == "Juv4"), REML = FALSE)

HungNestJuv4MdRed <- lmer(logHung ~  (1|NestID), subset(spidersMul, Instar == "Juv4"), REML = FALSE)

anova(HungNestJuv4Md1, HungNestJuv4MdRed)

#Sub1

HungNestSub1Md1 <- lmer(logHung ~ I(logCtFm^2)+  logCtFm + (1|NestID), subset(spidersMul, Instar == "Sub1"), REML = FALSE)

anova(HungNestSub1Md1)

HungNestSub1MdRed <- lmer(logHung ~  (1|NestID), subset(spidersMul, Instar == "Sub1"), REML = FALSE)

anova(HungNestSub1Md1, HungNestSub1MdRed)

#Juv4

HungNestJuv4Md1 <- lmer(logHung ~ I(logCtFm^2)+  logCtFm + (1|NestID), subset(spidersMul, Instar == "Juv4"), REML = FALSE)

anova(HungNestJuv4Md1)

HungNestJuv4MdRed <- lmer(logHung ~  (1|NestID), subset(spidersMul, Instar == "Juv4"), REML = FALSE)

anova(HungNestJuv4Md1, HungNestJuv4MdRed)



################################# CV of Leg by nest size###############################################


cvLegMod1<- lmer(logcvByNLeg ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar + (1|NestID), data = SpiNestAve, REML = FALSE)

modelPlot(cvLegMod1)

anova(cvLegMod1)

##testing individual terms
### removing square interation term and testing

cvLegMod2a<- lmer(logcvByNLeg ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				 (1|NestID), data = SpiNestAve,  REML = FALSE)
 
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
 anova(cvLegMod3a, cvLegMod7a)
 
 
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


################ Testing seperate instars ####################################################


###Adult
cvLegAdultMod1<- lm(logcvByNLeg ~ I(logCtFm^2) + logCtFm, subset(SpiNestAve, Instar =="Adult"))

anova(cvLegAdultMod1)

cvLegAdultModSq1<- lm(logcvByNLeg ~ I(logCtFm^2), subset(SpiNestAve, Instar =="Adult"))
cvLegAdultModRed<- lm(logcvByNLeg ~ 1, subset(SpiNestAve, Instar =="Adult"))

anova(cvLegAdultModSq1, cvLegAdultModRed)

###Sub2
cvLegSub2Mod1<- lm(logcvByNLeg ~ I(logCtFm^2) + logCtFm, subset(SpiNestAve, Instar =="Sub2"))

anova(cvLegSub2Mod1)

cvLegSub2ModSq<- lm(logcvByNLeg ~ I(logCtFm^2), subset(SpiNestAve, Instar =="Sub2"))
cvLegSub2ModRed<- lm(logcvByNLeg ~ 1, subset(SpiNestAve, Instar =="Sub2"))

anova(cvLegSub2ModSq, cvLegSub2ModRed)

cvLegSub2ModNS<- lm(logcvByNLeg ~ logCtFm, subset(SpiNestAve, Instar =="Sub2"))

anova(cvLegSub2ModNS, cvLegSub2ModRed)

###Sub1
cvLegSub1Mod1<- lm(logcvByNLeg ~ I(logCtFm^2) + logCtFm, subset(SpiNestAve, Instar =="Sub1"))

anova(cvLegSub1Mod1)

cvLegSub1ModSq<- lm(logcvByNLeg ~ I(logCtFm^2), subset(SpiNestAve, Instar =="Sub1"))
cvLegSub1ModRed<- lm(logcvByNLeg ~ 1, subset(SpiNestAve, Instar =="Sub1"))

anova(cvLegSub1ModSq, cvLegSub1ModRed)

cvLegSub1ModNS<- lm(logcvByNLeg ~ logCtFm, subset(SpiNestAve, Instar =="Sub1"))

anova(cvLegSub1ModNS, cvLegSub1ModRed)

###AdMale
cvLegAdMaleMod1<- lm(logcvByNLeg ~ I(logCtFm^2) + logCtFm, subset(SpiNestAve, Instar =="AdMale"))

anova(cvLegAdMaleMod1)
anova(cvLegAdMaleMod1, cvLegMaleMod)

cvLegAdMaleModSq<- lm(logcvByNLeg ~ I(logCtFm^2), subset(SpiNestAve, Instar =="AdMale"))
cvLegAdMaleModRed<- lm(logcvByNLeg ~ 1, subset(SpiNestAve, Instar =="AdMale"))

anova(cvLegAdMaleMod1, cvLegAdMaleModRed)
anova(cvLegAdMaleModSq, cvLegAdMaleModRed)

cvLegAdMaleModNS<- lm(logcvByNLeg ~ logCtFm, subset(SpiNestAve, Instar =="AdMale"))

anova(cvLegAdMaleModNS, cvLegAdMaleModRed)
anova(cvLegAdMaleModNS, cvLegAdMaleMod1)

###Juv4
cvLegJuv4Mod1<- lm(logcvByNLeg ~ I(logCtFm^2) + logCtFm, subset(SpiNestAve, Instar =="Juv4"))

anova(cvLegJuv4Mod1)
anova(cvLegJuv4Mod1, cvLegMaleMod)

cvLegJuv4ModSq<- lm(logcvByNLeg ~ I(logCtFm^2), subset(SpiNestAve, Instar =="Juv4"))
cvLegJuv4ModRed<- lm(logcvByNLeg ~ 1, subset(SpiNestAve, Instar =="Juv4"))

anova(cvLegJuv4Mod1, cvLegJuv4ModRed)
anova(cvLegJuv4ModSq, cvLegJuv4ModRed)

cvLegJuv4ModNS<- lm(logcvByNLeg ~ logCtFm, subset(SpiNestAve, Instar =="Juv4"))

anova(cvLegJuv4ModNS, cvLegJuv4ModRed)
anova(cvLegJuv4ModNS, cvLegJuv4Mod1)



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

######### CV of hunger by individual instar

##Adult
cvHungModAdultFull<- lm(cvByNHung ~ I(logCtFm^2) + logCtFm, subset(SpiNestAve, Instar == "Adult"))
anova(cvHungModAdultFull)

##Sub2
cvHungModSub2Full<- lm(cvByNHung ~ I(logCtFm^2) + logCtFm, subset(SpiNestAve, Instar == "Sub2"))
anova(cvHungModSub2Full)


##Sub1


cvHungModSub1Red<- lm(cvByNHung ~ 1, subset(SpiNestAve, Instar == "Sub1"))

cvHungModSub1Full<- lm(cvByNHung ~  logCtFm + I(logCtFm^2) , subset(SpiNestAve, Instar == "Sub1"))
anova(cvHungModSub1Full)

anova(cvHungModSub1Full, cvHungModSub1Red)

cvHungModSub1Sq<- lm(cvByNHung ~ I(logCtFm^2), subset(SpiNestAve, Instar == "Sub1"))
anova(cvHungModSub1Sq)
anova( cvHungModSub1Full, cvHungModSub1Sq)
anova(cvHungModSub1Sq,  cvHungModSub1Red) # don't understand why this is not significant



cvHungModSub1NS<- lm(cvByNHung ~  logCtFm, subset(SpiNestAve, Instar == "Sub1"))
anova(cvHungModSub1Full, cvHungModSub1NS)

##AdMale


cvHungModAdMaleRed<- lm(cvByNHung ~ 1, subset(SpiNestAve, Instar == "AdMale"))

cvHungModAdMaleFull<- lm(cvByNHung ~  logCtFm + I(logCtFm^2) , subset(SpiNestAve, Instar == "AdMale"))
anova(cvHungModAdMaleFull)

anova(cvHungModAdMaleFull, cvHungModAdMaleRed)

cvHungModAdMaleSq<- lm(cvByNHung ~ I(logCtFm^2), subset(SpiNestAve, Instar == "AdMale"))
anova(cvHungModAdMaleSq)
anova( cvHungModAdMaleFull, cvHungModAdMaleSq)
anova(cvHungModAdMaleSq,  cvHungModAdMaleRed) # don't understand why this is not significant



cvHungModAdMaleNS<- lm(cvByNHung ~  logCtFm, subset(SpiNestAve, Instar == "AdMale"))
anova(cvHungModAdMaleFull, cvHungModAdMaleNS)

##Juv4


cvHungModJuv4Red<- lm(cvByNHung ~ 1, subset(SpiNestAve, Instar == "Juv4"))

cvHungModJuv4Full<- lm(cvByNHung ~  logCtFm + I(logCtFm^2) , subset(SpiNestAve, Instar == "Juv4"))
anova(cvHungModJuv4Full)

anova(cvHungModJuv4Full, cvHungModJuv4Red)

cvHungModJuv4Sq<- lm(cvByNHung ~ I(logCtFm^2), subset(SpiNestAve, Instar == "Juv4"))
anova(cvHungModJuv4Sq)
anova( cvHungModJuv4Full, cvHungModJuv4Sq)
anova(cvHungModJuv4Sq,  cvHungModJuv4Red) 



cvHungModJuv4NS<- lm(cvByNHung ~  logCtFm, subset(SpiNestAve, Instar == "Juv4"))
anova(cvHungModJuv4Full, cvHungModJuv4NS)

##Juv4


cvHungModJuv4Red<- lm(cvByNHung ~ 1, subset(SpiNestAve, Instar == "Juv4"))

cvHungModJuv4Full<- lm(cvByNHung ~  logCtFm + I(logCtFm^2) , subset(SpiNestAve, Instar == "Juv4"))
anova(cvHungModJuv4Full)

anova(cvHungModJuv4Full, cvHungModJuv4Red)

cvHungModJuv4Sq<- lm(cvByNHung ~ I(logCtFm^2), subset(SpiNestAve, Instar == "Juv4"))
anova(cvHungModJuv4Sq)
anova( cvHungModJuv4Full, cvHungModJuv4Sq)
anova(cvHungModJuv4Sq,  cvHungModJuv4Red) 


cvHungModJuv4NS<- lm(cvByNHung ~  logCtFm, subset(SpiNestAve, Instar == "Juv4"))
anova(cvHungModJuv4Full, cvHungModJuv4NS)


