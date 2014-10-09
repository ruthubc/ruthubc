# TODO: Add comment
# 
# Author: Ruth
###############################################################################

################## Testing condition vs capture overall

### getting percentages

CapAtAllvsCond<- aggregate((1/BoxComboAve$Hunger), by = list(BoxComboAve$Feed, BoxComboAve$Treatment, BoxComboAve$Instar), 
		FUN = mean, na.rm=TRUE)
CapAtAllvsCond

HungDiff(CapAtAllvsCond)

BoxAveCap <- subset(BoxComboAve, Cap != "NA"  & LogCond != "NA")

CapConFull<- lmer(LogCond ~ Cap:Treatment + Cap+Treatment+Instar  +
				(1|IndBoxID), BoxAveCap, REML = FALSE)

summary(CapConFull)

CapCon1<- lmer(LogCond ~ Cap +Treatment+Instar  +
				(1|IndBoxID), BoxAveCap, REML = FALSE)

anova(CapCon1, CapConFull)

CapCon2<- lmer(LogCond ~  Treatment+Instar  +
				(1|IndBoxID), BoxAveCap, REML = FALSE)

anova(CapCon2, CapConFull)

CapCon3<- lmer(LogCond ~  Cap +Instar  +
				(1|IndBoxID), BoxAveCap, REML = FALSE)

anova(CapCon3, CapConFull)

#################Testing condition individually with capture lmer and other way around! ########


### Sub1 & Large

CapSub1LgFull <- lmer(LogCond ~ Cap + (1|IndBoxID), subset(BoxAveCap, 
				Instar == "Sub1" & Treatment == "large"), REML = FALSE   )

modelPlot(CapSub1LgFull)
summary(CapSub1LgFull)

CapSub1LgRed <- lmer(LogCond ~ 1+ (1|IndBoxID), subset(BoxAveCap, 
				Instar == "Sub1" & Treatment == "large"), REML = FALSE    )

anova(CapSub1LgFull, CapSub1LgRed)

### Sub2 & Large

CapSub2LgFull <- lmer(LogCond ~ Cap + (1|IndBoxID), subset(BoxAveCap, 
				Instar == "Sub2" & Treatment == "large"), REML = FALSE   )

summary(CapSub2LgFull)

CapSub2LgRed <- lmer(LogCond ~ 1+ (1|IndBoxID), subset(BoxAveCap, 
				Instar == "Sub2" & Treatment == "large"), REML = FALSE    )


anova(CapSub2LgFull, CapSub2LgRed)


### Sub1 & small

CapSub1SmFull <- lmer(LogCond ~ Cap + (1|IndBoxID), subset(BoxAveCap, 
				Instar == "Sub1" & Treatment == "small"), REML = FALSE   )

summary(CapSub1SmFull)

modelPlot(CapSub1SmFull)

CapSub1SmRed <- lmer(LogCond ~ 1+ (1|IndBoxID), subset(BoxAveCap, 
				Instar == "Sub1" & Treatment == "small"), REML = FALSE    )

anova(CapSub1SmFull, CapSub1SmRed)

### Sub2 & small

CapSub2SmFull <- lmer(LogCond ~ Cap + (1|IndBoxID), subset(BoxAveCap, 
				Instar == "Sub2" & Treatment == "small"), REML = FALSE   )

modelPlot(CapSub2SmFull)
summary(CapSub2SmFull)

CapSub2SmRed <- lmer(LogCond ~ 1+ (1|IndBoxID), subset(BoxAveCap, 
				Instar == "Sub2" & Treatment == "small"), REML = FALSE    )


anova(CapSub2SmFull, CapSub2SmRed)

############################################TESTING IND INSTARS AND PREY SIZES ###########

## sub1 and large prey

EatBinlargeSub1 <- glmer(IndCapture ~ LogCond + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub1" & Treatment == "large")), 
		family = binomial(logit))

EatBinlargeSub1Red <- glmer(IndCapture ~(1|IndBoxID) + (1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub1" & Treatment == "large")), 
		family = binomial(logit))	

anova(EatBinlargeSub1, EatBinlargeSub1Red)

## sub1 and small prey

EatBinsmallSub1 <- glmer(IndCapture ~ LogCond + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub1" & Treatment == "small")), 
		family = binomial(logit))

EatBinsmallSub1Red <- glmer(IndCapture ~(1|IndBoxID) + (1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub1" & Treatment == "small")), 
		family = binomial(logit))	

anova(EatBinsmallSub1, EatBinsmallSub1Red)

## Sub2 and large prey

EatBinlargeSub2 <- glmer(IndCapture ~ LogCond + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub2" & Treatment == "large")), 
		family = binomial(logit))

EatBinlargeSub2Red <- glmer(IndCapture ~(1|IndBoxID) + (1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub2" & Treatment == "large")), 
		family = binomial(logit))	

anova(EatBinlargeSub2, EatBinlargeSub2Red)

## Sub2 and small prey

EatBinsmallSub2 <- glmer(IndCapture~ LogCond + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub2" & Treatment == "small")), 
		family = binomial(logit))

EatBinsmallSub2Red <- glmer(IndCapture ~(1|IndBoxID) + (1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub2" & Treatment == "small")), 
		family = binomial(logit))	

anova(EatBinsmallSub2, EatBinsmallSub2Red)


#########Testing individuals capture vs hunger

## sub1 and large prey

CapBinlargeSub1 <- glmer(IndCapture ~ LogHunger + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub1" & Treatment == "large")), 
		family = binomial(logit))

CapBinlargeSub1Red <- glmer(IndCapture ~(1|IndBoxID) + (1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub1" & Treatment == "large")), 
		family = binomial(logit))	

anova(CapBinlargeSub1, CapBinlargeSub1Red)

## sub1 and small prey

CapBinsmallSub1 <- glmer(IndCapture ~ LogHunger + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub1" & Treatment == "small")), 
		family = binomial(logit))

CapBinsmallSub1Red <- glmer(IndCapture ~(1|IndBoxID) + (1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub1" & Treatment == "small")), 
		family = binomial(logit))	

anova(CapBinsmallSub1, CapBinsmallSub1Red)

## Sub2 and large prey

CapBinlargeSub2 <- glmer(IndCapture ~ LogHunger + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub2" & Treatment == "large")), 
		family = binomial(logit))

CapBinlargeSub2Red <- glmer(IndCapture ~(1|IndBoxID) + (1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub2" & Treatment == "large")), 
		family = binomial(logit))	

anova(CapBinlargeSub2, CapBinlargeSub2Red)

## Sub2 and small prey

CapBinsmallSub2 <- glmer(IndCapture ~ LogHunger + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub2" & Treatment == "small")), 
		family = binomial(logit))

CapBinsmallSub2Red <- glmer(IndCapture ~(1|IndBoxID) + (1|IndBoxID:SpiderID), (subset(BoxComboMorn, Instar =="Sub2" & Treatment == "small")), 
		family = binomial(logit))	

anova(CapBinsmallSub2, CapBinsmallSub2Red)


######## Testing overall model

CapConFull.glmer <- glmer(IndCapture ~ LogCond+Instar+Treatment + LogCond*Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
summary(CapConFull.glmer)

##Checking LogCond:Treatment

CapConInter.glmer <- glmer(IndCapture ~ LogCond+Instar+Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
anova(CapConFull.glmer, CapConInter.glmer)

###Checking LogCond + Treatment
CapConCond.glmer <- glmer(IndCapture ~ Instar+Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
anova(CapConFull.glmer, CapConCond.glmer)

###Checking LogCond + Treatment
CapConTreat.glmer <- glmer(IndCapture ~ LogCond + Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
anova(CapConFull.glmer, CapConTreat.glmer)

#######Checking Instar
CapConInstar.glmer <- glmer(IndCapture ~ LogCond+ Treatment + LogCond*Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
anova(CapConFull.glmer, CapConInstar.glmer)



############# Testing prey size separately ###

CapSmallFull <- lmer(LogCond ~ Cap + Instar + (1|IndBoxID), subset(BoxAveCap, 
				Treatment == "small"), REML = FALSE   )

modelPlot(CapSmallFull)
summary(CapSmallFull)

CapSmallRed <- lmer(LogCond ~ Instar +  (1|IndBoxID), subset(BoxAveCap, 
				Treatment == "small"), REML = FALSE    )


anova(CapSmallFull, CapSmallRed)


############# LargePrey

CaplargeFull <- lmer(LogCond ~ Cap + Instar + (1|IndBoxID), subset(BoxAveCap, 
				Treatment == "large"), REML = FALSE   )

modelPlot(CaplargeFull)
summary(CaplargeFull)

CaplargeRed <- lmer(LogCond ~ Instar +  (1|IndBoxID), subset(BoxAveCap, 
				Treatment == "large"), REML = FALSE    )


anova(CaplargeFull, CaplargeRed)

