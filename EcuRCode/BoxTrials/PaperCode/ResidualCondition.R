# Author: Ruth
###############################################################################
# Residual Index Condition

require(reshape2)
library (ggplot2)
library (grid)
library (lme4)
library(lmerTest)
library(visreg)

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv", na.strings = NA)
Feeding <-read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv", na.strings = NA)
Weights <-read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv", na.strings = NA)


Weights<-subset(Weights, select=c("IndBoxID", "SpiderID", "Instar", "Treatment", "Weight.1", "LegLen.mm", "HeadLen.mm"))


Weights$logLeg <- log10(Weights$LegLen.mm)
Weights$logWt <- log10(Weights$Weight.1)

Weights<- subset(Weights, !is.na(logWt)& !is.na(logLeg))

#calculating the residual index
#ggplot(Weights, aes(logLeg, logWt)) + geom_point() + geom_smooth(method=lm)# + facet_wrap(~Instar,  scales = "free")
#ggplot(spiders, aes(lnLeg, lnWt)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE) # log or nat log doesn't make a difference

model <- lm(logWt ~ logLeg, Weights ) # doesn't matter which way round this is
#model <- lm(logWt ~ logLeg + Instar + logLeg:Instar, spiders ) # whichever one i use doesn't make a difference

#visreg(model, xvar = "logLeg", by = "Instar" )


#summary(model)
Weights$condResiduals <- resid(model)  # putting the residuales into the dable


#ggplot(Weights, aes(condResiduals, fill = Instar)) + geom_histogram() # normal

# plotting res condition against leg length
#ggplot(Weights, aes(x = condResiduals, y = logLeg, colour = Instar)) + geom_point()

# plotting res condition against weight
#ggplot(Weights, aes(x = condResiduals, y = logWt, colour = Instar)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE)


### Putting together the tables

Feeding<-subset(Feeding, select=c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture"))
Trials<- subset(Trials, select = c("TrialID", "TimeOfDay", "BoxAtePrey", "BoxCapture"))

## Combining tables

BoxCombo <- merge(merge(Feeding, Weights, by = c("SpiderID")), Trials, by = c("TrialID"))

BoxCombo<-transform(BoxCombo, TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))

BoxCombo$BoxFeedObs <- as.factor(ifelse(BoxCombo$TotBoxEating > 30, "y", "n")) #change to 15mins?30mins?

BoxCombo$IndFeed <- as.factor(ifelse (BoxCombo$TotalTimeEating == "NA", NA, 
				ifelse(BoxCombo$TotalTimeEating > 0, "y", "n")))

BoxCombo$LogicalIndFeed <- ifelse(BoxCombo$IndFeed == "y", "1", ifelse(BoxCombo$IndFeed == "n", "0", NA))

Capture <- data.frame (IndCapture = c("y", "n", "n"), BoxCapture = c("y", "y", "n"), 
		CaptureIndPos = c("y", "n", NA))
BoxCombo <- merge(BoxCombo, Capture, by = (c("IndCapture", "BoxCapture")))


Feed <- data.table (IndFeed = c("y", "n", "n", NA), BoxFeedObs = c("y", "y", "n", "n"), 
		FeedIndPos = c("y", "n", NA, NA))
BoxCombo <- merge(BoxCombo, Feed, by= c("IndFeed", "BoxFeedObs"))

BoxCombEve <- subset(BoxCombo, TimeOfDay == "eve" & BoxCapture == "y") ## Why do so many eve have lots of tot time feeding obs?

BoxCombMorn<- subset(BoxCombo, TimeOfDay == "morn" | BoxCapture == "y") ## Why do so many eve have lots of tot time feeding obs?
# check including eve box captures is OK. might just be better to take it out so I don't rock the boat. In total only 2 spiders were observed capturing in the eve 
#and obvioulsy all small prey

###################### STATISTICS #######################
# food vs. capture

# removing interactions as they either are insignificant or don't make sense
EatBinMod3 <- glmer(LogicalIndFeed ~ condResiduals + Treatment + Instar + Treatment:condResiduals +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


# testing Treatment:Hunger interaction
EatBinRedModInt <- glmer(IndFeed ~ LogHunger + Treatment + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


anova(EatBinRedModInt, EatBinMod3) #very significant interaction effect

# testing treatment with reduced model
EatBinRedModTreatment <- glmer(IndFeed ~ LogHunger  + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


anova(EatBinRedModTreatment, EatBinMod3)

#testing hunger with reduced model
EatBinRedModHun <- glmer(IndFeed ~ Treatment  + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


## Testing just large prey

EatBinlargeFull <- glmer(IndFeed ~ LogCond + Instar+ (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "large"), family = binomial(logit))

EatBinlargeRed <- glmer(IndFeed ~ 1 + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "large"), family = binomial(logit))


anova(EatBinlargeFull, EatBinlargeRed)

## Testing just small prey

EatBinsmallFull <- glmer(IndFeed ~ LogCond + Instar+ (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "small"), family = binomial(logit))

EatBinsmallRed <- glmer(IndFeed ~ 1 + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "small"), family = binomial(logit))


anova(EatBinsmallFull, EatBinsmallRed)
