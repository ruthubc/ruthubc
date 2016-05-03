# Author: Ruth
###############################################################################
# Residual Index Condition
# I did try this with leg length, but head length is so much better.
# Head means the capture interaction is significant, only just nonnsignifianct otherwise

require(reshape2)
library (ggplot2)
library (grid)
library (lme4)
library(lmerTest)
library(visreg)
library(data.table)

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv", na.strings = NA)
Feeding <-read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv", na.strings = NA)
Weights <-read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv", na.strings = NA)


Weights<-subset(Weights, select=c("IndBoxID", "SpiderID", "Instar", "Treatment", "Weight.1", "LegLen.mm", "HeadLen.mm"))


Weights$logHead <- log10(Weights$HeadLen.mm)
Weights$logWt <- log10(Weights$Weight.1)

Weights<- subset(Weights, !is.na(logWt)& !is.na(logHead))

#calculating the residual index
#ggplot(Weights, aes(logHead, logWt)) + geom_point() + geom_smooth(method=lm)# + facet_wrap(~Instar,  scales = "free")
#ggplot(spiders, aes(lnLeg, lnWt)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE) # log or nat log doesn't make a difference

model <- lm(logWt ~ logHead + Instar, Weights ) # doesn't matter which way round this is
# If instar is included in this model the interaction in prey capture is only just not significant. 
#model <- lm(logWt ~ logHead + Instar + logHead:Instar, spiders ) # whichever one i use doesn't make a difference

#visreg(model, xvar = "logHead", by = "Instar" )


#summary(model)
Weights$condResiduals <- resid(model)  # putting the residuales into the dable


ggplot(Weights, aes(condResiduals, fill = Instar)) + geom_histogram() # normal

# plotting res condition against leg length
#ggplot(Weights, aes(x = condResiduals, y = logHead, colour = Instar)) + geom_point()

# plotting res condition against weight
ggplot(Weights, aes(x = condResiduals, y = logWt, colour = Instar)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE)

### Is there a difference in residual condition between instars

InstarCond <- lm(condResiduals~ Instar, Weights )
anova(InstarCond)

### Putting together the tables

Feeding<-subset(Feeding, select=c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture"))
Trials<- subset(Trials, select = c("TrialID", "TimeOfDay", "BoxAtePrey", "BoxCapture"))

## Combining tables

BoxCombo <- merge(merge(Feeding, Weights, by = c("SpiderID")), Trials, by = c("TrialID"))

BoxCombo<-transform(BoxCombo, TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))

BoxCombo$BoxFeedObs <- as.factor(ifelse(BoxCombo$TotBoxEating > 30, "y", "n")) #change to 15mins?30mins?

BoxCombo$IndFeed<- as.factor(ifelse (BoxCombo$BoxFeedObs == "n", NA, 
				ifelse(BoxCombo$TotalTimeEating > 0, "y", "n")))


Capture <- data.frame (IndCapture = c("y", "n", "n"), BoxCapture = c("y", "y", "n"), 
		CaptureIndPos = c("y", "n", NA))

BoxCombo <- merge(BoxCombo, Capture, by = (c("IndCapture", "BoxCapture")))

BoxCombo$IndCapNum<- ifelse(BoxCombo$CaptureIndPos=="y", 1,
		ifelse(BoxCombo$CaptureIndPos =="n", 0, NA))

BoxCombo$IndFeedNum<- ifelse(BoxCombo$IndFeed=="y", 1,
		ifelse(BoxCombo$IndFeed=="n", 0, NA))



BoxComboEve <- subset(BoxCombo, TimeOfDay == "eve" & BoxCapture == "y") ## Why do so many eve have lots of tot time feeding obs?

BoxComboMorn<- subset(BoxCombo, TimeOfDay == "morn")


#Ind feed logistic plot
ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = condResiduals, y = IndFeedNum, linetype = Treatment)) + ylab("Individual fed?") +
		geom_point(aes(shape = Treatment),  size = 1.2, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black", size = 0.3) + xlab("Log condition scaled") + 
		scale_shape_manual(values = c(1, 17)) +	scale_linetype_manual(values = c(1, 5))  + 
		theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0))

#IndCap logistic plot
ggplot(subset(BoxComboMorn, CaptureIndPos != "NA"), aes(x = condResiduals, y = IndCapNum, linetype = Treatment)) + ylab("Individual fed?") +
		geom_point(aes(shape = Treatment),  size = 1.2, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black", size = 0.3) + xlab("Log condition scaled") + 
		scale_shape_manual(values = c(1, 17)) +	scale_linetype_manual(values = c(1, 5))  + 
		theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0))

###################### STATISTICS #######################
# eat vs condition

# full model
EatBinMod3 <- glmer(IndFeed ~ condResiduals + Treatment + Instar + Treatment:condResiduals +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, IndFeed != "NA"), family = binomial(logit))

summary(EatBinMod3)
anova(EatBinMod3)


# testing Treatment:condition interaction
EatBinRedModInt <- glmer(IndFeed ~ condResiduals + Treatment + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, IndFeed != "NA"), family = binomial(logit))


anova(EatBinRedModInt, EatBinMod3) #very significant interaction effect

# testing treatment with reduced model
EatBinRedModTreatment <- glmer(IndFeed ~ condResiduals  + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, IndFeed != "NA"), family = binomial(logit))


anova(EatBinRedModTreatment, EatBinMod3)

#testing hunger with reduced model
EatBinRedModCond <- glmer(IndFeed ~ Treatment  + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, IndFeed != "NA"), family = binomial(logit))

anova(EatBinRedModCond, EatBinMod3)




## Testing just large prey

EatBinlargeFull <- glmer(IndFeed ~ condResiduals + Instar+ (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "large"), family = binomial(logit))

EatBinlargeRed <- glmer(IndFeed ~ 1 + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "large"), family = binomial(logit))


anova(EatBinlargeFull, EatBinlargeRed)

## Testing just small prey

EatBinsmallFull <- glmer(IndFeed ~ condResiduals + Instar+ (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "small"), family = binomial(logit))

EatBinsmallRed <- glmer(IndFeed ~ 1 + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "small"), family = binomial(logit))


anova(EatBinsmallFull, EatBinsmallRed)

########## Individual capture

# full model
CapBinMod3 <- glmer(CaptureIndPos ~ condResiduals + Treatment + Instar + Treatment:condResiduals +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, CaptureIndPos != "NA"), family = binomial(logit))


anova(CapBinMod3)


# testing Treatment:condition interaction
CapBinRedModInt <- glmer(CaptureIndPos ~ condResiduals + Treatment + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, CaptureIndPos != "NA"), family = binomial(logit))


anova(CapBinRedModInt, CapBinMod3) #very significant interaction effect

# Testing treatment
CapBinRedModTreatment <- glmer(CaptureIndPos ~ condResiduals  + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, CaptureIndPos != "NA"), family = binomial(logit))


anova(CapBinRedModTreatment, CapBinMod3)

#testing hunger with reduced model
CapBinRedModCond <- glmer(CaptureIndPos ~ Treatment  + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, CaptureIndPos != "NA"), family = binomial(logit))

anova(CapBinRedModCond, CapBinMod3)




## Testing just large prey

CapBinlargeFull <- glmer(CaptureIndPos ~ condResiduals + Instar+ (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "large" & CaptureIndPos != "NA"), family = binomial(logit))

CapBinlargeRed <- glmer(CaptureIndPos ~ 1 + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "large" & CaptureIndPos != "NA"), family = binomial(logit))


anova(CapBinlargeFull, CapBinlargeRed)

## Testing just small prey

CapBinsmallFull <- glmer(CaptureIndPos ~ condResiduals + Instar+ (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "small" & CaptureIndPos != "NA"), family = binomial(logit))

CapBinsmallRed <- glmer(CaptureIndPos ~ 1 + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Treatment == "small" & CaptureIndPos != "NA"), family = binomial(logit))


anova(CapBinsmallFull, CapBinsmallRed)




