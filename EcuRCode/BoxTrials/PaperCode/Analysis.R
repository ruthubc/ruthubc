# TODO: Add comment
# 
# Author: Ruth
###############################################################################


#### Box trails graphs. Code importing and manipulating the data is in BoxTrialsData.R
source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/BoxTrials/BoxTrialsData.R")

## Function calculating percentage difference on a aggregate table

PerDiff<-function(table){
	
	for(i in c(2,4)){
		
		diff<-table[i-1, 3] - table[i, 3]
		per<- ((table[i-1, 3] - table[i, 3])/ ((table[i-1, 3] + table[i, 3])/2)) *100 
		print(paste(table[i, 2], "diff:",  diff, ", %diff:", per))
		
	}
	
	
}


#######################################################################################
# Evenness vs prey size

PJMod4 <-  lmer(AsinPJEven ~ Treatment +Instar+ (1|IndBoxID), SubAveByTrial, REML = FALSE)
#mod4 has no interaction as interaction is very not significant
#Glmer very very overdispersed. Lmer Fits assumptions reasonably well

PJRedModTreat <-  lmer(AsinPJEven ~ Instar+ (1|IndBoxID), SubAveByTrial, REML = FALSE)

anova(PJMod4, PJRedModTreat)


#####################################################################################
## Condition vs feeding

# removing interactions as they either are insignificant or don't make sense
EatBinMod3 <- glmer(IndFeed ~ LogHunger + Treatment + Instar + Treatment:LogHunger +  (1|IndBoxID)+
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



############## Percentage difference in condition between feeders and non-feeders

EatConDiff<- aggregate(BoxComboMorn$Cond, by = list(BoxComboMorn$FeedIndPos, BoxComboMorn$Treatment), 
		FUN = mean, na.rm=TRUE)

PerDiff(EatConDiff)

################ Capture vs condition


CapConFull.glmer <- glmer(IndCapture ~ LogCond+Instar+Treatment + LogCond*Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
summary(CapConFull.glmer)

##Checking LogCond:Treatment

CapConInter.glmer <- glmer(IndCapture ~ LogCond+Instar+Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
anova(CapConFull.glmer, CapConInter.glmer)

###Checking LogCond
CapConCond.glmer <- glmer(IndCapture ~ Instar+Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
anova(CapConFull.glmer, CapConCond.glmer)

###Checking LogCond + Treatment
CapConTreat.glmer <- glmer(IndCapture ~ LogCond + Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))
anova(CapConFull.glmer, CapConTreat.glmer)

### Percentage difference

EatConDiff<- aggregate(BoxComboMorn$Cond, by = list(BoxComboMorn$CaptureIndPos, BoxComboMorn$Treatment), 
		FUN = mean, na.rm=TRUE)

PerDiff(EatConDiff)

### Just small prey 


