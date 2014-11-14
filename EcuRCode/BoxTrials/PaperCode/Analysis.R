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

### 
nlevels(BoxComboMorn$IndBoxID, BoxComboMorn$Treatment)
unique(BoxComboMorn$IndBoxID)

ddply(BoxComboMorn,. (Treatment),
		summarise,
		BoxCount = length(unique(IndBoxID)))


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

CapConCond.small <- glmer(IndCapture ~ Instar+LogCond + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, Treatment == 'small'), family = binomial(logit))

CapConCond.smallRed <- glmer(IndCapture ~ Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, Treatment == 'small'), family = binomial(logit))

anova(CapConCond.small, CapConCond.smallRed)


### Just large prey 

CapConCond.large <- glmer(IndCapture ~ Instar+LogCond + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, Treatment == 'large'), family = binomial(logit))

CapConCond.largeRed <- glmer(IndCapture ~ Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), subset(BoxComboMorn, Treatment == 'large'), family = binomial(logit))

anova(CapConCond.large, CapConCond.largeRed)


##### The removing non- participants , cheaters vs altrusits vs others.

table(BoxMornFeedOrCap$CapAndFeed, BoxMornFeedOrCap$Treatment)



## Small prey



GlmFeedAndCapSm<- glmer(CapAndFeed ~  LogCond + Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "small"), family = binomial(logit))

summary(GlmFeedAndCapSm)

GlmFeedAndCapRedSm<- glmer(CapAndFeed ~  Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "small"), family = binomial(logit))

anova(GlmFeedAndCapSm, GlmFeedAndCapRedSm )


## large prey


GlmFeedAndCapLg<- glmer(CapAndFeed ~  LogCond + Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "large"), family = binomial(logit))

summary(GlmFeedAndCap)

GlmFeedAndCapRedLg<- glmer(CapAndFeed ~  Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "large"), family = binomial(logit))

anova(GlmFeedAndCapLg, GlmFeedAndCapRedLg )

########################################################################
### Testing if there is a difference in the average number in each catorary "CapNoEat"   "NoCapEat"   "CapEat"     "NoCapNoEat"

FrCtsGlm <- glmer(value ~ variable + Instar + Treatment + variable:Treatment + (1|IndBoxID), FdCapByTrial, family = poisson )

summary(FrCtsGlm)
overdisp_fun(FrCtsGlm) # fine. Not overdispersed

FrCtsGlmInt <- glmer(value ~ variable + Instar + Treatment  + (1|IndBoxID), FdCapByTrial, family = poisson )

anova(FrCtsGlmInt, FrCtsGlm)

FrCtsGlmTreat <- glmer(value ~ variable + Instar  + (1|IndBoxID), FdCapByTrial, family = poisson )

anova(FrCtsGlmTreat, FrCtsGlm)
#Levels: "CapNoEat"   "NoCapEat"   "CapEat"     "NoCapNoEat"
### PostHoc- testing Cap And Eat 

FrCtsGlmCE <- glmer(value ~  Instar + Treatment + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "CapEat"), family = poisson )

summary(FrCtsGlmCE)
overdisp_fun(FrCtsGlmCE) # not over dispersed

FrCtsGlmCERed <- glmer(value ~  Instar  + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "CapEat"), family = poisson )

anova(FrCtsGlmCE,FrCtsGlmCERed) # not significant

# Testing NoCapEat
FrCtsGlmNCE <- glmer(value ~  Instar + Treatment + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "NoCapEat"), family = poisson )

summary(FrCtsGlmNCE)
overdisp_fun(FrCtsGlmNCE) # not overdispersed

FrCtsGlmNCERed <- glmer(value ~  Instar  + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "NoCapEat"), family = poisson )

anova(FrCtsGlmNCE,FrCtsGlmNCERed) 


# Testing NoCapNoEat
FrCtsGlmNCNE <- glmer(value ~  Instar + Treatment + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "NoCapNoEat"), family = poisson )

summary(FrCtsGlmNCNE)
overdisp_fun(FrCtsGlmNCNE) # not overdispersed

FrCtsGlmNCNERed <- glmer(value ~  Instar  + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "NoCapNoEat"), family = poisson )

anova(FrCtsGlmNCNE,FrCtsGlmNCNERed) # not significant


# Testing CapNoEat
FrCtsGlmCNE <- glmer(value ~  Instar + Treatment + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "CapNoEat"), family = poisson )

summary(FrCtsGlmCNE)
overdisp_fun(FrCtsGlmCNE) # not overdispersed

FrCtsGlmCNERed <- glmer(value ~  Instar  + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "CapNoEat"), family = poisson )

anova(FrCtsGlmCNE,FrCtsGlmCNERed) 


table(AveFdOrCap$NoCapNoEat, AveFdOrCap$Treatment)

#the number of spiders
table(BoxComboMorn$CapAndFeed)
# the number of boxes that had each of the four catorgies. 
tapply(BoxComboMorn$IndBoxID,BoxComboMorn$CapAndFeed, function(x) length(unique(x)))
