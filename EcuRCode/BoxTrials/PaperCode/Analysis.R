# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(lme4)

#### Box trails graphs. Code importing and manipulating the data is in BoxTrialsData.R
source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/BoxTrials/BoxTrialsData.R")


## number of boxes per treatment
ddply(BoxComboMorn,. (Treatment),
		summarise,
		BoxCount = length(unique(IndBoxID)))

## number of boxes with prey capture
ddply(BoxComboMorn[BoxComboMorn$CaptureIndPos == 'y',],. (Treatment),
		summarise,
		BoxCount = length(unique(IndBoxID)))


### Means of intrabox evenness by prey size
ddply(AveByTrial,. (Treatment),
		summarise,
		mean = mean(AsinPJEven), SE = sd(AsinPJEven)/sqrt(length(AsinPJEven)))


#######################################################################################
# Evenness vs prey size

PJMod4 <-  lmer(AsinPJEven ~ Treatment +Instar+ (1|IndBoxID), SubAveByTrial, REML = FALSE)
#mod4 has no interaction as interaction is very not significant
#Glmer very very overdispersed. Lmer Fits assumptions reasonably well

PJRedModTreat <-  lmer(AsinPJEven ~ Instar+ (1|IndBoxID), SubAveByTrial, REML = FALSE)

anova(PJMod4, PJRedModTreat)


#####################################################################################
## Condition vs feeding

##### Means and se's 
ddply(BoxComboMorn,. (FeedIndPos),
		summarise,
		mean = mean(Cond.Scal, na.rm = TRUE), SE = sd(Cond.Scal, na.rm = TRUE)/sqrt(length(Cond.Scal)))

## Combining errors http://www.met.reading.ac.uk/~swrhgnrj/combining_errors.pdf
ddply(BoxComboMorn,. (Treatment, FeedIndPos),
		summarise,
		mean = mean(Cond.Scal, na.rm = TRUE), SE = sd(Cond.Scal, na.rm = TRUE)/sqrt(length(Cond.Scal)))

### Getting the difference in the mean condition between feeders and non-feeders and the standard error of that.

#### LARGE PREY
lenLFd <- length(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "y" & BoxComboMorn$Treatment == "large"])
lenLNonFd <- length(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "n" & BoxComboMorn$Treatment == "large"])

condLFdMn <-mean(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "y" & BoxComboMorn$Treatment == "large"], na.rm = TRUE)
condLNonFdMn <-mean(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "n" & BoxComboMorn$Treatment == "large"], na.rm = TRUE)

condLNonFdMn - condLFdMn # difference in mean condition for large prey

condLFdSD <-(sd(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "y" & BoxComboMorn$Treatment == "large"], na.rm = TRUE) ) / 
		sqrt(lenLFd)
condLNonFdSD <-sd(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "n" & BoxComboMorn$Treatment == "large"], na.rm = TRUE) /
		sqrt(lenLNonFd)

sqrt(condLFdSD^ 2 +  condLNonFdSD ^ 2)


#### Small PREY
lenSFd <- length(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "y" & BoxComboMorn$Treatment == "small"])
lenSNonFd <- length(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "n" & BoxComboMorn$Treatment == "small"])

condSFdMn <-mean(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "y" & BoxComboMorn$Treatment == "small"], na.rm = TRUE)
condSNonFdMn <-mean(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "n" & BoxComboMorn$Treatment == "small"], na.rm = TRUE)

condSNonFdMn - condSFdMn # difference in mean condition for small prey

condSFdSD <-(sd(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "y" & BoxComboMorn$Treatment == "small"], na.rm = TRUE) ) / 
		sqrt(lenSFd)
condSNonFdSD <-sd(BoxComboMorn$Cond.Scal [BoxComboMorn$FeedIndPos == "n" & BoxComboMorn$Treatment == "small"], na.rm = TRUE) /
		sqrt(lenSNonFd)

sqrt(condSFdSD^ 2 +  condSNonFdSD ^ 2)



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



###########################################
# Capture vs feeding

##numbers

BoxComboCap$FedWords<-ifelse(BoxComboCap$FeedIndPos == "y", "Fed", "Did Not Feed")
BoxComboCap$CapWords<-ifelse(BoxComboCap$CaptureIndPos == "y", "Cap", "Did Not Cap")

table(BoxComboMorn$IndCapture, BoxComboMorn$IndFeed)


### Stats Tests
CapFdGlmer <- glmer(IndCapture ~ IndFeed + Treatment + Instar + IndFeed:Treatment+  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

summary(CapFdGlmer)

CapFdGlmerInt <- glmer(IndCapture ~ IndFeed + Treatment + Instar +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

anova(CapFdGlmer, CapFdGlmerInt)


CapFdGlmerFd <- glmer(IndCapture ~ Treatment + Instar +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

anova(CapFdGlmer, CapFdGlmerFd)

################ Capture vs condition



############## Difference in mean scaled condition between capturers and non-capturers

#### LARGE PREY
lenLCp <- length(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "y" & BoxComboMorn$Treatment == "large"])
lenLNonCp <- length(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "n" & BoxComboMorn$Treatment == "large"])

condLCpMn <-mean(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "y" & BoxComboMorn$Treatment == "large"], na.rm = TRUE)
condLNonCpMn <-mean(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "n" & BoxComboMorn$Treatment == "large"], na.rm = TRUE)

condLNonCpMn - condLCpMn # difference in mean condition for large prey

condLCpSD <-(sd(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "y" & BoxComboMorn$Treatment == "large"], na.rm = TRUE) ) / 
		sqrt(lenLCp)
condLNonCpSD <-sd(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "n" & BoxComboMorn$Treatment == "large"], na.rm = TRUE) /
		sqrt(lenLNonCp)

sqrt(condLCpSD^ 2 +  condLNonCpSD ^ 2)


#### Small PREY
lenSCp <- length(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "y" & BoxComboMorn$Treatment == "small"])
lenSNonCp <- length(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "n" & BoxComboMorn$Treatment == "small"])

condSCpMn <-mean(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "y" & BoxComboMorn$Treatment == "small"], na.rm = TRUE)
condSNonCpMn <-mean(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "n" & BoxComboMorn$Treatment == "small"], na.rm = TRUE)

condSNonCpMn - condSCpMn # difference in mean condition for small prey

condSCpSD <-(sd(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "y" & BoxComboMorn$Treatment == "small"], na.rm = TRUE) ) / 
		sqrt(lenSCp)
condSNonCpSD <-sd(BoxComboMorn$Cond.Scal [BoxComboMorn$CaptureIndPos == "n" & BoxComboMorn$Treatment == "small"], na.rm = TRUE) /
		sqrt(lenSNonCp)

sqrt(condSCpSD^ 2 +  condSNonCpSD ^ 2)

############### Stats tests GLMER

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
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "small" & IndFeedNum == 1), family = binomial(logit))

anova(GlmFeedAndCapSm, GlmFeedAndCapRedSm )


## large prey


GlmFeedAndCapLg<- glmer(CapAndFeed ~  LogCond + Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "large"& IndFeedNum == 1), family = binomial(logit))

summary(GlmFeedAndCapLg)
visreg(GlmFeedAndCapLg)

GlmFeedAndCapRedLg<- glmer(CapAndFeed ~  Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "large" && IndFeedNum == 1), family = binomial(logit))

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
tapply(BoxComboMorn$SpiderID,BoxComboMorn$CapAndFeed, function(x) length(unique(x)))
