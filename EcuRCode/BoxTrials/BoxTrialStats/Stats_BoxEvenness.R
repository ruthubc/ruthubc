# TODO: Add comment
# 
# Author: Ruth
###############################################################################


############################################################################
##Testing pj's against treatment



# linear model
SubAveByTrial <-subset(AveByTrial, TrialID != "T3")

# mean differences between prey size
PJTrtMn<-by(SubAveByTrial$PJEven, SubAveByTrial$Treatment, mean)
PJTrtMn

##overall difference
PJTrtMn[1] -PJTrtMn[2]

#difference within instars
PJTrtInMn <- by(SubAveByTrial$PJEven, paste(SubAveByTrial$Instar, SubAveByTrial$Treatment), mean)
PJTrtInMn

PJTrtInMn[1]- PJTrtInMn[2]
PJTrtInMn[3]- PJTrtInMn[4]

#Difference within instars
PJInsMn<-by(SubAveByTrial$PJEven, SubAveByTrial$Instar, mean)
PJInsMn

PJInsMn[1] - PJInsMn[2]
# testing as simple linear model
PJMod1 <-  lmer(AsinPJEven ~ Treatment*Instar + (1|IndBoxID), SubAveByTrial,REML = FALSE)

modelPlot(PJMod1)
summary(PJMod1)
anova(PJMod1)

## try binomial glmer but very very oversdispersed
PJMod2 <- glmer(PJEven ~ Treatment*Instar + (1|IndBoxID), SubAveByTrial, family = binomial)


overdisp_fun(PJMod2) # Very overdispersed


## I will try the zero inflated model
PJMod3 <- glmmadmb(PJEven ~ Treatment + Instar + (1|IndBoxID), SubAveByTrial, family = "binomial", zeroInflation = TRUE)

qqnorm(resid(PJMod3, main = "PJMod3")) ; abline(0, 1) #  looks good if i don't look at the abline
overdisp_fun(PJMod2) # Very very very overdispersed
summary(PJMod3)


### I'll stick to the linear model for the moment

#removing the interaction as not sigificant but then the model doesn't fit at all
PJMod4 <-  lmer(AsinPJEven ~ Treatment +Instar+ (1|IndBoxID), SubAveByTrial, REML = FALSE)

modelPlot(PJMod4)
summary(PJMod4)	
anova(PJMod4)

anova(PJMod1, PJMod4)

#### Testing against reduced model
PJRedModTreat <-  lmer(AsinPJEven ~ Instar+ (1|IndBoxID), SubAveByTrial, REML = FALSE)

qqnorm(resid(PJRedModTreat, main = "PJRedModTreat")) ; abline(0, 1) # again not so good.
overdisp_fun(PJRedModTreat) # underdispersed
summary(PJRedModTreat)	

anova(PJRedModTreat, PJMod1)
anova(PJMod4, PJRedModTreat) #mod4 has no interaction as interaction is not significant

#### Testing against reduced model
PJRedModIns <-  lmer(AsinPJEven ~ Treatment + (1|IndBoxID), SubAveByTrial)

qqnorm(resid(PJRedModIns, main = "PJRedModTreat")) ; abline(0, 1) # again not so good.
overdisp_fun(PJRedModIns) # underdispersed
summary(PJRedModIns)	

anova(PJRedModIns, PJMod4 )

interaction.plot(AveByTrial$Treatment, AveByTrial$Instar,AveByTrial$AsinPJEven)


#subset(AveByTrial, Instar == "Sub2")
#Calculating overdispersion
rdev <- sum(residuals(PJMod,"pearson")^2)
mdf <- length(fixef(PJMod))
rdf <- nrow(AveByTrial)-mdf
rdev/rdf # =9.7

##Boot strappin

lrstat<- numeric(1000)
for (i in 1:1000){
	print(i)
	SimPJ <- unlist(simulate(PJRedModTreat))
	SimPJRedMod <- lmer(SimPJ ~ Instar+ (1|IndBoxID), SubAveByTrial, REML = FALSE)
	SimPJMod <- lmer(SimPJ ~Treatment+ Instar+ (1|IndBoxID), SubAveByTrial, REML = FALSE)
	lrstat[i] <- as.numeric(2*(logLik(SimPJMod)-logLik(SimPJRedMod)))
}                                



number<-as.numeric(2*(logLik(PJMod4) - logLik(PJRedModTreat)))


mean(lrstat > number)

#0.057 - 1000 iterations
#0.046 - 2000 iterations
#0.0533 - 3000 iterations

############## Testing just sub1 vs pj evenness 

PJModSub1 <-  lmer(AsinPJEven ~ Treatment + (1|IndBoxID), (subset(SubAveByTrial, Instar == "Sub1")), 
		REML = FALSE)

PJModSub1Red <-  lmer(AsinPJEven ~ (1|IndBoxID), (subset(SubAveByTrial, Instar == "Sub1")), 
		REML = FALSE)

anova(PJModSub1, PJModSub1Red)

############## Testing just sub2 vs pj evenness 

PJModSub2 <-  lmer(AsinPJEven ~ Treatment + (1|IndBoxID), (subset(SubAveByTrial, Instar == "Sub2")), 
		REML = FALSE)

PJModSub2Red <-  lmer(AsinPJEven ~ (1|IndBoxID), (subset(SubAveByTrial, Instar == "Sub2")), 
		REML = FALSE)

anova(PJModSub2, PJModSub2Red)

############################ Individual fraction of time eating vs instar and prey##################
## Starting with the linear model

SubBoxMorn <-subset(BoxComboMorn, FeedFraction>0 & TrialID != "T3")

# testing as simple linear model
FeedFracMod1 <-  lmer(ASFeedFrac ~ Treatment + Instar + (1|IndBoxID) + (1|IndBoxID:SpiderID), SubBoxMorn)
# interactions not significant so removing them
qqnorm(resid(FeedFracMod1, main = "FeedFracMod1")) ; abline(0, 1) # 
overdisp_fun(FeedFracMod1) # very underdispersed
summary(FeedFracMod1)

# testing against reduced model
FeedFracRedModTreat <-  lmer(ASFeedFrac ~ Instar + (1|IndBoxID) + (1|IndBoxID:SpiderID), SubBoxMorn)

qqnorm(resid(FeedFracRedModTreat, main = "FeedFracRedModTreat")) ; abline(0, 1) # 
overdisp_fun(FeedFracRedModTreat) # very underdispersed

anova(FeedFracMod1, FeedFracRedModTreat)

# testing against reduced model
FeedFracRedModIns <-  lmer(ASFeedFrac ~ Treatment + (1|IndBoxID) + (1|IndBoxID:SpiderID), SubBoxMorn)

qqnorm(resid(FeedFracRedModIns, main = "FeedFracRedModIns")) ; abline(0, 1) # 
overdisp_fun(FeedFracRedModIns) # very underdispersed

anova(FeedFracMod1, FeedFracRedModIns)
