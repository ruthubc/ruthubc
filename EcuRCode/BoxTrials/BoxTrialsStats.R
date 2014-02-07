# Statistical analysis of box trial data
# 
# Author: Ruth
###############################################################################
library (lme4)
library(lmerTest) # not sure what excatly this does
library(glmmADMB)
library(ICC)
library(reshape)
source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/BoxTrials/BoxTrialsData.R")

######### Overdisperson function from 'http://glmm.wikidot.com/faq'
 overdisp_fun <- function(model) {
	## number of variance parameters in 
	##   an n-by-n variance-covariance matrix
	vpars <- function(m) {
		nrow(m)*(nrow(m)+1)/2
	}
	model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
	rdf <- nrow(model.frame(model))-model.df
	rp <- residuals(model,type="pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
	dev <- deviance(model)
	myRatio <- dev/rdf	
	c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval, dev = dev, myRatio = myRatio)

}


#################################################################################
##### feeding vs prey capture

#I haven't included interactions but they are not significant, nor is treatment or instar.
#I made instar a random variable
BoxComboCap <- subset(BoxComboMorn, IndFeed != "NA") # removing NA lines as the bootrstrapping can't deal

CapMod1 <- glmer(IndCapture ~ IndFeed*Treatment*Instar + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

summary(CapMod1)
anova(CapMod1) 
qqnorm(resid(CapMod1), main = "main") # not great but not bad

##Removing all interaction terms as they are massively not significant

CapMod2 <- glmer(IndCapture ~ IndFeed + Treatment + Instar + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

summary(CapMod2)
anova(CapMod2) 
qqnorm(resid(CapMod2), main = "main") # not great but not bad

##Removing treatment as it is very not significat from summary!

CapMod3 <- glmer(IndCapture ~ IndFeed  + Instar + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

summary(CapMod3)
anova(CapMod3) 
qqnorm(resid(CapMod3), main = "main") # not great but not bad

##Removing instar as it is very not significat from summary so we are just left with IndFeed!

CapMod4 <- glmer(IndCapture ~ IndFeed+ (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

summary(CapMod4)
qqnorm(resid(CapMod4), main = "main") # not great but not bad

RedCapMod <- glmer(IndCapture ~ 1+ (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

overdisp_fun(CapMod4); overdisp_fun(RedCapMod)

anova(CapMod4, RedCapMod) # testing the full model against the reduced model


#################################################################
##Time eating vs Hunger
# Only include morning trials and might have to disregard boxes that did not eat for under 30 mins

#linear model
TimeHunMod1 <- lmer(TimeEatingLog1 ~ LogHunger*Treatment*Instar +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, REML = FALSE)

summary(TimeHunMod1)
anova(TimeHunMod1) 
qqnorm(resid(TimeHunMod1), main = "TimeHunMod1"); abline(0,1) # dips in the middle
overdisp_fun(TimeHunMod1) # it is massively over dispersed

# Glmer with untransformed data
TimeHunMod2 <- glmer(TotalTimeEating ~ Hunger*Treatment*Instar + 
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod2)
anova(TimeHunMod2) 
qqnorm(resid(TimeHunMod2), main = "TimeHunMod2") # qqplot looked worse that TimeModHum1
overdisp_fun(TimeHunMod2) # But is less overdispersed, althoguh sitll over dispersed

#GLMER with transformed data, won't work with log transformed Time Eating
TimeHunMod3 <- glmer(TotalTimeEating ~ LogHunger*Treatment*Instar +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod3)
anova(TimeHunMod3) 
qqnorm(resid(TimeHunMod3), main = "TimeHunMod3"); abline(0, 1)  # same as TimeHunMod2
overdisp_fun(TimeHunMod3) # same as TimeHunMod2

#Trying to correct for overdispersion
BoxComboMorn$obsID<-as.factor(1:nrow(BoxComboMorn))

TimeHunMod4 <- glmer(TotalTimeEating ~ LogHunger*Treatment*Instar +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID) + (1|obsID), 
		BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod4)
anova(TimeHunMod4) 
qqnorm(resid(TimeHunMod4), main = "TimeHunMod4"); abline(0, 1) # once the line is added in it doesn't lookgood
overdisp_fun(TimeHunMod4) # massively over dispersed!! SHIT


#####The problem is that I need a zero-inflated model. So I have installed glmmadmb

TimeEatSub <- subset(BoxComboMorn, select = c("TotalTimeEating", "LogHunger", "Treatment", "Instar", "IndBoxID",
				"SpiderID"))

TimeEatSub <- na.omit(TimeEatSub)

TimeHunMod5 <- glmmadmb(TotalTimeEating ~LogHunger+Treatment+Instar +
				(1|IndBoxID) + (1|IndBoxID:SpiderID), data = TimeEatSub,
		zeroInflation = TRUE, family = "poisson")

### this doesn't work either.. I get an error meassage. I think best just to leave it

TimeHunRedMod <- glmer(TotalTimeEating ~ Hunger + Treatment +  Instar  + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))



#############################################################################
#Time eating vs hunger given that they have fed .. not significant!! ZEROES REMOVED
##The zeros seem to matter in this case

BoxMornFed <- subset(BoxComboMorn, TotalTimeEating > 0)

BoxMornFed$RowID<-(seq_len(nrow(BoxMornFed)))

TimeNoZeroHunMod1 <- lmer(TimeEatingLog ~ LogHunger*Treatment*Instar +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed, REML = FALSE)

qqnorm(resid(TimenNoZeroHunMod1), main = "TimenNoZeroHunMod1"); abline(0, 1) # 
overdisp_fun(TimeNoZeroHunMod1)# might be just about OK! Faraway book says might be fine p169
summary(TimeNoZeroHunMod1)
anova(TimenNoZeroHunMod1)

##testing the full model against completely reduced model: everything taken out
TimeNoZeroHunRedMod <- lmer(TotalTimeEating ~  1+
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed)

qqnorm(resid(TimeNoZeroHunRedMod), main = "TimeNoZeroHunRedMod"); abline(0, 1) # strange
overdisp_fun(TimeNoZeroHunRedMod) #over disperesed

anova(GiveTimeHunRedMod, TimeNoZeroHunMod1 )

# removing all interactions as not significant
TimeNoZeroHunMod2 <- lmer(TimeEatingLog ~ LogHunger+Treatment+Instar  +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed, REML = FALSE)

qqnorm(resid(TimeNoZeroHunMod2), main = "TimeNoZeroHunMod2"); abline(0, 1) # same as model 1
overdisp_fun(TimeNoZeroHunMod2)# might be just about OK! Faraway book says might be fine p169
summary(TimeNoZeroHunMod2)
anova(TimeNoZeroHunMod2)

# removing all interactions as not significant
TimeNoZeroHunMod3 <- lmer(TimeEatingLog ~ Treatment+Instar  +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed, REML = FALSE)

qqnorm(resid(TimeNoZeroHunMod3), main = "TimeNoZeroHunMod3"); abline(0, 1) # same as model 1
overdisp_fun(TimeNoZeroHunMod3)# might be just about OK! Faraway book says might be fine p169
summary(TimeNoZeroHunMod3)
anova(TimeNoZeroHunMod3)


######################################################################################
# Eating at all (binary) vs everything else
## has everything in it model
#glmer(IndFeed ~ LogHunger + Treatment +  Instar + LogHunger:Treatment + LogHunger:Instar +
			#	Treatment:Instar + LogHunger:Treatment:Instar+  (1|Instar) + (1|Instar:IndBoxID) + 
			#	(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

EatBinMod1 <- glmer(IndFeed ~ LogHunger*Treatment*Instar + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

qqnorm(resid(EatBinMod1, main = "TimeHunMod4")) ; abline(0, 1)
overdisp_fun(EatBinMod1)
summary(EatBinMod1)
anova(EatBinMod1) 

#Removing 3-way interaction and treatment:Huger and instar:treatment
EatBinMod2 <- glmer(IndFeed ~ LogHunger + Treatment+ Instar + LogHunger:Treatment+ (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

qqnorm(resid(EatBinMod2, main = "EatBinMod2")) ; abline(0, 1)
overdisp_fun(EatBinMod2)
summary(EatBinMod2)
anova(EatBinMod2)

#The only thing not significant is instar, so I am removing it
EatBinMod3 <- glmer(IndFeed ~ LogHunger + Treatment + LogHunger:Treatment+ (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

qqnorm(resid(EatBinMod3, main = "EatBinMod3")) ; abline(0, 1)
overdisp_fun(EatBinMod3)
summary(EatBinMod3)
anova(EatBinMod3)

# testing intereaction effect with reduced model
EatBinRedModInt <- glmer(IndFeed ~ LogHunger + Treatment + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

qqnorm(resid(EatBinRedModInt, main = "EatBinMod3")) ; abline(0, 1)
overdisp_fun(EatBinRedModInt)

anova(EatBinRedModInt, EatBinMod3) #very significant interaction effect

# testing treatment with reduced model
EatBinRedModTreatment <- glmer(IndFeed ~ LogHunger  + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

qqnorm(resid(EatBinRedModTreatment, main = "EatBinRedModTreatment")) ; abline(0, 1)
overdisp_fun(EatBinRedModTreatment)

anova(EatBinRedModTreatment, EatBinMod3)

#testing hunger with reduced model
EatBinRedModHun <- glmer(IndFeed ~ Treatment  + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

qqnorm(resid(EatBinRedModHun, main = "EatBinRedModHun")) ; abline(0, 1)
overdisp_fun(EatBinRedModHun)

anova(EatBinRedModHun, EatBinMod3)

############################################################################
##Testing pj's against treatment

# linear model
SubAveByTrial <-subset(AveByTrial, TrialID != "T3")

# testing as simple linear model
PJMod1 <-  lmer(AsinPJEven ~ Treatment*Instar + (1|IndBoxID), SubAveByTrial)

qqnorm(resid(PJMod1, main = "PJMod1")) ; abline(0, 1) # looks ok until you add the abline
overdisp_fun(PJMod1) # underdispersed
summary(PJMod1)

## try binomial glmer but very very oversdispersed
PJMod2 <- glmer(PJEven ~ Treatment*Instar + (1|IndBoxID), SubAveByTrial, family = binomial)

qqnorm(resid(PJMod2, main = "PJMod2")) ; abline(0, 1) # 
overdisp_fun(PJMod2) # Very overdispersed


## I will try the zero inflated model
PJMod3 <- glmmadmb(PJEven ~ Treatment + Instar + (1|IndBoxID), SubAveByTrial, family = "binomial", zeroInflation = TRUE)

qqnorm(resid(PJMod3, main = "PJMod3")) ; abline(0, 1) #  looks good if i don't look at the abline
overdisp_fun(PJMod2) # Very very very overdispersed
summary(PJMod3)

### I'll stick to the linear model for the moment

#removing the interaction as not sigificant but then the model doesn't fit at all
PJMod4 <-  lmer(AsinPJEven ~ Treatment +Instar+ (1|IndBoxID), SubAveByTrial)

qqnorm(resid(PJMod4, main = "PJMod4")) ; abline(0, 1) # again not so good.
overdisp_fun(PJMod4) # VERY VERY OVERDISPERSED
summary(PJMod4)	

#### Testing against reduced model
PJRedModTreat <-  lmer(AsinPJEven ~ Instar+ (1|IndBoxID), SubAveByTrial)

qqnorm(resid(PJRedModTreat, main = "PJRedModTreat")) ; abline(0, 1) # again not so good.
overdisp_fun(PJRedModTreat) # underdispersed
summary(PJRedModTreat)	

anova(PJRedModTreat, PJMod4 )

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
	SimPJ <- unlist(simulate(PJRedMod))
	SimPJRedMod <- glmer(SimPJ ~ Instar + Treatment + (1|IndBoxID), AveByTrial, family = binomial(logit)) 
	SimPJMod <- glmer(SimPJ ~  Treatment*Instar + (1|IndBoxID), AveByTrial, family = binomial(logit))
	lrstat[i] <- as.numeric(2*(logLik(SimPJMod)-logLik(SimPJRedMod)))
}                                


number<-as.numeric(2*(logLik(PJMod) - logLik(PJRedMod)))


mean(lrstat > number)

#0.057 - 1000 iterations
#0.046 - 2000 iterations
#0.0533 - 3000 iterations


#### Individual fraction of time eating vs instar and prey
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


#### Is personality consistant over time? Intra class correlation

#Use weights table!

Boldness<-na.omit(subset(Weights, select = c("SpiderID", "BoldnessRank.1", "BoldnessRank.2")))
Boldness$BoldnessRank.1 <- as.numeric(Boldness$BoldnessRank.1)
Boldness$BoldnessRank.2 <- as.numeric(Boldness$BoldnessRank.2)


MeltBold<-melt(Boldness, id = c("SpiderID"))
MeltBold$SpiderID <- as.character(MeltBold$SpiderID)

ICCbare(SpiderID, value, data= MeltBold)

ICCest(SpiderID, value, data= MeltBold, alpha = 0.05)

##poke

Poke<-na.omit(subset(Weights, select = c("SpiderID", "Poke.1", "Poke.2")))
Poke$Poke.1 <- as.numeric(Poke$Poke.1)
Poke$Poke.2 <- as.numeric(Poke$Poke.2)


MeltPoke<-melt(Poke, id = c("SpiderID"))
MeltPoke$SpiderID <- as.character(MeltPoke$SpiderID)


ICCest(SpiderID, value, data= MeltPoke, alpha = 0.05)

##Testing ICC

ID <- c("ID1", "ID2", "ID3", "ID4", "ID5", "ID1", "ID2", "ID3", "ID4", "ID5")
Mes1 <- c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)

ICCTest <- data.frame(ID, Mes1)

ICCest(ID, Mes1, data= ICCTest, alpha = 0.05)
