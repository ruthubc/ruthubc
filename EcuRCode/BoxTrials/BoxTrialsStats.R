# Statistical analysis of box trial data
# 
# Author: Ruth
###############################################################################
library (lme4)
library(aods3)
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
	c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

#################################################################################
##### feeding vs prey capture

#I haven't included interactions but they are not significant, nor is treatment or instar.
#I made instar a random variable
BoxComboCap <- subset(BoxComboMorn, IndFeed != "NA") # removing NA lines as the bootrstrapping can't deal

CapMod <- glmer(IndCapture ~ IndFeed + (1|Instar) + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

RedCapMod <- glmer(IndCapture ~  (1|Instar)+ (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

anova(CapMod, RedCapMod) # testing the full model against the reduced model

###other ways to check the model
summary(CapMod) 
logLik(CapMod)
deviance(CapMod)
overdisp_fun(CapMod)


#################################################################
##Time eating vs Hunger
# Only include morning trials and might have to disregard boxes that did not eat for under 30 mins

#linear model
TimeHunMod <- lmer(TimeEatingLog1 ~ LogHunger + Treatment +  Instar + LogHunger:Treatment + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, REML = FALSE)


TimeHunMod <- glmer(TotalTimeEating ~ Hunger + Treatment +  Instar + LogHunger:Treatment + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))

anova(TimeHunMod)
summary(TimeHunMod)

TimeHunRedMod <- glmer(TotalTimeEating ~ Hunger + Treatment +  Instar  + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))

anova(TimeHunRedMod, TimeHunMod) #testing the full vs reduced model

ranef(TimeHunMod)$"Instar:LogHunger"

drop1(TimeHunMod, test = "F")  # not at all sure that this does anything useful!

#############################################################################
#Time eating vs hunger given that they have fed .. not significant!! The zeros seem to matter in this case

BoxMornFed <- subset(BoxComboMorn, TotalTimeEating > 0)


GiveTimeHunMod <- glmer(TotalTimeEating ~ Hunger + Treatment +  Instar + LogHunger:Treatment + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed, family = poisson(link = "log" ))

summary(GiveTimeHunMod)
anova(GiveTimeHunMod)

GiveTimeHunRedMod <- glmer(TotalTimeEating ~ Treatment +  Instar + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed, family = poisson(link = "log" ))

anova(GiveTimeHunRedMod, GiveTimeHunMod )


######################################################################################
# Eating at all (binary) vs everything else
## has everything in it model
#glmer(IndFeed ~ LogHunger + Treatment +  Instar + LogHunger:Treatment + LogHunger:Instar +
			#	Treatment:Instar + LogHunger:Treatment:Instar+  (1|Instar) + (1|Instar:IndBoxID) + 
			#	(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

EatBinMod <- glmer(IndFeed ~ LogHunger + Treatment  + LogHunger:Treatment  +
				   (1|Instar) + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

summary(EatBinMod)

EatBinRedMod <- glmer(IndFeed ~ Treatment   +
				LogHunger:Treatment +  (1|Instar) + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


anova(EatBinMod, EatBinRedMod)

############################################################################
##Testing pj's against treatment

# linear model 

PJMod <-  glmer(PJEven ~ Treatment + Instar + (1|IndBoxID),
		(AveByTrial) , family = binomial(logit))
summary(PJMod)
anova(PJMod)

PJRedMod <-  glm(PJEven ~ 1, 
		subset(AveByTrial, Instar == "Sub2"), family = quasibinomial(logit))
summary(PJRedMod)

anova( PJRedMod, PJMod, test = 'Chi')

overdisp_fun(PJMod)
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