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

##Transforming the data
BoxComboMorn$TimeEatingLog <- log(BoxComboMorn$TotalTimeEating)
BoxComboMorn$TimeEatingLog1 <- log(BoxComboMorn$TotalTimeEating + 1)
BoxComboMorn$LogHunger<- log(BoxComboMorn$Hunger)

#linear modle
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





