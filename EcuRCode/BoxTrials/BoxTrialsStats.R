# Statistical analysis of box trial data
# 
# Author: Ruth
###############################################################################
library (lme4)
#library(lmerTest) not sure what excatly this does
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
TimeHunMod1 <- lmer(TimeEatingLog1 ~ LogHunger*Treatment*Instar + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, REML = FALSE)

summary(TimeHunMod1)
anova(TimeHunMod1) 
qqnorm(resid(TimeHunMod1), main = "TimeHunMod1") # dips in the middle
overdisp_fun(TimeHunMod1) # it is massively over dispersed

# Glmer with untransformed data
TimeHunMod2 <- glmer(TotalTimeEating ~ Hunger*Treatment*Instar + (1|Instar:Hunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod2)
anova(TimeHunMod2) 
qqnorm(resid(TimeHunMod2), main = "TimeHunMod2") # qqplot looked worse that TimeModHum1
overdisp_fun(TimeHunMod2) # But is less overdispersed, althoguh sitll over dispersed

#GLMER with transformed data, won't work with log transformed Time Eating
TimeHunMod3 <- glmer(TotalTimeEating ~ LogHunger*Treatment*Instar + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod3)
anova(TimeHunMod3) 
qqnorm(resid(TimeHunMod3), main = "TimeHunMod3") # same as TimeHunMod2
overdisp_fun(TimeHunMod3) # same as TimeHunMod2

#Trying to correct for overdispersion
BoxComboMorn$obsID<-as.factor(1:nrow(BoxComboMorn))

TimeHunMod4 <- glmer(TotalTimeEating ~ LogHunger*Treatment*Instar + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID) + (1|obsID), 
		BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod4)
anova(TimeHunMod4) 
qqnorm(resid(TimeHunMod4), main = "TimeHunMod4") # looks much better than TimeHunMod3
overdisp_fun(TimeHunMod4) # massively over dispersed!! SHIT




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

AveByTrial$resid<-as.factor(1:dim(AveByTrial)[1]) # http://tolstoy.newcastle.edu.au/R/e12/help/10/11/5740.html

AveByTrial$RowID<-factor(seq_len(nrow(AveByTrial)))

PJMod <-  glmer(log(PJEven+1) ~ Treatment*Instar + (1|IndBoxID),
		(AveByTrial), family = binomial)
summary(PJMod)
anova(PJMod, type = "marginal")
anova(PJMod, ddf = "Kenward-Roger")

PJRedMod <-  lmer(log(PJEven+1) ~ (1|IndBoxID), 
		AveByTrial)
summary(PJRedMod)$dispersion

anova( PJRedMod, PJMod)

overdisp_fun(PJMod) 

qqnorm(resid(PJMod), main = "main")

overdisp_fun(PJMod) 

deviance(PJMod)

plot(PJMod)

interaction.plot(AveByTrial$Treatment, AveByTrial$Instar,log(AveByTrial$PJEven+1))
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