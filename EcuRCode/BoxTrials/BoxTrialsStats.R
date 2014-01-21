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
######################################################3

#################################################################################
##### feeding vs prey capture

#I haven't included interactions in this but not sure there are significant.
## I think I need to transform some of the variables
##-------from histogram weight needs to be log transformed

		
CapModel = glmer(IndCapture ~ IndFeed + Instar + Treatment +  (Instar|LogWeight1)
				+ (1|IndBoxID) + (1|TrialID) + (1|SpiderID), data = BoxComboMorn, family = binomial(logit))

CapRedMod= glmer(IndCapture ~  IndFeed + Instar + Treatment + (1|IndBoxID) + (1|SpiderID),
		data = BoxComboMorn, family = binomial(logit) )

summary(CapModel) ## doesn't look like there are any effects for interaction but prob need to check
fixef(CapModel)
ranef(CapModel)
deviance(CapModel)
overdisp_fun(CapModel)
summary(CapRedMod) # don't know what this means!!
plot(CapRedMod) # not sure what these plots mean
qqnorm(fitted(CapModel))
ranef(CapModel)$IndBoxID #get individual predicted random effects

## testing full model against reduced model
anova(model, RedMod)


### testing the random effects.. not sure that I actually want to do this but the 
	# ... bootstrap methods seem useful
number<-as.numeric(2*(logLik(CapModel) - logLik(CapRedMod)))
pchisq(number, 5)
##bootstraping
test<- simulate(CapRedMod)

lrstat<- numeric(1000)
for (i in 1:1000){
	y<-unlist(simulate(CapRedMod))
	bnull <-
} # p160 to finish if necessary                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

ftable(xtabs())
###################################################################################
#### Time eating vs hunger







