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

BoxComboCap <- subset(BoxComboMorn, IndFeed != "NA") # removing NA lines as the bootrstrapping can't deal

# orig model: CapMod <- glmer(IndCapture ~ IndFeed*Instar*Treatment + (1|Instar:IndBoxID) + 
				#(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

CapMod <- glmer(IndCapture ~ IndFeed + (1|Instar) + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

RedCapMod <- glmer(IndCapture ~ IndFeed + (1|Instar)+ (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

anova(CapMod, RedCapMod) # testing the full model against the reduced model

###other ways to check the model
summary(CapMod) 
logLik(CapMod)
deviance(CapMod)
overdisp_fun(CapMod)



### testing the random effects.. not sure that I actually want to do this but the 
	# ... bootstrap methods seem useful
number<-as.numeric(2*(logLik(CapModel) - logLik(CapRedMod)))
pchisq(number, 5)
##bootstraping Faraway p160 and 164


lrstat<- numeric(10)
for (i in 1:10){
	print(i)
	SimCap <- unlist(simulate(CapRedMod))
	SimCapRedMod <- glmer(SimCap ~  IndFeed + Instar + Treatment + (1|IndBoxID) + (1|SpiderID),
			data = BoxComboCap, family = binomial(logit), REML=FALSE) 
	SimCapMod <- glmer(SimCap ~  Instar + Treatment + (1|IndBoxID) + (1|SpiderID),
			data = BoxComboCap, family = binomial(logit), REML=FALSE)
	lrstat[i] <- as.numeric(2*(logLik(SimCapMod)-logLik(SimCapRedMod)))
}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           

2*(logLik(CapModel)-logLik(CapRedMod))

logLik(CapModel)
logLik(CapRedMod)

#lrstat[1]<-(2*(
logLik(SimCapMod)#-
logLik(SimCapRedMod)


##plotting the simulated results but not sure how useful this is with the bionomial
plot(qchisq((1:10)/11, 6), sort(lrstat), xlab = expression(chi[4]^2), ylab = "simulated LRT")
abline(0,1)

ftable(xtabs()) # I can't remember the point of the ftable but may come in handy later!
###################################################################################
#### Time eating vs hunger







