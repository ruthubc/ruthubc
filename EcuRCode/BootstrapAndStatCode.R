# TODO: Add comment
# 
# Author: Ruth
###############################################################################


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
