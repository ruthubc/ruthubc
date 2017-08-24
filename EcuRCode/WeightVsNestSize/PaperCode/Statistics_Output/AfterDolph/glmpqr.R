# TODO: Add comment
# 
# Author: Ruth
###############################################################################


condBootFormula <-  logCtFm + InstarNumber + InstarNumber:InstarSex
varBootCondMod <- glmmPQL(bootVarTrans ~ logCtFm + InstarNumber + InstarNumber:InstarSex, ~1|NestID, family = gaussian(link = "log"), 
		data = condBootVar, weights = lmrWgts, niter = 10)

summary(varBootCondMod)

varBootCondMod2 <- glmmPQL(bootVarTrans ~ InstarNumber + InstarNumber:InstarSex , ~1|NestID, family = gaussian(link = "log"), 
		data = condBootVar, weights = lmrWgts, niter = 10)

Anova(varBootCondMod, test.statistic=c("Wald", "LR"))

aov(varBootCondMod, test.statistic=c("Wald"))

anova(varBootCondMod)

wald.test(varBootCondMod, "logCtFm")

wald.test(b = coef(fm), Sigma = vcov(fm), Terms = 3:4)

library(aod)

library(survey)

regTermTest(varBootCondMod, "logCtFm")