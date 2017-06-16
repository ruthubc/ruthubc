# TODO: Add comment
# 
# Author: Ruth
###############################################################################


condBootFormula <-  bootVarTrans ~ logCtFm + InstarNumber:InstarSex + InstarSex:I(InstarNumber^2)
varBootCondMod <- glmmPQL(bootVarTrans ~ logCtFm + InstarNumber:InstarSex + InstarSex:I(InstarNumber^2), ~1|NestID, family = gaussian(link = "log"), 
		data = condBootVar, weights = lmrWgts, niter = 10)

summary(varBootCondMod)

varBootCondMod2 <- glmmPQL(bootVarTrans ~ logCtFm + InstarNumber:InstarSex , ~1|NestID, family = gaussian(link = "log"), 
		data = condBootVar, weights = lmrWgts, niter = 10)

Anova(varBootCondMod)

aov(varBootCondMod, type=c("II","III"))

anova(varBootCondMod)

wald.test(varBootCondMod, "logCtFm")

wald.test(b = coef(fm), Sigma = vcov(fm), Terms = 3:4)

library(aod)

library(survey)

regTermTest(varBootCondMod, "logCtFm")