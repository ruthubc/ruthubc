# TODO: Add comment
# 
# Author: Ruth
###############################################################################


condBootFormula <-  logCtFm + InstarNumber + InstarNumber:InstarSex
varBootCondMod <- glmmPQL(bootVarTrans * 100 ~ logCtFm + InstarNumber + InstarNumber:InstarSex, ~1|NestID, family = gaussian(link = "log"), 
		data = condBootVar, weights = lmrWgts, niter = 10)

summary(varBootCondMod)

varBootCondMod2 <- glmmPQL(bootVarTrans ~ InstarNumber + InstarNumber:InstarSex , ~1|NestID, family = gaussian(link = "log"), 
		data = condBootVar, weights = lmrWgts, niter = 10)

Anova(varBootCondMod, test.statistic=c("Wald", "LR"))




waldTest <- wald.test(vcov(varBootCondMod),fixef(varBootCondMod), Terms = 2)


myPredict <- predict(varBootCondMod, condBootVar)
