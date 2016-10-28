# TODO: Add comment
# 
# Author: user
###############################################################################

######## Final


totSpis<- sum(condBootVar$N)
condBootVar$lmrWgts <- condBootVar$N/totSpis
condBootVar$bootVarTrans <- asin(sqrt(condBootVar$bootSD_var))
condBootVar$bootVarTrans <- condBootVar$bootVarTrans*100
condBootVar$bootVarTrans <- condBootVar$bootVarTrans +0.0000001

descdist(condBootVar$bootSD_var, discrete = FALSE)
descdist(condBootVar$bootVarTrans, discrete = FALSE)

glmerMod <- glmer(bootVarTrans ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber +   (1|NestID), family = gaussian(link = "log") ,
		data = condBootVar, weights = lmrWgts, nAGQ = 10)

anova(glmerMod)

summary(glmerMod)

glmerMod2 <- glmer((bootSD_cond_trans+1)*100 ~ InstarSex + InstarNumber + logCtFm +   (1|NestID), family = gaussian(link = "log") ,
		data = condBootVar, weights = lmrWgts)

plot(fitted(glmerMod), residuals(glmerMod), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(glmerMod), residuals(glmerMod)))


outcome    <- c("bootVarTrans")
predictors <- c("InstarNumber", "InstarNumber:InstarSex", "logCtFm:InstarNumber", "logCtFm:InstarNumber:InstarSex", 
		"I(InstarNumber^2)", "I(InstarNumber^2):InstarSex","logCtFm:I(InstarNumber^2)", "logCtFm:I(InstarNumber^2):InstarSex")

predictors <- c("InstarNumber", "InstarNumber:InstarSex", "logCtFm:InstarNumber", "logCtFm:InstarNumber:InstarSex")

modTable <- allModelsAICWithSex(outcome, predictors, condBootVar, weights = "n", nnLnr = "y")


update(as.formula(bootVarTrans ~ InstarSex + InstarNumber + logCtFm), .~. + (1|NestID))



########## Outputting the data to python  #

mySpiders <- subset(spiders, !is.na(condResiduals))

mySpiders <- ddply(mySpiders, "Instar", transform, maxCond = max(condResiduals), minCond = min(condResiduals)) # calculating max and min

spidersBootAve <- ddply(mySpiders, .(NestID, Instar, CountFemales, logCtFm, minCond, maxCond, InstarNumber, InstarSex, type), summarise,
		N = length(condResiduals),
		mean = mean(condResiduals),
		sd_data = sd(condResiduals)
)

spidersBootAveLeg <- ddply(mySpiders, .(NestID, Instar, CountFemales, logCtFm, minCond, maxCond, InstarNumber, InstarSex, type), summarise,
		N = length(logLeg),
		mean = mean(logLeg),
		sd_data = sd(logLeg)
)



spidersBootAve <- subset(spidersBootAve, N == 2 & type == 'multiple')
nrow(spidersBootAve)

write.csv(spidersBootAve, "spidersAveN2.csv")

######## Stats



numbins <- 10000

addAmt <- (1/numbins)

condBootVar <- read.csv("G:/Dropbox/RuthEcuador2013/NestSize/bootSampCondPython_cond_combined.csv")
condBootVar <- read.csv("C:/Work/Dropbox/RuthEcuador2013/NestSize/bootSampCondPython_cond_combined.csv")


dataset <- condBootVar
## myIndex is very normal, bootSD var is NOT
ggplot(data = dataset, aes(x = bootSD_var)) + geom_histogram()

condBootVar$bootVarTrans <- asin(sqrt(condBootVar$bootSD_var))
condBootVar$bootVarTrans <- condBootVar$bootVarTrans +1
condBootVar$bootVarTrans <- condBootVar$bootVarTrans*100






condBootVar$bootSD_cond_trans <- asin(sqrt(dataset$bootSD_var))

ggplot(data = condBootVar, aes(x = bootVarTrans)) + geom_histogram()


##Checking normality
ggplot(condBootVar, aes(x = bootSD_cond)) + geom_histogram()

condBootVar$bootSD_cond_trans <- asin(((condBootVar$bootSD_var + 0)^ (1/2)) ^(1/2)) # does not make it normal, but it does make it better.

nrow(condBootVar) - sum(is.finite(condBootVar$bootSD_cond_trans)) # checking the transformation worked for all numbers

ggplot(condBootVar, aes(x = bootSD_cond_trans)) + geom_histogram()

condBootVar$bootSD_cond_trans <- 1/(condBootVar$bootSD_var+1)
condBootVar$bootSD_cond_trans <- asin(sqrt(condBootVar$bootSD_var))
condBootVar$bootSD_cond_trans <- sqrt(condBootVar$bootSD_var+addAmt)
condBootVar$bootSD_cond_trans <- log(condBootVar$bootSD_var + 1)

nrow(condBootVar) - sum(is.finite(condBootVar$bootSD_cond_trans)) # checking the transformation worked for all numbers

ggplot(condBootVar, aes(bootSD_cond_trans)) + geom_histogram(bins = 60)
ggplot(condBootVar, aes(bootSD_cond_trans)) + geom_density()

ggplot(condBootVar, aes(bootSD_cond_trans, sd_data)) + geom_point()

ggplot(condBootVar, aes(N, bootSD_cond_trans)) + geom_point()


InstarGridGraph(condBootVar, "bootVarTrans", "condition variance", "n", "", "", "n")

model <- lmer(bootSD_cond_trans ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber +  (1|NestID), data = BootVar)
anova(model)

library('nlme')
model_nml <- lme(bootVarTrans ~ logCtFm + InstarSex:InstarNumber+ InstarNumber:logCtFm + InstarSex:logCtFm + InstarSex:logCtFm:InstarNumber, 
		random = ~1|NestID, weights = ~I(1/lmrWgts), data = condBootVar, method = "ML")

model_nml
plot(model_nml)

AIC(model_nml)
summary(model_nml)
anova(model_nml)

library(effects)
plot(allEffects(model_nml))

model_nmlRED  <- lme(bootVarTrans ~  InstarSex:InstarNumber + InstarSex:logCtFm:InstarNumber, 
		random = ~1|NestID, weights = ~I(1/lmrWgts), data = condBootVar, method = "ML")


anova(model_nml, model_nmlRED)

outcome    <- c("bootVarTrans")


predictors <- c("InstarNumber", "InstarNumber:InstarSex", "logCtFm:InstarNumber", "logCtFm:InstarNumber:InstarSex", 
		"I(InstarNumber^2)", "I(InstarNumber^2):InstarSex","logCtFm:I(InstarNumber^2)", "logCtFm:I(InstarNumber^2):InstarSex")


modTable <- allModelsAICWithSex(outcome, predictors, condBootVar, weights = "n", nnLnr = "y")




model_nml2 <- lme(bootVarTrans ~ logCtFm + InstarNumber:InstarSex, 
		random = ~1|NestID, weights = ~I(1/N), data = condBootVar)
AIC(model_nml2)

anova(model_nml, model_nml2)

model_nmlRed <- lme(bootSD_cond_trans ~ InstarSex:InstarNumber, random = ~1|NestID, weights = ~I(1/N), data = condBootVar)
AIC(model_nmlRed)

Anova.lme(model_nml)
summary(model_nml)$tTable

anova.lme(model_nmlRed)
summary(model_nmlRed)$tTable


anova(model_nml, model_nmlRed, method='ML')

AIC(model_nml)

model <- lmer(bootSD_cond_trans ~ InstarSex + InstarNumber + InstarSex:InstarNumber + logCtFm+  (1|NestID), data = condBootVar)

modRed <- lmer(bootSD_cond_trans ~ InstarSex + InstarNumber + InstarSex:InstarNumber +  (1|NestID), data = condBootVar)

anova(modRed, model)

modPred <- c("InstarSex:InstarNumber", "InstarSex:logCtFm", "logCtFm", "InstarNumber:logCtFm", "InstarNumber:logCtFm:InstarSex")

allModels <- allModelsAICWithSex("bootSD_cond_trans", modPred, BootVar)





glmerMod <- glmer(bootVarTrans+ 1 ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber+ 
				InstarNumber:logCtFm + InstarSex:logCtFm +  InstarSex:InstarNumber:logCtFm + 
				(1|NestID), data = condBootVar, family = Gamma)


anova(glmerMod)
summary(glmerMod)
AIC(glmerMod)

library("blmeco") 

dispersion_glmer(glmerMod) # should be between 0.75 and 1.4

require(car)
require(MASS)
# from http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

ggplot(condBootVar, aes(bootSD_cond_trans)) + geom_histogram(bins = 60)

qqp(condBootVar$bootSD_cond_trans, "norm")
qqp(condBootVar$bootSD_var, "norm")
# lnorm means lognormal
qqp(condBootVar$bootSD_cond_trans, "lnorm")
qqp(condBootVar$bootSD_var, "lnorm")

qqp(condBootVar$bootSD_cond_trans, "gamma")

# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I
# have shown below.

poisson <- fitdistr(condBootVar$bootSD_cond_trans, "Poisson")
qqp(condBootVar$bootSD_cond_trans, "pois", poisson$estimate)

nbinom <- fitdistr(condBootVar$bootSD_cond_trans +1, "Inverse Gaussian")
qqp(condBootVar$bootSD_cond_trans, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])


gamma <- fitdistr(condBootVar$bootSD_cond_trans+1, "gamma")
qqp(condBootVar$bootSD_cond_trans, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

gamma <- fitdistr(condBootVar$bootSD_var+1, "gamma")
qqp(condBootVar$bootSD_var, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])



mean(condBootVar$bootSD_var)*100

glmerMod <- glmer((bootSD_cond_trans+1) ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber +   (1|NestID), family = gaussian(link = "log") ,
		data = condBootVar, weights = lmrWgts)

glmerMod2 <- glmer((bootSD_cond_trans+1)*100 ~ InstarSex + InstarNumber + logCtFm +   (1|NestID), family = gaussian(link = "log") ,
		data = condBootVar, weights = lmrWgts)

plot(fitted(glmerMod), residuals(glmerMod), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(glmerMod), residuals(glmerMod)))

summary(glmerMod)
Anova(glmerMod)
anova(glmerMod, glmerMod2)
AIC(glmerMod2)

PQLMod <- glmmPQL((bootSD_cond_trans+1)*100 ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber, ~1|NestID, family = gaussian(link = "log") ,
				 data = condBootVar, weights = lmrWgts, niter = 10)
		 
PQLMod2 <- glmmPQL((bootSD_cond_trans+1)*100 ~ InstarSex + InstarNumber + InstarSex:InstarNumber, ~1|NestID, family = gaussian(link = "log") ,
				 data = condBootVar, weights = lmrWgts, niter = 10)
	
summary(PQLMod)$dispersion
dfbeta(PQLMod, intercept = TRUE)    	 
anova.lme(PQLMod)
Anova(PQLMod, type = c("II"), test.statistic = "Wald")
logLik(PQLMod)
df(PQLMod)

(-2*(logLik(PQLMod2)-logLik(PQLMod)))

PQLMod$dispersion

c_hat(PQLMod,  method = "pearson")

hat(PQLMod$varFix)
		 

library(AICcmodavg)
library(lmtest)
		 
summary(PQLMod)
Anova(PQLMod)
conif()
require(car)
require(MASS)
waldtest(PQLMod)
AIC(PQLMod)

library(bbmle)
#https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf

#dispersion parameter
#That quantity is the square root of the penalized residual sum of
#squares divided by n, the number of observations, evaluated as:

dfun <- function(object) {
	with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

n <- length(model$residuals)

return(  sqrt( sum(c(modelglmer@resid, modelglmer@u) ^2) / n ) )

dfun(PQLMod)

rp <- residuals(model, type = "pearson")
Pearson.chisq <- sum(rp^2)

model@u

qAIC(PQLMod, dispersion = -0.4)

library(aod)
summary(PQLMod)
wald.test(PQLMod)

overdisp_fun(PQLMod)

plot(fitted(PQLMod), residuals(PQLMod), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(PQLMod), residuals(PQLMod)))



outcome    <- c("bootVarTrans")
predictors <- c("InstarNumber", "InstarNumber:InstarSex", "logCtFm:InstarNumber", "logCtFm:InstarNumber:InstarSex", 
		"I(InstarNumber^2)", "I(InstarNumber^2):InstarSex","logCtFm:I(InstarNumber^2)", "logCtFm:I(InstarNumber^2):InstarSex")

modTable <- allModelsAICWithSex(outcome, predictors, dataset, weights = "n", nnLnr = "y")
