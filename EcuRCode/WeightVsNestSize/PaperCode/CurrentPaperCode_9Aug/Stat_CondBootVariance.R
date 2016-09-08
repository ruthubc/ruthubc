# TODO: Add comment
# 
# Author: user
###############################################################################

########## Outputting the data to python

mySpiders <- subset(spiders, !is.na(condResiduals))

mySpiders <- ddply(mySpiders, "Instar", transform, maxCond = max(condResiduals), minCond = min(condResiduals)) # calculating max and min

spidersBootAve <- ddply(mySpiders, .(NestID, Instar, CountFemales, logCtFm, minCond, maxCond, InstarNumber, InstarSex, type), summarise,
		N = length(condResiduals),
		mean = mean(condResiduals),
		sd_data = sd(condResiduals)
)


spidersBootAve <- subset(spidersBootAve, N == 2 & type == 'multiple')
nrow(spidersBootAve)

write.csv(spidersBootAve, "spidersAveN2.csv")

######## Stats



numbins <- 10000

addAmt <- (1/numbins)


condBootVar <- read.csv("RuthSync/BootStrapVarianceForCluster/combined5Sept_Num13Missing.csv")

##Checking normality
ggplot(condBootVar, aes(x = bootSD_cond)) + geom_histogram()

condBootVar$bootSD_cond_trans <- (condBootVar$bootSD_cond + addAmt)^ (1/3) # does not make it normal, but it does make it better.

nrow(condBootVar) - sum(is.finite(condBootVar$bootSD_cond_trans)) # checking the transformation worked for all numbers

ggplot(condBootVar, aes(x = bootSD_cond_trans)) + geom_histogram()

condBootVar$bootSD_cond_trans <- 1/(condBootVar$bootSD_cond + addAmt)
condBootVar$bootSD_cond_trans <- asin(sqrt(condBootVar$bootSD_cond+addAmt))
condBootVar$bootSD_cond_trans <- 1/(asin(sqrt(condBootVar$bootSD_cond+addAmt)))
condBootVar$bootSD_cond_trans <- log10(condBootVar$bootSD_cond + addAmt)

nrow(condBootVar) - sum(is.finite(condBootVar$bootSD_cond_trans)) # checking the transformation worked for all numbers

ggplot(condBootVar, aes(bootSD_cond_trans)) + geom_histogram(bins = 60)

ggplot(condBootVar, aes(bootSD_cond_trans, sd_data)) + geom_point()

ggplot(condBootVar, aes(N, bootSD_cond_trans)) + geom_point()


InstarGridGraph(subset(condBootVar, type = "multiple"), "bootSD_cond_trans", "condition variance", "n", "", "", "n")

model <- lmer(bootSD_cond_trans ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber +  (1|NestID), data = BootVar)
anova(model)

library('nlme')
model_nml <- lme(bootSD_cond_trans ~ logCtFm + InstarSex:InstarNumber+ InstarNumber:logCtFm + InstarSex:logCtFm + InstarSex:logCtFm:InstarNumber, 
		random = ~1|NestID, weights = ~I(1/N), data = condBootVar)
AIC(model_nml)
model_nml2 <- lme(bootSD_cond_trans ~ logCtFm + InstarNumber:InstarSex, 
		random = ~1|NestID, weights = ~I(1/N), data = condBootVar)
AIC(model_nml2)



model_nmlRed <- lme(bootSD_cond_trans ~ InstarSex:InstarNumber, random = ~1|NestID, weights = ~I(1/N), data = condBootVar)
AIC(model_nmlRed)

anova.lme(model_nml)
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





glmerMod <- glmer(bootSD_cond_trans ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber+ 
				InstarNumber:logCtFm + InstarSex:logCtFm +  InstarSex:InstarNumber:logCtFm + 
				(1|NestID), weights = N,  data = condBootVar, family = gaussian(link = "log"))
anova(glmerMod)
summary(glmerMod)
AIC(glmerMod)

library("blmeco") 

dispersion_glmer(glmerMod) # should be between 0.75 and 1.4

require(car)
# from http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

qqp(condBootVar$bootSD_cond_trans, "norm")

# lnorm means lognormal
qqp(condBootVar$bootSD_cond_trans, "lnorm")

# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I
# have shown below.

poisson <- fitdistr(condBootVar$bootSD_cond_trans, "Poisson")
qqp(condBootVar$bootSD_cond_trans, "pois", poisson$estimate)

nbinom <- fitdistr(condBootVar$bootSD_cond_trans, "Inverse Gaussian")
qqp(condBootVar$bootSD_cond_trans, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])


gamma <- fitdistr(condBootVar$bootSD_cond_trans, "gamma")
qqp(condBootVar$bootSD_cond_trans, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])



