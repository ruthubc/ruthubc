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


BootVar <- read.csv("RuthSync/BootStrapVarianceForCluster/New28Aug/Output/combined.csv")

BootVar$bootSD_cond_trans <- 1/(BootVar$bootSD_cond + addAmt)
BootVar$bootSD_cond_trans <- asin(sqrt(BootVar$bootSD_cond+addAmt))
BootVar$bootSD_cond_trans <- 1/(asin(sqrt(BootVar$bootSD_cond+addAmt)))
BootVar$bootSD_cond_trans <- BootVar$bootSD_cond + addAmt

ggplot(BootVar, aes(bootSD_cond_trans)) + geom_histogram(bins = 60)

ggplot(BootVar, aes(bootSD_cond_trans, sd_data)) + geom_point()

ggplot(BootVar, aes(N, bootSD_cond_trans)) + geom_point()


InstarGridGraph(subset(BootVar, type = "multiple"), "bootSD_cond_trans", "condition variance", "n", "", "", "n")

model <- lmer(bootSD_cond_trans ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber +  (1|NestID), data = BootVar)
anova(model)
model_nml <- lme(bootSD_cond_trans ~ logCtFm + InstarSex:InstarNumber+ InstarNumber + InstarSex, random = ~1|NestID, weights = ~I(1/N), data = BootVar)

anova(model_nml)

AIC(model_nml)

modRed <- lmer(bootSD_cond_trans ~ InstarSex + InstarNumber + InstarSex:InstarNumber +  (1|NestID), data = BootVar)

anova(modRed, model)

modPred <- c("InstarSex:InstarNumber", "InstarSex:logCtFm", "logCtFm", "InstarNumber:logCtFm", "InstarNumber:logCtFm:InstarSex")

allModels <- allModelsAICWithSex("bootSD_cond_trans", modPred, BootVar)

glmerMod <- glmer((bootSD_cond_trans) ~ InstarSex + InstarNumber + logCtFm + InstarSex:InstarNumber +  
				(1|NestID), data = BootVar, family = gaussian(link = "log"))
anova(glmerMod)
summary(glmerMod)
AIC(glmerMod)

library("blmeco") 

dispersion_glmer(glmerMod) # should be between 0.75 and 1.4

require(car)
# from http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

qqp(BootVar$bootSD_cond_trans, "norm")

# lnorm means lognormal
qqp(BootVar$bootSD_cond_trans, "lnorm")

# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I
# have shown below.

poisson <- fitdistr(BootVar$bootSD_cond_trans, "Poisson")
qqp(BootVar$bootSD_cond_trans, "pois", poisson$estimate)

nbinom <- fitdistr(BootVar$bootSD_cond_trans, "Inverse Gaussian")
qqp(BootVar$bootSD_cond_trans, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])


gamma <- fitdistr(BootVar$bootSD_cond_trans, "gamma")
qqp(BootVar$bootSD_cond_trans, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])




