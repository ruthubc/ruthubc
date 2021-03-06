# TODO: Add comment
# 
# Author: user
###############################################################################
library(lme4)
library(lmerTest)
library(visreg)
library(ggplot2)

SpiAveMerge<- ddply(spidersMul, .(NestID, Instar, logCtFm), summarise, 
		N = length(!is.na(Weight.mg)),
		LogLegMean = mean(logLeg, na.rm = TRUE),
		condSqMean = mean(conditionSq, na.rm = TRUE)
)

spidersVar <- merge(spidersMul[,c("NestID", "Instar", "logLeg", "conditionSq")], SpiAveMerge, by = c("NestID", "Instar") )


spidersVar$legDiff <- (spidersVar$logLeg - spidersVar$LogLegMean) / spidersVar$LogLegMean # dividing by the mean
spidersVar$legDiff <- abs(spidersVar$legDiff)  # for some reason this wasn't working until I seperated it !
spidersVar$condDiff <- (spidersVar$conditionSq - spidersVar$condSqMean)/spidersVar$condSqMean # dividing by the mean
spidersVar$condDiff <- abs(spidersVar$condDiff)



# removing records that only have one spider, could change the min number of spiders needed

MinNumSpis <- 3

spidersVar$legDiff[spidersVar$N <= MinNumSpis] <- NA 

spidersVar$condDiff[spidersVar$N <= MinNumSpis] <- NA

xtabs(~ NestID + Instar , data = subset(spidersVar, legDiff != "NA"))
xtabs(~ NestID + Instar , data = subset(spidersVar, condDiff != "NA"))

#spidersVar <- subset(spidersVar, Instar !="SubMale")  # removing sub males as not enought to do variance tests
#spidersVar <- subset(spidersVar, Instar !="SubMale") 
spidersVar$Instar <- factor(spidersVar$Instar)


## histogram to check the fit
ggplot(spidersVar, aes(log(legDiff))) + geom_histogram(binwidth = 0.25)  # must be log transformed to be normal
ggplot(spidersVar, aes(condDiff^0.5)) + geom_histogram(binwidth = 0.02) # must be squareroot transformed



# transformations
spidersVar$logLegDiff <- log(spidersVar$legDiff)
spidersVar$sqCondDiff <- sqrt(spidersVar$condDiff)


##########################################################################3
## Stats Tests

## Leg variance

legDiffLm <- lmer(logLegDiff ~  logCtFm + I(logCtFm ^2)+ Instar+ Instar:logCtFm + I(logCtFm ^2):Instar +  
				(1|NestID), data = spidersVar, REML = FALSE)

anova(legDiffLm)
visreg(legDiffLm, xvar = "logCtFm", by = "Instar")
summary(legDiffLm)

# term with no square term is not significant.
legDiffLmNoSq <- lmer(logLegDiff ~  logCtFm +  Instar+ Instar:logCtFm  +  
				(1|NestID), data = spidersVar, REML = FALSE)

anova(legDiffLmNoSq)  ## Slight increase with nest size p = 0.091
visreg(legDiffLmNoSq, xvar = "logCtFm", by = "Instar")
summary(legDiffLmNoSq)

legDiffLmRed <- lmer(logLegDiff ~ logCtFm + Instar + (1|NestID), data = spidersVar, REML = FALSE)

anova(legDiffLmNoSq, legDiffLmRed)




## Org Condition variance

# when removing records that have 

condDiffLm <- lmer(sqCondDiff ~  logCtFm + I(logCtFm ^2)+ Instar + Instar:logCtFm + I(logCtFm ^2):Instar +  
				(1|NestID), data = spidersVar, weights = N, REML = FALSE)

anova(condDiffLm)
visreg(condDiffLm , xvar = "logCtFm", by = "Instar")

## no square term
condDiffLmNoSq <- lmer(sqCondDiff ~  logCtFm + Instar + Instar:logCtFm  +  
				(1|NestID), data = spidersVar, REML = FALSE)

anova(condDiffLmNoSq)
visreg(condDiffLmNoSq , xvar = "logCtFm", by = "Instar")
 
# I expect only the sub2s will be significant decrease in variance as colonies get larger

