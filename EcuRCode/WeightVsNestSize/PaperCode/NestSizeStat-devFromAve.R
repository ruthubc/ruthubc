# TODO: Add comment
# 
# Author: user
###############################################################################
library(lme4)
library(lmerTest)
library(visreg)

SpiAveMerge<- ddply(spidersMul, .(NestID, Instar), summarise, 
		N = length(!is.na(Weight.mg)),
		LogLegMean = mean(logLeg, na.rm = TRUE),
		condSqMean = mean(conditionSq, na.rm = TRUE),
		condCbSqMean = mean(condCbSq, na.rm = TRUE)
)

spidersVar <- merge(spidersMul, SpiAveMerge, by = c("NestID", "Instar") )


spidersVar$legDiff <- (spidersVar$logLeg - spidersVar$LogLegMean) / spidersVar$LogLegMean # dividing by the mean
spidersVar$legDiff <- abs(spidersVar$legDiff)  # for some reason this wasn't working until I seperated it !
spidersVar$condDiff <- (abs(spidersVar$conditionSq - spidersVar$condSqMean))/spidersVar$condSqMean # dividing by the mean
spidersVar$condCbDiff <- (abs(spidersVar$condCbSq- spidersVar$condCbSqMean))/spidersVar$condCbSqMean # dividing by the mean


# removing records that only have one spider, could change the min number of spiders needed
spidersVar$legDiff[spidersVar$N <= 3] <- NA 
spidersVar$condCbDiff[spidersVar$N <= 3] <- NA
spidersVar$condDiff[spidersVar$N <= 3] <- NA

xtabs(~ NestID + Instar , data = subset(spidersVar, legDiff != "NA"))

spidersVar <- subset(spidersVar, Instar !="SubMale")  # removing sub males as not enought to do variance tests
spidersVar$Instar <- factor(spidersVar$Instar)


## histogram to check the fit
ggplot(spidersVar, aes(log(legDiff))) + geom_histogram(binwidth = 0.25)  # must be log transformed to be normal
ggplot(spidersVar, aes(condDiff^0.5)) + geom_histogram(binwidth = 0.02) # must be squareroot transformed
ggplot(spidersVar, aes(condCbDiff^0.5)) + geom_histogram(binwidth = 0.01) # must be squareroot transformed


# transformations
spidersVar$logLegDiff <- log(spidersVar$legDiff)
spidersVar$sqCondDiff <- sqrt(spidersVar$condDiff)
spidersVar$sqCondCbDiff <- sqrt(spidersVar$condCbDiff)

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

anova(legDiffLmNoSq)
visreg(legDiffLmNoSq, xvar = "logCtFm", by = "Instar")
summary(legDiffLmNoSq)

legDiffLmRed <- lmer(logLegDiff ~ logCtFm + Instar + (1|NestID), data = spidersVar, REML = FALSE)

anova(legDiffLmNoSq, legDiffLmRed)

## Condition Cubed variance

# the model whether a femcount squre term is used or not is very insignificant.

condCbDiffLm <- lmer(sqCondCbDiff ~  logCtFm + I(logCtFm ^2)+ Instar + Instar:logCtFm + I(logCtFm ^2):Instar +  
				(1|NestID), data = spidersVar, REML = FALSE)

anova(condCbDiffLm )
visreg(condCbDiffLm , xvar = "logCtFm", by = "Instar")

condCbDiffLmNoSq <- lmer(sqCondCbDiff ~  logCtFm + Instar + Instar:logCtFm  +  
				(1|NestID), data = spidersVar, REML = FALSE)

visreg(condCbDiffLmNoSq, xvar ="logCtFm", by ="Instar")
anova(condCbDiffLmNoSq)


## Org Condition variance

# when removing records that have 

condDiffLm <- lmer(sqCondDiff ~  logCtFm + I(logCtFm ^2)+ Instar + Instar:logCtFm + I(logCtFm ^2):Instar +  
				(1|NestID), data = spidersVar, REML = FALSE)

anova(condDiffLm)
visreg(condDiffLm , xvar = "logCtFm", by = "Instar")

## no square term
condDiffLm <- lmer(sqCondDiff ~  logCtFm + Instar + Instar:logCtFm  +  
				(1|NestID), data = subset(spidersVar, Instar != "AdMale"), REML = FALSE)

anova(condDiffLm)
visreg(condDiffLm , xvar = "logCtFm", by = "Instar")
 
# I expect only the sub2s will be significant decrease in variance as colonies get larger

